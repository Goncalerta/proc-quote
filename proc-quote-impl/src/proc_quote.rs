use proc_macro2::*;
use quote::{quote, TokenStreamExt};

/// Wraps the inner content inside a block with boilerplate to create and return `__stream`.
fn generate_quote_header(inner: TokenStream) -> TokenStream {
    quote! {
        {
            let mut __stream = ::proc_quote::__rt::TokenStream::new();
            #inner
            __stream
        }
    }
}

/// Transforms an `Ident` into code that appends the given `Ident` into `__stream`.
fn parse_ident(stream: &mut TokenStream, ident: &Ident) {
    let ident = ident.to_string();
    stream.append_all(quote! {
        ::proc_quote::__rt::append_ident(&mut __stream, #ident, ::proc_quote::__rt::Span::call_site());
    });
}

/// Transforms a `Punct` into code that appends the given `Punct` into `__stream`.
fn parse_punct(stream: &mut TokenStream, punct: &Punct) {
    let spacing = punct.spacing();
    let punct = punct.as_char();
    let append = match spacing {
        Spacing::Alone => quote! {
            ::proc_quote::__rt::append_punct(&mut __stream, #punct, ::proc_quote::__rt::Spacing::Alone);
        },
        Spacing::Joint => quote! {
            ::proc_quote::__rt::append_punct(&mut __stream, #punct, ::proc_quote::__rt::Spacing::Joint);
        },
    };
    stream.append_all(append);
}

/// Transforms a `Literal` into code that appends the given `Literal` into `__stream`.
fn parse_literal(stream: &mut TokenStream, lit: &Literal) {
    let lit_to_string = lit.to_string();

    if [
        "u8", "u16", "u32", "u64", "u128", "usize", "i8", "i16", "i32", "i64", "i128", "isize",
        "f32", "f64", "\"", "\'", "#",
    ]
    .iter()
    .any(|suffix| lit_to_string.ends_with(suffix))
    {
        // Number with a suffix, char, str, raw char, raw str
        // It should be safe to turn them into tokens
        stream.append_all(quote! {
            ::proc_quote::__rt::append_to_tokens(&mut __stream, & #lit);
        });
    } else {
        // Integer without suffix, float without suffix
        // Must be more careful, in order for the macro not to assume a wrong suffix
        if let Ok(i) = lit_to_string.parse::<i32>() {
            stream.append_all(quote! {
                ::proc_quote::__rt::append_lit(&mut __stream, Literal::i32_unsuffixed(#i));
            });
        } else if let Ok(i) = lit_to_string.parse::<i64>() {
            stream.append_all(quote! {
                ::proc_quote::__rt::append_lit(&mut __stream, Literal::i64_unsuffixed(#i));
            });
        } else if let Ok(u) = lit_to_string.parse::<u64>() {
            stream.append_all(quote! {
                ::proc_quote::__rt::append_lit(&mut __stream, Literal::u64_unsuffixed(#u));
            });
        } else if let Ok(f) = lit_to_string.parse::<f64>() {
            stream.append_all(quote! {
                ::proc_quote::__rt::append_lit(&mut __stream, Literal::f64_unsuffixed(#f));
            });
        } else {
            // This should never show up
            panic!("Unable to parse this literal. Please, fill in an issue in `proc-macro`'s repository.");
        }
    }
}

/// Logic common to `parse_group` and `parse_group_in_iterator_pattern`.
fn parse_group_inner(stream: &mut TokenStream, inner: TokenStream, delimiter: Delimiter) {
    let delimiter = match delimiter {
        Delimiter::Parenthesis => quote! {
            ::proc_quote::__rt::Delimiter::Parenthesis
        },
        Delimiter::Brace => quote! {
            ::proc_quote::__rt::Delimiter::Brace
        },
        Delimiter::Bracket => quote! {
            ::proc_quote::__rt::Delimiter::Bracket
        },
        Delimiter::None => quote! {
            ::proc_quote::__rt::Delimiter::None
        },
    };

    stream.append_all(quote! {
        ::proc_quote::__rt::append_group(&mut __stream, #inner, #delimiter);
    });
}

/// Transforms a `Group` into code that appends the given `Group` into `__stream`.
///
/// Inside iterator patterns, use `parse_group_in_iterator_pattern`.
fn parse_group(stream: &mut TokenStream, group: &Group) {
    let inner = parse_token_stream(group.stream());
    let inner = generate_quote_header(inner);

    parse_group_inner(stream, inner, group.delimiter())
}

/// Transforms a `Group` into code that appends the given `Group` into `__stream`.
///
/// This function is used inside the iterator patterns, to check for iterators used
/// inside.
fn parse_group_in_iterator_pattern(
    stream: &mut TokenStream,
    group: &Group,
    iter_idents: &mut Vec<Ident>,
) {
    let inner = parse_token_stream_in_iterator_pattern(group.stream(), iter_idents);
    let inner = generate_quote_header(inner);

    parse_group_inner(stream, inner, group.delimiter())
}

/// Helper enum for `interpolation_pattern_type`'s return type.
enum InterpolationPattern {
    /// #ident
    Ident(Ident),

    /// #( group ) token_stream *
    Iterator(Group, TokenStream),

    /// Not an interpolation pattern
    None,
}

/// Helper type alias for `interpolation_pattern_type`'s input type.
type InputIter = std::iter::Peekable<token_stream::IntoIter>;

/// Returns the interpolation pattern type based on the content of the given 
/// `punct` and the rest of the `input`.
/// 
/// Input that is part of the pattern is automatically consumed.
fn interpolation_pattern_type(
    punct: &Punct,
    input: &mut InputIter,
) -> InterpolationPattern {
    match (punct.as_char(), input.peek()) {
        // #ident
        ('#', Some(TokenTree::Ident(_))) => {
            if let Some(TokenTree::Ident(ident)) = input.next() {
                InterpolationPattern::Ident(ident)
            } else {
                panic!("guaranteed by previous match")
            }
        },

        // #(group)
        ('#', Some(TokenTree::Group(group))) if group.delimiter() == Delimiter::Parenthesis => {
            let inner = match input.next() {
                Some(TokenTree::Group(inner)) => inner,
                _ => panic!("guaranteed by previous match"),   
            };

            let separator = parse_separator(input);

            InterpolationPattern::Iterator(inner, separator)
        },

        // Not an interpolation pattern
        _ => InterpolationPattern::None,
    }
}

/// Interpolates the given variable, which should implement `ToTokens`.
fn interpolate_to_tokens_ident(stream: &mut TokenStream, ident: &Ident) {
    stream.append_all(quote! {
        ::proc_quote::__rt::append_to_tokens(&mut __stream, & #ident);
    });
}

/// Interpolates the expression inside the group, which should evaluate to
/// something that implements `ToTokens`.
fn interpolate_iterator_group(stream: &mut TokenStream, group: &Group, separator: &TokenStream) {
    let mut iter_idents = Vec::new();

    let output = parse_token_stream_in_iterator_pattern(group.stream(), &mut iter_idents);

    let mut idents = iter_idents.iter();
    let first = idents
        .next()
        .expect("TODO ERROR: Iterator pattern without iterators inside"); 
    let first = quote!{ #first };
    let idents_in_tuple = idents.fold(first, |previous, next| quote!{ (#previous, #next) });

    let mut idents = iter_idents.iter();
    let first = idents
        .next()
        .expect("TODO ERROR: Iterator pattern without iterators inside");
    let zip_iterators = quote! {
        #first .into_iter() #(.zip( #idents ))*
    };
    if separator.is_empty() {
        stream.append_all(quote! {
            for #idents_in_tuple in #zip_iterators {
                #output
            }
        });
    } else {
        stream.append_all(quote! {
            for (__i, #idents_in_tuple) in #zip_iterators .enumerate() {
                if __i > 0 {
                    #separator
                }
                #output
            }
        });
    }
}

/// Parses the input according to `quote!` rules.
fn parse_token_stream(input: TokenStream) -> TokenStream {
    let mut output = TokenStream::new();

    let mut input = input.into_iter().peekable();
    while let Some(token) = input.next() {
        match &token {
            TokenTree::Group(group) => parse_group(&mut output, group),
            TokenTree::Ident(ident) => parse_ident(&mut output, ident),
            TokenTree::Literal(lit) => parse_literal(&mut output, lit),
            TokenTree::Punct(punct) => {
                match interpolation_pattern_type(&punct, &mut input) {
                    InterpolationPattern::Ident(ident) => {
                        interpolate_to_tokens_ident(&mut output, &ident)
                    },
                    InterpolationPattern::Iterator(group, separator) => {
                        interpolate_iterator_group(&mut output, &group, &separator)
                    },
                    InterpolationPattern::None => {
                        parse_punct(&mut output, punct);
                    },
                }
            }
        }
    }

    output
}

/// Parses the input according to `quote!` rules inside an iterator pattern.
fn parse_token_stream_in_iterator_pattern(
    input: TokenStream,
    iter_idents: &mut Vec<Ident>,
) -> TokenStream {
    let mut output = TokenStream::new();

    let mut input = input.into_iter().peekable();
    while let Some(token) = input.next() {
        match &token {
            TokenTree::Group(group) => {
                parse_group_in_iterator_pattern(&mut output, group, iter_idents)
            }
            TokenTree::Ident(ident) => parse_ident(&mut output, ident),
            TokenTree::Literal(lit) => parse_literal(&mut output, lit),
            TokenTree::Punct(punct) => {
                match interpolation_pattern_type(&punct, &mut input) {
                    InterpolationPattern::Ident(ident) => {
                        interpolate_to_tokens_ident(&mut output, &ident);
                        if !iter_idents.iter().any(|i| i == &ident) {
                            iter_idents.push(ident);
                        }
                    },
                    InterpolationPattern::Iterator(_, _) => {
                        panic!("TODO ERROR: Nested iterator patterns not supported");
                    },
                    InterpolationPattern::None => {
                        parse_punct(&mut output, punct);
                    },
                }
            }
        }
    }

    output
}

/// Parses the input according to `quote!` rules in an iterator pattern, between 
/// the parenthesis and the asterisk.
fn parse_separator(input: &mut InputIter) -> TokenStream {
    let mut output = TokenStream::new();

    while let Some(token) = input.next() {
        match &token {
            TokenTree::Group(group) => parse_group(&mut output, group),
            TokenTree::Ident(ident) => parse_ident(&mut output, ident),
            TokenTree::Literal(lit) => parse_literal(&mut output, lit),
            TokenTree::Punct(punct) => {
                if punct.as_char() == '*' {
                    // The asterisk marks the end of the iterator pattern
                    return output;
                } else {
                    match interpolation_pattern_type(&punct, input) {
                        InterpolationPattern::Ident(ident) => {
                            // TODO don't allow iterator variables
                            interpolate_to_tokens_ident(&mut output, &ident)
                        },
                        InterpolationPattern::Iterator(_, _) => {
                            panic!("TODO ERROR: Nested iterator patterns not supported");
                        },
                        InterpolationPattern::None => {
                            parse_punct(&mut output, punct);
                        },
                    }
                }
            }
        }
    }

    panic!("TODO ERROR: EOF before asterisk")
}

pub fn quote(input: TokenStream) -> TokenStream {
    generate_quote_header(parse_token_stream(input))
}
