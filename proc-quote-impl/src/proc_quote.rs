use proc_macro2::*;
use quote::{TokenStreamExt, quote};

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
// TODO parse Literal instead of depending on ToTokens (otherwise unsuffixed literals won't work properly)
fn parse_literal(stream: &mut TokenStream, lit: &Literal) {
    stream.append_all(quote! {
        ::proc_quote::__rt::append_to_tokens(&mut __stream, & #lit);
    });
}

/// Logic common to `parse_group` and `parse_group_in_iterator_pattern`.
fn parse_group_inner(stream: &mut TokenStream, inner: TokenStream, delimiter: Delimiter) {
    let delimiter = match delimiter {
        Delimiter::Parenthesis => quote!{
            ::proc_quote::__rt::Delimiter::Parenthesis
        },
        Delimiter::Brace => quote!{
            ::proc_quote::__rt::Delimiter::Brace
        },
        Delimiter::Bracket => quote!{
            ::proc_quote::__rt::Delimiter::Bracket
        },
        Delimiter::None => quote!{
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
fn parse_group_in_iterator_pattern(stream: &mut TokenStream, group: &Group, iter_idents: &mut Vec<Ident>) {
    let inner = parse_token_stream_in_iterator_pattern(group.stream(), iter_idents);
    let inner = generate_quote_header(inner);

    parse_group_inner(stream, inner, group.delimiter())
}

/// Returns true if `punct` is a pound sign and `next` is an Ident or a Group. 
/// 
/// This is used to check if this may be an interpolation pattern, so as to 
/// decide whether it is safe to call .next() on the input.
/// 
/// Not every pattern that returns true is actually an interpolation
/// pattern. The complete check is done in `interpolation_pattern_type`.
fn may_be_interpolation_pattern(punct: &Punct, next: Option<&TokenTree>) -> bool {
    punct.as_char() == '#' && match next {
        Some(TokenTree::Ident(_)) | Some(TokenTree::Group(_)) => true,
        _ => false,
    }
}

/// Returns true if the given token is an asterisk.
/// Used to check the pattern `#(...)*`.
fn is_asterisk(token: Option<&TokenTree>) -> bool {
    if let Some(TokenTree::Punct(punct)) = token {
        punct.as_char() == '*'
    } else {
        false
    }
}

/// Helper enum for `interpolation_pattern_type`'s return type.
enum InterpolationPattern<'a> {
    /// #ident
    Ident(&'a Ident),
    /// #{ ... }
    Expression(&'a Group),
    /// #( ... )*
    Iterator(&'a Group),
    /// Not an interpolation pattern
    None(&'a Group),
}

/// Helper type alias for `interpolation_pattern_type`'s input type.
type InputIter = std::iter::Peekable<token_stream::IntoIter>;

/// Returns the interpolation pattern type based on the content of `next` token
/// and the rest of the `input`.
/// 
/// It is assumed `may_be_interpolation_pattern` was called first, so the pattern
/// starts with '#' and `next` is either an `Ident` or a `Group`.
fn interpolation_pattern_type<'a>(next: &'a TokenTree, input: &mut InputIter) -> InterpolationPattern<'a> {
    match next {
        TokenTree::Ident(ident) => InterpolationPattern::Ident(ident),

        TokenTree::Group(group) if group.delimiter() == Delimiter::Brace => {
            InterpolationPattern::Expression(group)
        },

        TokenTree::Group(group) if group.delimiter() == Delimiter::Parenthesis => {
            // May be interpolation if followed by *
            if is_asterisk(input.peek()) {
                input.next(); // Drop * from the iterator
                InterpolationPattern::Iterator(group)
            } else {
                InterpolationPattern::None(group)
            }
        },

        TokenTree::Group(group) => {
            InterpolationPattern::None(group)
        },

        _ => panic!("`may_be_interpolation_pattern` should be called before."),
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
fn interpolate_to_tokens_group(stream: &mut TokenStream, group: &Group) {
    let inner = group.stream();
    stream.append_all(quote! {
        ::proc_quote::__rt::append_to_tokens(&mut __stream, & { #inner });
    });
}

/// Interpolates the expression inside the group, which should evaluate to 
/// something that implements `ToTokens`.
fn interpolate_iterator_group(stream: &mut TokenStream, group: &Group) {
    let mut iter_idents = Vec::new();

    let output = parse_token_stream_in_iterator_pattern(group.stream(), &mut iter_idents);

    let idents = iter_idents.iter();
    let idents_in_tuple = match iter_idents.len() {
        0 => panic!("TODO ERROR: Iterator pattern without iterators inside"),
        1 => quote!{ #(#idents)* },
        _ => quote!{ ( #( #idents ,)* ) },
    };

    let mut idents = iter_idents.iter();
    let first = idents.next().expect("TODO ERROR: Iterator pattern without iterators inside");
    let zip_iterators = quote! {
        #first .into_iter() #(.zip( #idents ))*
    };

    stream.append_all(quote! {
        for #idents_in_tuple in #zip_iterators {
            let _ = #output ;
        }
    });
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
                if may_be_interpolation_pattern(&punct, input.peek()) {
                    let next = input.next().expect("is Ident or Group");

                    match interpolation_pattern_type(&next, &mut input) {
                        InterpolationPattern::Ident(ident) => {
                            interpolate_to_tokens_ident(&mut output, ident)
                        },

                        InterpolationPattern::Expression(group) => {
                            interpolate_to_tokens_group(&mut output, group)
                        },

                        InterpolationPattern::Iterator(group) => {
                            interpolate_iterator_group(&mut output, group)
                        },

                        InterpolationPattern::None(group) => {
                            parse_punct(&mut output, punct);
                            parse_group(&mut output, group);
                        },
                    }
                } else {
                    parse_punct(&mut output, punct);
                }
            },
        }
    }

    output
}

/// Parses the input according to `quote!` rules inside an iterator pattern.
fn parse_token_stream_in_iterator_pattern(input: TokenStream, iter_idents: &mut Vec<Ident>) -> TokenStream {
    let mut output = TokenStream::new();

    let mut input = input.into_iter().peekable();
    while let Some(token) = input.next() {
        match &token {
            TokenTree::Group(group) => parse_group_in_iterator_pattern(&mut output, group, iter_idents),
            TokenTree::Ident(ident) => parse_ident(&mut output, ident),
            TokenTree::Literal(lit) => parse_literal(&mut output, lit),
            TokenTree::Punct(punct) => {
                if may_be_interpolation_pattern(&punct, input.peek()) {
                    let next = input.next().expect("is Ident or Group");

                    match interpolation_pattern_type(&next, &mut input) {
                        InterpolationPattern::Ident(ident) => {
                            interpolate_to_tokens_ident(&mut output, ident);
                            if !iter_idents.iter().any(|i| i == ident) {
                                iter_idents.push(ident.clone());
                            }
                        },

                        InterpolationPattern::Expression(_) => {
                            panic!("TODO ERROR: Pattern #{ ... } not supported in iterators");
                        },

                        InterpolationPattern::Iterator(_) => {
                            panic!("TODO ERROR: Nested iterator patterns not supported");
                        },

                        InterpolationPattern::None(group) => {
                            parse_punct(&mut output, punct);
                            parse_group_in_iterator_pattern(&mut output, group, iter_idents);
                        },
                    }
                } else {
                    parse_punct(&mut output, punct);
                }
            },
        }
    }

    output
}

pub fn quote(input: TokenStream) -> TokenStream {
    generate_quote_header(parse_token_stream(input))
}
