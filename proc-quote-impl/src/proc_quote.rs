use proc_macro2::*;
use quote::{ToTokens, TokenStreamExt, quote};

/// Consumes a `TokenStream`, producing a new `TokensStream` with the contents of
/// the first wrapped between the given delimiter.
fn wrap_in_group(tokens: TokenStream, delimiter: Delimiter) -> TokenStream {
    let mut stream = TokenStream::new();
    let token: Vec<TokenTree> = vec![Group::new(delimiter, tokens).into()];
    stream.extend(token);
    stream
}

/// Transforms an `Ident` into code that appends the given `Ident` into `__stream`.
fn parse_ident(stream: &mut TokenStream, ident: Ident) {
    let ident = ident.to_string();
    stream.append_all(quote! {
        ::proc_quote::__rt::append_ident(&mut __stream, #ident, ::proc_quote::__rt::Span::call_site());
    });
}

/// Transforms a `Punct` into code that appends the given `Punct` into `__stream`.
fn parse_punct(stream: &mut TokenStream, punct: Punct) {
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
fn parse_literal(stream: &mut TokenStream, lit: Literal) {
    stream.append_all(quote! {
        ::proc_quote::__rt::append_to_tokens(&mut __stream, #lit);
    });
}

/// Transforms a `Group` into code that appends the given `Group` into `__stream`.
fn parse_group(stream: &mut TokenStream, group: Group) {
    let inner = group.stream();
    let delimiter = group.delimiter();

    let inner = quote(inner);
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

pub fn quote(input: TokenStream) -> TokenStream {
    let mut output = TokenStream::new();
    output.append_all(quote! {
        let mut __stream = ::proc_quote::__rt::TokenStream::new();
    });

    let mut input = input.into_iter().peekable();
    while let Some(token) = input.next() {
        match token {
            TokenTree::Group(group) => parse_group(&mut output, group),
            TokenTree::Ident(ident) => parse_ident(&mut output, ident),
            TokenTree::Literal(lit) => parse_literal(&mut output, lit),
            TokenTree::Punct(punct) => parse_punct(&mut output, punct),
        }
    }

    output.append(Ident::new("__stream", Span::call_site()));
    let output = wrap_in_group(output, Delimiter::Brace);
    output 
}
