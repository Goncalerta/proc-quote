use proc_macro_hack::proc_macro_hack;

// TODO documentation

#[proc_macro_hack]
pub use proc_quote_impl::quote;
#[proc_macro_hack]
pub use proc_quote_impl::quote_spanned;

pub use quote::ToTokens;
pub use quote::TokenStreamExt;

// Not public API.
#[doc(hidden)]
pub mod __rt {
    use super::*;
    pub use proc_macro2::*;

    pub fn append_ident(stream: &mut TokenStream, ident: &str, span: Span) {
        stream.append(Ident::new(ident, span));
    }
    pub fn append_punct(stream: &mut TokenStream, punct: char, spacing: Spacing, span: Span) {
        let mut punct = Punct::new(punct, spacing);
        punct.set_span(span);
        stream.append(punct);
    }
    pub fn append_lit(stream: &mut TokenStream, lit: Literal, span: Span) {
        let mut lit = lit;
        lit.set_span(span);
        stream.append(lit);
    }
    pub fn append_to_tokens<T: ToTokens>(stream: &mut TokenStream, to_tokens: &T) {
        to_tokens.to_tokens(stream);
    }
    pub fn append_group(
        stream: &mut TokenStream,
        inner: TokenStream,
        delimiter: Delimiter,
        span: Span,
    ) {
        let mut group = Group::new(delimiter, inner);
        group.set_span(span);
        stream.append(group);
    }
}
