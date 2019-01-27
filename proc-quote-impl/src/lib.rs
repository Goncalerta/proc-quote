extern crate proc_macro;

use proc_macro_hack::proc_macro_hack;
use proc_macro::TokenStream;

mod proc_quote;

#[proc_macro_hack]
pub fn quote(input: TokenStream) -> TokenStream {
    proc_quote::quote(input.into()).into()
}

// TODO quote_spanned!