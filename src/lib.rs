use proc_macro_hack::proc_macro_hack;

#[proc_macro_hack]
pub use proc_quote_impl::quote;

pub use quote::ToTokens;
pub use quote::TokenStreamExt;
