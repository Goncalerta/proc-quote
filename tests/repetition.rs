extern crate proc_macro2;
extern crate proc_quote;

use proc_macro2::{Ident, Span, TokenStream};
use proc_quote::{quote, TokenStreamExt};

struct X;

impl quote::ToTokens for X {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append(Ident::new("X", Span::call_site()));
    }
}

#[test]
fn test_repetition_simple() {
    let primes = &[X, X, X, X];

    assert_eq!("X X X X", quote!(#(#primes)*).to_string());

    assert_eq!("X , X , X , X ,", quote!(#(#primes,)*).to_string());

    assert_eq!("X , X , X , X", quote!(#(#primes),*).to_string());
}

#[test]
#[ignore] // TODO(#6)
fn test_repetition_no_vars() {
    unimplemented!("doesn't compile")
    // let tokens = quote!(#(a b)* #(c d),*);
    // assert_eq!("", tokens.to_string());
}

#[test]
fn test_repetition_two_vars() {
    let foo = vec!["a", "b"];
    let bar = vec![true, false];

    let tokens = quote! {
        #(#foo: #bar),*
    };

    let expected = r#""a" : true , "b" : false"#;
    assert_eq!(expected, tokens.to_string());
}

#[test]
fn test_repetition_nested() {
    let nested = vec![vec!['a', 'b', 'c'], vec!['x', 'y', 'z']];

    let tokens = quote! {
        #(
            #(#nested)*
        ),*
    };

    let expected = "'a' 'b' 'c' , 'x' 'y' 'z'";
    assert_eq!(expected, tokens.to_string());
}

#[test]
fn test_var_name_conflict() {
    // The implementation of `#(...),*` uses the variable `__i` but it should be
    // fine, if a little confusing when debugging.
    let __i = vec!['a', 'b'];
    let tokens = quote! { #(#__i),* };
    let expected = "'a' , 'b'";
    assert_eq!(expected, tokens.to_string());
}

#[test]
fn test_repetition_same_var_twice() {
    let a = vec![1, 2, 3];
    let b = quote!{ #(#a #a)* };
    assert_eq!("1i32 1i32 2i32 2i32 3i32 3i32", b.to_string());
}