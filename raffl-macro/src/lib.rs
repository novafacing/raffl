//! Rust Asynchronous FFI Library
//!

use std::any::Any;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse::Nothing, parse_macro_input, ImplItemFn};

#[proc_macro_attribute]
/// Automatically create a companion C FFI function that calls the Rust function with the
/// arguments the C function received
pub fn callback_wrappers(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = parse_macro_input!(args as Nothing);
    let func = parse_macro_input!(input as ImplItemFn);

    let func_name = &func.sig.ident;
    let func_name_str = func_name.to_string();

    let callback_args = func.sig.inputs.iter().map(|arg| {
        let (arg_name, arg_ty) = match arg {
            syn::FnArg::Receiver(_) => panic!("Callbacks cannot have a receiver"),
            syn::FnArg::Typed(pat_type) => match &*pat_type.pat {
                syn::Pat::Ident(ident) => (&ident.ident, &pat_type.ty),
                _ => panic!("Callbacks cannot have destructuring arguments"),
            },
        };
        quote! {
            #arg_name: #arg_ty
        }
    });

    let func_args_conversions = func.sig.inputs.iter().map(|arg| {
        let arg_name = match arg {
            syn::FnArg::Receiver(_) => panic!("Callbacks cannot have a receiver"),
            syn::FnArg::Typed(pat_type) => match &*pat_type.pat {
                syn::Pat::Ident(ident) => &ident.ident,
                _ => panic!("Callbacks cannot have destructuring arguments"),
            },
        };
        quote! {
            let #arg_name = #arg_name.into();
        }
    });

    quote! {
        #func
    }
    .into()
}
