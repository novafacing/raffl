//! Rust Asynchronous FFI Library
//!

use proc_macro::TokenStream;
use proc_macro_error::{abort, proc_macro_error};
use quote::{format_ident, quote};
use std::hash::Hash;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    FnArg, Ident, ItemImpl, ReturnType, Token, Type,
};

/// A type or '...'
enum CallbackWrapperArgParam {
    Arg(FnArg),
    BangArg(FnArg),
    Ellipsis,
}

impl Parse for CallbackWrapperArgParam {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![!]) {
            input.parse::<Token![!]>()?;
            let typ = input.parse::<FnArg>()?;
            Ok(Self::BangArg(typ))
        } else if input.peek(Token![...]) {
            input.parse::<Token![...]>()?;
            Ok(Self::Ellipsis)
        } else {
            let typ = input.parse::<FnArg>()?;
            Ok(Self::Arg(typ))
        }
    }
}

struct CallbackWrapperArgParams {
    params: Vec<CallbackWrapperArgParam>,
}

impl Parse for CallbackWrapperArgParams {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let parsed = Punctuated::<CallbackWrapperArgParam, Token![,]>::parse_terminated(input)?;

        let params: Vec<CallbackWrapperArgParam> = parsed.into_iter().collect();

        Ok(Self { params })
    }
}

#[derive(Hash, Eq, PartialEq)]
enum CallbackWrapperArgType {
    /// The argument index of the callback function that is a pointer to the instance the callback
    /// function is called on. This pointer must be convertible to a `&self` or `&mut self` reference,
    /// depending on the callback function's receiver parameter mutability.
    Pub,
    /// Whether result types should be unwrapped by the wrapper function (to allow the callback
    /// function to return `Result<T, E>` instead of `T`).
    UnwrapResult,
}

impl Parse for CallbackWrapperArgType {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if let Ok(_pub_literal) = input.parse::<Token![pub]>() {
            Ok(Self::Pub)
        } else if let Ok(ident) = input.parse::<Ident>() {
            match ident.to_string().as_str() {
                "unwrap_result" => Ok(Self::UnwrapResult),
                _ => abort!(ident.span(), "Unknown callback wrapper argument type"),
            }
        } else {
            abort!(input.span(), "Expected 'pub'");
        }
    }
}

enum CallbackWrapperArgValue {
    None,
}

struct CallbackWrapperArg {
    typ: CallbackWrapperArgType,
    value: CallbackWrapperArgValue,
}

impl Parse for CallbackWrapperArg {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let typ = input.parse::<CallbackWrapperArgType>()?;
        let value = match typ {
            CallbackWrapperArgType::Pub | CallbackWrapperArgType::UnwrapResult => {
                CallbackWrapperArgValue::None
            }
        };
        Ok(Self { typ, value })
    }
}

struct CallbackWrapperArgs {
    args: Vec<CallbackWrapperArg>,
}

impl Parse for CallbackWrapperArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let parsed = Punctuated::<CallbackWrapperArg, Token![,]>::parse_terminated(input)?;

        let args: Vec<CallbackWrapperArg> = parsed.into_iter().collect();

        Ok(Self { args })
    }
}

impl CallbackWrapperArgs {
    pub fn is_pub(&self) -> bool {
        self.args
            .iter()
            .any(|arg| matches!(arg.typ, CallbackWrapperArgType::Pub))
    }
}

#[proc_macro_attribute]
pub fn params(_args: TokenStream, input: TokenStream) -> TokenStream {
    input
}

#[proc_macro_error]
#[proc_macro_attribute]
/// Create a companion C FFI function that calls the Rust method.
///
/// # Syntax
///
/// Below is an example of the syntax for this macro. The `callback_wrappers` macro must be
/// applied to an `impl` block. The `params` macro must be applied to *all* methods in that
/// `impl` block. Parameters specified in the `params` must be in the order the C FFI callback
/// function expects to receive them from the C code. The parameter that is a pointer or some
/// other value that can be converted to a `&self` or `&mut self` reference must be prefixed
/// with a `!`, and it must have a `From` implementation from the type of the `self` parameter in
/// the FFI callback function to the type of the `self` parameter in the Rust method (e.g. either
/// `&self` or `&mut self`).
///
/// ```ignore
/// #[callback_wrappers(<visibility>)]
///
/// impl <type> {
///    #[params(<arg0>, <arg1> ..., !<self>)]
///     pub fn <method_name>(&self, <arg0>, <arg1> ...) -> <return_type> {
///     }
/// }
/// ```
///
///
/// # Example
///
/// This will generate an extern "C" function `test_callbacks::test` that calls the Rust method
/// `Test::test`, with the first argument being a pointer to the instance of `Test`.
///
/// ```text
/// use raffl_macro::{callback_wrappers, params};
///
/// struct TestStruct {}
///
/// #[callback_wrappers(pub)]
/// impl TestStruct {
///    #[params(!slf: *mut std::ffi::c_void, ...)]
///   pub fn test(&self, a: i32, b: i32) -> i32 {
///        a + b
///   }
/// }
/// ```
pub fn callback_wrappers(args: TokenStream, input: TokenStream) -> TokenStream {
    let impl_args = parse_macro_input!(args as CallbackWrapperArgs);
    let visibility = if impl_args.is_pub() {
        quote! { pub }
    } else {
        quote! {}
    };
    let implementation = parse_macro_input!(input as ItemImpl);

    let struct_name = implementation.self_ty.clone();

    let struct_name_string = quote! { #struct_name }.to_string().to_ascii_lowercase();

    let impl_fns = implementation
        .items
        .iter()
        .filter_map(|item| {
            if let syn::ImplItem::Fn(method) = item {
                Some(method)
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    let callbacks = impl_fns
        .iter()
        .map(|f| {
            let attrs = &f.attrs;
            let args: CallbackWrapperArgParams =
                if let Some(args) = attrs.iter().find(|a| a.path().is_ident("params")) {
                    match args.parse_args() {
                        Ok(parsed) => parsed,
                        Err(e) => {
                            abort! {
                                args,
                                "expected `#[params(...)]`: {}", e
                            }
                        }
                    }
                } else {
                    abort! {
                        f,
                        "expected `#[params(...)]`"
                    };
                };
            let fname = &f.sig.ident;

            // Check if frty is a Result
            let is_result = if let ReturnType::Type(_, ty) = &f.sig.output {
                if let Type::Path(tp) = ty.as_ref() {
                    if let Some(segment) = tp.path.segments.last() {
                        segment.ident == "Result"
                    } else {
                        false
                    }
                } else {
                    false
                }
            } else {
                false
            };

            let frty = if is_result {
                quote! {}
            } else {
                let rty = &f.sig.output;
                quote! { #rty  }
            };

            let receiver = &f.sig.receiver().unwrap();

            // Get the args without the receiver, this will be dropped in for 'Ellipsis'
            let fargs = &f
                .sig
                .inputs
                .iter()
                .filter(|fa| !matches!(fa, syn::FnArg::Receiver(_)))
                .cloned()
                .collect::<Vec<_>>();

            let mut cb_args = Vec::new();
            let mut cb_receiver_arg_offset = None;
            let mut receiver_arg = None;

            for param in args.params {
                match param {
                    CallbackWrapperArgParam::BangArg(arg) => {
                        receiver_arg = Some(arg.clone());

                        cb_args.push(arg);
                        cb_receiver_arg_offset = Some(cb_args.len() - 1);
                    }
                    CallbackWrapperArgParam::Arg(arg) => cb_args.push(arg),
                    CallbackWrapperArgParam::Ellipsis => {
                        for arg in fargs {
                            cb_args.push(arg.clone());
                        }
                    }
                }
            }

            let receive = match receiver_arg.clone() {
                Some(arg) => {
                    let ident = match arg {
                        FnArg::Typed(pat) => match *pat.pat {
                            syn::Pat::Ident(ref ident) => ident.ident.clone(),
                            _ => abort! {
                                pat,
                                "expected identifier"
                            },
                        },
                        _ => abort! {
                            arg,
                            "expected identifier"
                        },
                    };
                    quote! {
                        let #ident: &mut #struct_name = #ident.into();
                    }
                }
                None => quote! {},
            };

            let receiver_ident = match receiver_arg {
                Some(arg) => match arg {
                    FnArg::Typed(pat) => match *pat.pat {
                        syn::Pat::Ident(ref ident) => ident.ident.clone(),
                        _ => abort! {
                            pat,
                            "expected identifier"
                        },
                    },
                    _ => abort! {
                        arg,
                        "expected identifier"
                    },
                },
                None => abort! {
                    receiver,
                    "expected receiver argument"
                },
            };

            let cb_selfcall_args = if let Some(offset) = cb_receiver_arg_offset {
                let mut args = cb_args.clone();
                args.remove(offset);
                args
            } else {
                cb_args.clone()
            };

            let cb_selfcall_args_identsonly = cb_selfcall_args
                .iter()
                .map(|a| {
                    let ident = match a {
                        FnArg::Typed(pat) => match *pat.pat {
                            syn::Pat::Ident(ref ident) => ident.ident.clone(),
                            _ => abort! {
                                pat,
                                "expected identifier"
                            },
                        },
                        _ => abort! {
                            a,
                            "expected identifier"
                        },
                    };
                    quote! { #ident }
                })
                .collect::<Vec<_>>();

            let unwrap_mb = if is_result {
                quote! { .unwrap(); }
            } else {
                quote! {}
            };

            let call = quote! {
                #receiver_ident.#fname(
                    #( #cb_selfcall_args_identsonly ),*
                )#unwrap_mb
            };

            quote! {
                #[no_mangle]
                pub extern "C" fn #fname(
                    #( #cb_args ),*
                ) #frty {
                    #receive
                    #call
                }
            }
        })
        .collect::<Vec<_>>();

    let cb_mod_name = format_ident!("{}_callbacks", struct_name_string);

    let r: TokenStream = quote! {
        #implementation

        #visibility mod #cb_mod_name {
            use super::*;

            #(#callbacks)*
        }
    }
    .into();

    let s = r.to_string();

    eprintln!("{}", s);

    r
}
