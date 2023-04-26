//! Rust Asynchronous FFI Library
//!

use proc_macro::TokenStream;
use proc_macro_error::{abort, proc_macro_error};
use quote::quote;
use std::{
    collections::HashSet,
    hash::{Hash, Hasher},
};
use syn::{
    parse::{Nothing, Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    Expr, FnArg, Ident, ItemImpl, Token, Type,
};

#[derive(Hash, Eq, PartialEq)]
enum CallbackWrapperArgType {
    /// The argument index of the callback function that is a pointer to the instance the callback
    /// function is called on. This pointer must be convertible to a `&self` or `&mut self` reference,
    /// depending on the callback function's receiver parameter mutability.
    Prototype,
}

enum CallbackWrapperArgValue {
    // A single expression
    Expr(Expr),
    // A comma separated list of expressions or '...'
    Params(Vec<CallbackWrapperArgParam>),
}

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

struct CallbackWrapperArg {
    /// The type of the argument
    pub typ: CallbackWrapperArgType,
    /// The value of the argument
    pub value: CallbackWrapperArgValue,
}

impl Hash for CallbackWrapperArg {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.typ.hash(state);
    }
}

impl PartialEq for CallbackWrapperArg {
    fn eq(&self, other: &Self) -> bool {
        self.typ == other.typ
    }
}

impl Eq for CallbackWrapperArg {}

impl Parse for CallbackWrapperArg {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let name: Ident = input.parse()?;
        let name_str = name.to_string();

        let typ = match name_str.as_str() {
            "prototype" => CallbackWrapperArgType::Prototype,
            _ => abort! {
                name,
                "expected `prototype`"
            },
        };

        let value = match typ {
            CallbackWrapperArgType::Prototype => {
                // Syntax: prototype(arg, arg, ..., arg)
                let params =
                    Punctuated::<CallbackWrapperArgParam, Token![,]>::parse_terminated(input)?
                        .into_iter()
                        .collect::<Vec<_>>();
                CallbackWrapperArgValue::Params(params)
            }
        };

        Ok(Self { typ, value })
    }
}

struct CallbackWrappersArgs {
    args: HashSet<CallbackWrapperArg>,
}

impl Parse for CallbackWrappersArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let parsed = Punctuated::<CallbackWrapperArg, Token![,]>::parse_terminated(input)?;

        let args: HashSet<CallbackWrapperArg> = parsed.into_iter().collect();

        Ok(Self { args })
    }
}

#[proc_macro_attribute]
pub fn params(_args: TokenStream, input: TokenStream) -> TokenStream {
    input
}

#[proc_macro_error]
#[proc_macro_attribute]
/// Automatically create a companion C FFI function that calls the Rust function with the
/// arguments the C function received
pub fn callback_wrappers(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = parse_macro_input!(args as Nothing);
    let implementation = parse_macro_input!(input as ItemImpl);

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
                                "expected `params(...)`: {}", e
                            }
                        }
                    }
                } else {
                    CallbackWrapperArgParams { params: Vec::new() }
                };
            let fname = &f.sig.ident;
            let frty = &f.sig.output;

            let receiver = &f.sig.receiver().unwrap();

            // Get the args without the receiver, this will be dropped in for 'Ellipsis'
            let fargs = &f
                .sig
                .inputs
                .iter()
                .filter(|fa| !matches!(fa, syn::FnArg::Receiver(_)))
                .collect::<Vec<_>>();

            let mut cb_args = Vec::new();
            let mut receiver_arg = None;

            for param in args.params {
                match param {
                    CallbackWrapperArgParam::BangArg(arg) => {
                        receiver_arg = Some(arg.clone());

                        cb_args.push(quote! {
                            #arg
                        })
                    }
                    CallbackWrapperArgParam::Arg(arg) => cb_args.push(quote! {
                        #arg
                    }),
                    CallbackWrapperArgParam::Ellipsis => {
                        cb_args.extend(fargs.iter().map(|fa| quote! { #fa }))
                    }
                }
            }

            let receive = match receiver_arg {
                Some(arg) => quote! {
                    let #arg = unsafe { &mut *#arg };
                },
                None => quote! {},
            };

            eprintln!("{:?}", cb_args);

            quote! {
                #[no_mangle]
                pub extern "C" fn #fname(
                    #( #cb_args ),*
                ) #frty {
                    #receive
                }
            }
        })
        .collect::<Vec<_>>();

    quote! {
        #implementation

        #(#callbacks)*
    }
    .into()
}
