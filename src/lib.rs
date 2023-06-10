/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

//! This crate provides the [parameterize] macro for expanding generic test functions

mod extract;
mod fmt;
mod params;

use crate::extract::Extract;
use crate::params::Param;
use itertools::Itertools;
use proc_macro::TokenStream;
use quote::{format_ident, ToTokens};
use syn::parse::discouraged::Speculative;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::Comma;
use syn::{parse_macro_input, Expr, GenericParam, Ident, ItemFn, Lit, LitStr, Type, TypeBareFn};
use Default;

/// One argument in the input to the parameterize macro
#[derive(Clone)]
enum Argument {
    TypeList(Ident, Vec<Type>),
    LitList(Ident, Vec<Lit>),
    Format(String),
}

fn parse_typelist(input: ParseStream) -> Option<syn::Result<Argument>> {
    let ident = input.parse::<Ident>().ok()?;
    input.parse::<syn::token::Eq>().ok()?;
    if input.peek(syn::token::Paren) {
        // everything after this point is an error instead of a no-match when it fails
        let parse = || -> syn::Result<Argument> {
            let tt = input.parse::<syn::TypeTuple>()?;
            let entries: Vec<Type> = tt.elems.iter().cloned().collect();
            Ok(Argument::TypeList(ident, entries))
        };
        Some(parse())
    } else {
        None
    }
}

fn parse_litlist(input: ParseStream) -> Option<syn::Result<Argument>> {
    let ident = input.parse::<Ident>().ok()?;
    input.parse::<syn::token::Eq>().ok()?;
    if input.peek(syn::token::Bracket) {
        // everything after this point is an error instead of a no-match when it fails
        let parse = || -> syn::Result<Argument> {
            let exprs = input.parse::<syn::ExprArray>()?;
            let entries: syn::Result<Vec<Lit>> = exprs
                .elems
                .iter()
                .map(|expr: &Expr| -> syn::Result<Lit> {
                    return if let Expr::Lit(lit) = expr {
                        Ok(lit.lit.clone())
                    } else {
                        Err(syn::Error::new(expr.span(), "Expression is not a literal"))
                    };
                })
                .collect();
            Ok(Argument::LitList(ident, entries?))
        };

        Some(parse())
    } else {
        None
    }
}

fn parse_format(input: ParseStream) -> Option<syn::Result<Argument>> {
    let ident = input.parse::<Ident>().ok()?;
    input.parse::<syn::token::Eq>().ok()?;

    if ident.to_string() == "fmt" {
        // everything after this point is an error instead of a no-match when it fails

        let parse = || -> syn::Result<Argument> {
            let value = input.parse::<LitStr>()?.value();

            Ok(Argument::Format(value))
        };

        Some(parse())
    } else {
        None
    }
}

impl Parse for Argument {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        [parse_typelist, parse_litlist, parse_format]
            .iter()
            .find_map(|f| {
                let fork = input.fork();
                if let Some(arg) = (*f)(&fork) {
                    input.advance_to(&fork);
                    Some(arg)
                } else {
                    None
                }
            })
            .expect("Unexpected or malformed argument.")
    }
}

/// A list of arguments input to the macro
struct ArgumentList {
    pub args: Vec<Argument>,
}

impl Parse for ArgumentList {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let args: Vec<Argument> = Punctuated::<Argument, Comma>::parse_terminated(input)?
            .iter()
            .cloned()
            .collect();
        Ok(Self { args })
    }
}

fn format_params(fmt: &Option<String>, fn_ident: &Ident, params: Vec<&(Ident, Param)>) -> Ident {
    if let Some(ref result) = fmt {
        let mut result = result.clone();
        result = result.replace("{fn}", fn_ident.to_string().as_str());

        for (ident, param) in params {
            result = result.replace(format!("{{{ident}}}").as_str(), param.to_string().as_str());
        }

        format_ident!("{}", result)
    } else {
        format_ident!("{}_{}", fn_ident, params.iter().map(|(_, p)| p).join("_"))
    }
}

/// Expand a generic test function with the given parameter matrix
///
/// # Arguments
///
/// A comma separated list of identifiers and their values.
/// Types are passed using a tuple syntax, and literals are passed using an array syntax
///
///
/// # Examples
///
/// ```
/// # #[cfg(test)]
/// # mod array_tests {
/// # use std::println; // clion thinks this is an error not to include??
/// use generic_parameterize::parameterize;
/// use std::fmt::Debug;
///
/// #[parameterize(T = (i32, f32), N = [4,5,6])]
/// #[test]
/// fn test_array<T: Default, const N : usize>() where [T;N]: Default + Debug{
///     let foo: [T;N] = Default::default();
///     println!("{:?}", foo)
/// }
/// # }
/// ```
///
/// expands to:
///
/// ```ignore
/// mod test_array {
///     use std::println;
///     fn test_array<T: Default, const N : usize>() where [T;N]: Default + std::fmt::Debug{
///         let foo: [T;N] = Default::default();
///         println!("{:?}", foo)
///     }
///
///     #[test]
///     fn test_array_i32_4() {test_array::<i32,4>();}
///     #[test]
///     fn test_array_f32_4() {test_array::<f32,4>();}
///     #[test]
///     fn test_array_i32_5() {test_array::<i32,5>();}
///     // etc...
/// }
#[proc_macro_attribute]
pub fn parameterize(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut inner = parse_macro_input!(input as syn::ItemFn);

    let inner_ident = inner.sig.ident.clone();
    let output = inner.sig.output.clone();

    let attrs = inner.attrs.clone();
    inner.attrs.clear(); // clear attribute on inner, they will be copied to the inner attributes instead

    let mut args = parse_macro_input!(_args as ArgumentList).args;

    // consume arguments that are param lists and match them with generic params
    // this has to happen in the same order as the generic parameters
    let param_lists = inner
        .sig
        .generics
        .params
        .iter()
        .map(|gp| {
            let match_arg = |argument: &Argument| -> Option<Vec<(Ident, Param)>> {
                return match (argument, gp) {
                    (Argument::TypeList(id, tl), GenericParam::Type(tp)) if id == &tp.ident => {
                        Some(
                            tl.iter()
                                .map(|ty| (id.clone(), Param::Type(ty.clone())))
                                .collect(),
                        )
                    }

                    (Argument::LitList(id, ll), GenericParam::Const(cp)) if id == &cp.ident => {
                        Some(
                            ll.iter()
                                .map(|lit| (id.clone(), Param::Lit(lit.clone())))
                                .collect(),
                        )
                    }
                    _ => None,
                };
            };

            args.extract_map(match_arg)
                .at_most_one()
                .unwrap_or_else(|_| {
                    panic!(
                        "Multiple parameterizations provided for generic parameter {}",
                        gp.to_token_stream().to_string()
                    )
                })
                .unwrap_or_else(|| {
                    panic!(
                        "No parameterization provided for generic parameter {}",
                        gp.to_token_stream().to_string()
                    )
                })
        })
        .collect_vec();

    let fmt_string = args
        .extract_map(|arg| match arg {
            Argument::Format(fmt) => Some(fmt.clone()),
            _ => None,
        })
        .at_most_one()
        .unwrap_or_else(|fmts| {
            panic!("Multiple format strings provided: {:?}", fmts.collect_vec())
        });

    let (wrapper_idents, wrappers): (Vec<_>, Vec<_>) = param_lists
        .iter()
        .multi_cartesian_product()
        .map(|params| {
            let param_values = params.iter().map(|(_, p)| p).collect_vec();

            // let fn_ident = format_ident!("{}_{}", inner.sig.ident, param_values.iter().join("_"));
            let fn_ident = format_params(&fmt_string, &inner_ident, params);
            let fn_body: Expr = syn::parse_quote!(#inner_ident::<#(#param_values,)*>());
            let fn_doc = format!(" Wrapper for {}", fn_body.to_token_stream());
            let mut func: ItemFn = syn::parse_quote! {
                #[allow(non_snake_case)]
                #[doc = #fn_doc]
                pub fn #fn_ident() #output {
                    #fn_body
                }
            };

            func.attrs.extend(attrs.iter().cloned());
            (fn_ident, func)
        })
        .unzip();

    let fn_type = TypeBareFn {
        lifetimes: None,
        unsafety: inner.sig.unsafety.clone(),
        abi: inner.sig.abi.clone(),
        fn_token: Default::default(),
        paren_token: Default::default(),
        inputs: Default::default(),
        variadic: None,
        output: inner.sig.output.clone(),
    };
    let wrapper_len = wrappers.len();

    let module: syn::ItemMod = syn::parse_quote! {
        /// Autogenerated test module for a generic test function
        pub mod #inner_ident {
            use super::*;
            #inner

            #(#wrappers)*

            pub const manifest: [#fn_type; #wrapper_len]= [#(#wrapper_idents,)*];
        }
    };

    module.into_token_stream().into()
}
