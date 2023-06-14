/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

//! This crate provides the [parameterize] macro for expanding generic test functions

use proc_macro::TokenStream;
use Default;

use itertools::Itertools;
use proc_macro2::Span;
use quote::{format_ident, ToTokens};
use syn::{parse_macro_input, Expr, Ident, ItemFn, TypeBareFn};

use arguments::{ArgumentList, ArgumentValue};

use crate::extract::Extract;
use crate::params::Param;

mod arguments;
mod extract;
mod fmt;
mod params;

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
///
/// # Arguments
/// Arguments are provided in the [_MetaListNameValueStr_][mlnvs] format. Every argument consists
/// of an identifier, an equals sign, and a value. Arguments are seperated by commas.
///
/// ## Type Lists
/// Type lists are passed using the tuple syntax. There must be exactly one type list argument for every
/// generic type parameter in the target function
///
/// ## Const Lists
/// Const lists are passed using the array syntax, however expressions are not allowed, only literals.
/// There must be exactly one const list argument for every generic const parameter in the target function
///
/// ## Format String
/// An optional format string can be passed with the ident `fmt`. This uses a syntax similar to [`format!`](std::format),
/// however the colon and everything after it is not supported; only the identifier for each
/// parameter and `fn` for the name of the target function
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
/// #[parameterize(T = (i32, f32), N = [4,5,6], fmt = "{fn}_{T}x{N}")]
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
///     fn test_array_i32x4() {test_array::<i32,4>();}
///     #[test]
///     fn test_array_f32x4() {test_array::<f32,4>();}
///     #[test]
///     fn test_array_i32x5() {test_array::<i32,5>();}
///     // etc...
/// }
/// ```
///
/// [mlnvs]: https://doc.rust-lang.org/reference/attributes.html#meta-item-attribute-syntax
fn parameterize_impl(mut args: ArgumentList, mut inner: ItemFn) -> syn::Result<TokenStream> {
    let inner_ident = inner.sig.ident.clone();
    let output = inner.sig.output.clone();

    let attrs = inner.attrs.clone();
    inner.attrs.clear(); // clear attribute on inner, they will be copied to the inner attributes instead

    // consume arguments that are param lists and match them with generic params
    // this has to happen in the same order as the generic parameters
    let param_lists: Vec<_> = inner
        .sig
        .generics
        .params
        .iter()
        .map(|gp| args.consume_paramlist(gp))
        .collect::<syn::Result<_>>()?;

    // Consume format string argument
    let fmt_string = args
        .extract_map(|arg| match &arg.value {
            ArgumentValue::Str(fmt) if arg.ident.to_string() == "fmt".to_string() => {
                Some(fmt.clone())
            }
            _ => None,
        })
        .at_most_one()
        .map_err(|_| syn::Error::new(Span::call_site(), "Multiple fmt strings provided"))?;

    // args should now be empty
    // args.iter().map(|a| syn::Error::new(a.span, format!("Unused macro argument {}", a.value)).to_compile_error()).next()
    for arg in args.args.iter() {
        return Err(syn::Error::new(
            arg.ident.span(),
            format!("Unexpected {} argument `{}`", arg.short_type(), arg.ident),
        ));
    }

    // Produce a list of param values for every iteration,
    // iterate over them, and map them to wrapper functions
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

    // Make the module that we're replacing the function with
    let module: syn::ItemMod = syn::parse_quote! {
        /// Autogenerated test module for a generic test function
        pub mod #inner_ident {
            use super::*;
            #inner

            #(#wrappers)*

            pub const manifest: [#fn_type; #wrapper_len]= [#(#wrapper_idents,)*];
        }
    };

    Ok(module.into_token_stream().into())
}

#[proc_macro_attribute]
pub fn parameterize(args: TokenStream, input: TokenStream) -> TokenStream {
    let inner = parse_macro_input!(input as syn::ItemFn);
    let args = parse_macro_input!(args as ArgumentList);

    match parameterize_impl(args, inner) {
        Ok(output) => output,
        Err(err) => err.to_compile_error().into(),
    }
}
