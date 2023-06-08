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
use crate::params::{Argument, ArgumentList, ParamList};
use itertools::Itertools;
use proc_macro::TokenStream;
use proc_macro2;
use quote::{format_ident, ToTokens};
use std::collections::HashMap;
use std::fmt::{format, Display, Formatter};
use std::iter::{repeat, zip};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::{Bracket, Comma, Paren};
use syn::{parse_macro_input, Expr, GenericParam, Ident, ItemFn, Lit, Type, TypeBareFn, TypeParam};
use Default;
//
// /// Generate an identifier-safe string form of a literal
// fn lit_to_ident_safe(lit: &Lit) -> String {
//     lit.to_token_stream().to_string().replace(".", "_")
// }
//
// /// Generate an identifier-safe string form of a type name
// fn type_to_ident_safe(ty: &Type) -> String {
//     match ty {
//         Type::Array(syn::TypeArray { elem, len, .. }) => match &len {
//             Expr::Lit(e) => {
//                 format!("{}x{}", type_to_ident_safe(elem), lit_to_ident_safe(&e.lit))
//             }
//             _ => format! {"{}Array", type_to_ident_safe(elem)},
//         },
//         Type::BareFn(_) => "Fn".to_string(),
//         Type::Never(_) => "Never".to_string(),
//         Type::Path(syn::TypePath { path, .. }) => {
//             let ident = path.get_ident().expect("Expected an identifier");
//             ident
//                 .to_string()
//                 .replace("::", "_")
//                 .replace("<", "_")
//                 .replace(">", "")
//         }
//         Type::Reference(syn::TypeReference { elem, .. }) => {
//             format!("{}Ref", type_to_ident_safe(elem))
//         }
//         Type::Slice(syn::TypeSlice { elem, .. }) => {
//             format!("{}Slice", type_to_ident_safe(elem))
//         }
//         Type::Tuple(_) => "Tuple".to_string(),
//         _ => "Unknown".to_string(),
//     }
// }
//
// /// A single entry for parameterization, either a literal or a type
// enum ParameterEntry {
//     Lit { lit: Lit },
//     Type { ty: Type },
// }
//
// impl ParameterEntry {
//     fn from_expr(expr: &Expr) -> Result<Self, syn::Error> {
//         return if let Expr::Lit(lit) = expr {
//             Ok(Self::Lit {
//                 lit: lit.lit.clone(),
//             })
//         } else {
//             Err(syn::Error::new(expr.span(), "Expression is not a literal"))
//         };
//     }
// }
//
// impl ToTokens for ParameterEntry {
//     fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
//         match self {
//             ParameterEntry::Lit { lit } => {
//                 lit.to_tokens(tokens);
//             }
//             ParameterEntry::Type { ty } => {
//                 ty.to_tokens(tokens);
//             }
//         }
//     }
// }
//
// impl Display for ParameterEntry {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         match self {
//             ParameterEntry::Lit { lit } => {
//                 write!(f, "{}", lit_to_ident_safe(lit))
//             }
//             ParameterEntry::Type { ty } => {
//                 write!(f, "{}", type_to_ident_safe(ty))
//             }
//         }
//     }
// }
//
// /// A list of parameter entries, consisting of an identifier (for a generic parameter) and a
// /// list of entries, which are either types or literal values
// struct ParameterEntryList {
//     ident: Ident,
//     entries: Vec<ParameterEntry>,
// }
//
// impl Parse for ParameterEntryList {
//     fn parse(input: ParseStream) -> syn::Result<Self> {
//         let ident = input.parse::<syn::Ident>()?;
//         input.parse::<syn::token::Eq>()?;
//
//         return if input.peek(Paren) {
//             let tt = input.parse::<syn::TypeTuple>()?;
//             let entries = tt
//                 .elems
//                 .iter()
//                 .map(|ty| ParameterEntry::Type { ty: ty.clone() })
//                 .collect();
//
//             Ok(Self { ident, entries })
//         } else if input.peek(Bracket) {
//             let exprs = input.parse::<syn::ExprArray>()?;
//             let entries: Result<Vec<ParameterEntry>, syn::Error> = exprs
//                 .elems
//                 .iter()
//                 .map(|e| Ok(ParameterEntry::from_expr(e)?))
//                 .collect();
//             Ok(Self {
//                 ident,
//                 entries: entries?,
//             })
//         } else {
//             Err(syn::Error::new(input.span(), "Unknown parameter entry"))
//         };
//     }
// }
//
// /// A matrix of parameters to be expanded.
// /// The cartesian product of all entry lists is used when expanding
// struct ParameterMatrix {
//     params: HashMap<Ident, ParameterEntryList>,
// }
//
// impl Parse for ParameterMatrix {
//     fn parse(input: ParseStream) -> syn::Result<Self> {
//         let mut result = ParameterMatrix {
//             params: Default::default(),
//         };
//         let entries = Punctuated::<ParameterEntryList, Comma>::parse_terminated(input)?;
//
//         for entry in entries {
//             let ident = &entry.ident;
//
//             result.params.insert(ident.clone(), entry);
//         }
//
//         Ok(result)
//     }
// }

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
            let match_arg = |argument: &Argument| -> Option<(Ident, Box<dyn ParamList>)> {
                return match (argument, gp) {
                    (Argument::TypeList(id, tl), GenericParam::Type(tp)) if id == &tp.ident => {
                        Some((id.clone(), Box::new(tl.clone())))
                    }

                    (Argument::LitList(id, ll), GenericParam::Const(cp)) if id == &cp.ident => {
                        Some((id.clone(), Box::new(ll.clone())))
                    }
                    _ => None,
                };
            };

            args.extract_map(match_arg)
                .at_most_one()
                .unwrap_or_else(|_| {
                    panic!(
                        "Multiple parameterizations found for generic parameter {}",
                        gp.to_token_stream().to_string()
                    )
                })
                .unwrap_or_else(|| {
                    panic!(
                        "Multiple parameterizations found for generic parameter {}",
                        gp.to_token_stream().to_string()
                    )
                })
        })
        .collect_vec();

    &(&param_lists
        .iter()
        .map(|pl| zip(repeat(pl.0.clone()), pl.1.params()))
        .multi_cartesian_product());
    //
    // let mut param_matrix = Vec::<Vec<ParameterEntry>>::new();
    //
    // for param in &input.sig.generics.params {
    //     let ident = match param {
    //         syn::GenericParam::Type(t) => &t.ident,
    //         syn::GenericParam::Const(c) => &c.ident,
    //         syn::GenericParam::Lifetime(_) => {
    //             panic!("Lifetimes are unsupported");
    //         }
    //     };
    //
    //     let entry = args.params.remove(ident).expect(&*format!(
    //         "Generic parameter {} is not parameterized!",
    //         ident
    //     ));
    //
    //     param_matrix.push(entry.entries);
    // }
    //
    // let mod_ident = input.sig.ident.clone();
    // let output = input.sig.output.clone();
    // let mut fns = Vec::<ItemFn>::new();
    //
    // for params in param_matrix.iter().multi_cartesian_product() {
    //     let fn_ident = format_ident!("{}_{}", input.sig.ident, params.iter().join("_"));
    //     let mut func: ItemFn = syn::parse_quote! {
    //         #[allow(non_snake_case)]
    //         pub fn #fn_ident() #output {
    //             #mod_ident::<#(#params,)*>()
    //         }
    //     };
    //
    //     func.attrs.extend(attrs.iter().cloned());
    //
    //     fns.push(func);
    // }
    //
    // let fn_idents: Vec<_> = fns.iter().map(|f: &ItemFn| f.sig.ident.clone()).collect();
    // let fn_type = TypeBareFn {
    //     lifetimes: None,
    //     unsafety: input.sig.unsafety.clone(),
    //     abi: input.sig.abi.clone(),
    //     fn_token: Default::default(),
    //     paren_token: Default::default(),
    //     inputs: Default::default(),
    //     variadic: input.sig.variadic.clone(),
    //     output: input.sig.output.clone(),
    // };
    // let fn_cnt = fn_idents.len();
    //
    // let module: syn::ItemMod = syn::parse_quote! {
    //     /// Autogenerated test module for a generic test function
    //     pub mod #mod_ident {
    //         use super::*;
    //         #inner
    //
    //         #(#fns)*
    //
    //         pub const manifest: [#fn_type; #fn_cnt]= [#(#fn_idents,)*];
    //     }
    // };
    //
    // module.into_token_stream().into()
    todo!()
}
