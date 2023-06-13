/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use crate::extract::{Extract, ExtractIterator, ExtractMap};
use itertools::Itertools;
use proc_macro2::{Ident, Span};
use syn::parse::discouraged::Speculative;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::Comma;
use syn::{Expr, GenericParam, Lit, LitStr, Type};

use crate::params::Param;

/// One argument in the input to the parameterize macro
#[derive(Clone, Debug)]
pub(crate) enum ArgumentValue {
    TypeList(Vec<Type>),
    LitList(Vec<Lit>),
    Str(String),
}

#[derive(Clone, Debug)]
pub(crate) struct Argument {
    pub ident: Ident,
    pub value: ArgumentValue,
}

fn parse_typelist(input: ParseStream) -> Option<syn::Result<Argument>> {
    let ident = input.parse::<Ident>().ok()?;
    input.parse::<syn::token::Eq>().ok()?;
    if input.peek(syn::token::Paren) {
        // everything after this point is an error instead of a no-match when it fails
        let parse = || -> syn::Result<Argument> {
            let tt = input.parse::<syn::TypeTuple>()?;
            let entries: Vec<Type> = tt.elems.iter().cloned().collect();
            Ok(Argument {
                ident,
                value: ArgumentValue::TypeList(entries),
            })
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
            Ok(Argument {
                ident,
                value: ArgumentValue::LitList(entries?),
            })
        };

        Some(parse())
    } else {
        None
    }
}

fn parse_str(input: ParseStream) -> Option<syn::Result<Argument>> {
    let ident = input.parse::<Ident>().ok()?;
    input.parse::<syn::token::Eq>().ok()?;

    // everything after this point is an error instead of a no-match when it fails

    let parse = || -> syn::Result<Argument> {
        let value = input.parse::<LitStr>()?.value();

        Ok(Argument {
            ident,
            value: ArgumentValue::Str(value),
        })
    };

    Some(parse())
}

impl Parse for Argument {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        [parse_typelist, parse_litlist, parse_str]
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
            .unwrap_or(Err(input.error("Unexpected or malformed argument.")))
    }
}

impl Argument {
    pub fn short_name(&self) -> &str {
        match self.value {
            ArgumentValue::TypeList(_) => "type list",
            ArgumentValue::LitList(_) => "const list",
            ArgumentValue::Str(_) => "string",
        }
    }
    // pub fn match_paramlist(&self, gp: &GenericParam) -> Option<syn::Result<Vec<(Ident, Param)>>> {
    //     match (&self.ident, &self.value, gp) {
    //         (id, ArgumentValue::TypeList(tl), GenericParam::Type(tp)) if id == &tp.ident => Some(
    //             tl.iter()
    //                 .map(|ty| (id.clone(), Param::Type(ty.clone())))
    //                 .collect(),
    //         ),
    //
    //         (id, ArgumentValue::LitList(ll), GenericParam::Const(cp)) if id == &cp.ident => Some(
    //             ll.iter()
    //                 .map(|lit| (id.clone(), Param::Lit(lit.clone())))
    //                 .collect(),
    //         ),
    //
    //         _ => None,
    //     }
    // }
}

/// A list of arguments input to the macro
pub(crate) struct ArgumentList {
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

impl Extract for ArgumentList {
    type Item = Argument;

    fn extract<F: FnMut(&Self::Item) -> bool>(&mut self, f: F) -> ExtractIterator<Self::Item, F> {
        self.args.extract(f)
    }

    fn extract_map<U, F: FnMut(&Self::Item) -> Option<U>>(
        &mut self,
        f: F,
    ) -> ExtractMap<Self::Item, U, F> {
        self.args.extract_map(f)
    }
}

impl ArgumentList {
    pub fn consume_paramlist(&mut self, gp: &GenericParam) -> syn::Result<Vec<(Ident, Param)>> {
        let (g_ident, g_name) = match gp {
            GenericParam::Lifetime(lt) => Err(syn::Error::new(
                lt.span(),
                "Parameterizing lifetimes is not supported",
            )),
            GenericParam::Type(t) => Ok((&t.ident, "type")),
            GenericParam::Const(c) => Ok((&c.ident, "const")),
        }?;
        self.extract_map(|arg| -> Option<syn::Result<Vec<(Ident, Param)>>> {
            return if &arg.ident == g_ident {
                match (&arg.value, gp) {
                    (ArgumentValue::TypeList(tl), GenericParam::Type(_)) => Some(
                        Ok(tl.iter()
                            .map(|ty| (arg.ident.clone(), Param::Type(ty.clone())))
                            .collect()),
                    ),
                    (ArgumentValue::LitList(ll), GenericParam::Const(_)) => Some(
                        Ok(ll.iter()
                            .map(|lit| (arg.ident.clone(), Param::Lit(lit.clone())))
                            .collect()),
                    ),
                    (ArgumentValue::TypeList(_), _) | (ArgumentValue::LitList(_), _) => Some(Err(syn::Error::new(
                        arg.ident.span(),
                        format!("Mismatched parameterization: Expected {} list but found {}", g_name, arg.short_name()),
                    ))),
                    /* fall through, in case theres a generic argument named for example "fmt". there probably shouldn't be though*/
                    (_, _) => None }
            } else {
                None
            }
        })
        .at_most_one()
        .map_err(|_| {
            // more than one match
            syn::Error::new(
                Span::call_site(),
                format!("Multiple {g_name} parameterizations provided for `{g_ident}`"),
            )
        })?
        .ok_or(syn::Error::new(Span::call_site(), format!("No {g_name} parameterization provided for `{g_ident}`")))?
        // no matches
    }
}
