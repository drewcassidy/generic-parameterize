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

/// The value of an [`Argument`]; everything after the equal sign
#[derive(Clone, Debug)]
pub(crate) enum ArgumentValue {
    TypeList(Vec<Type>),
    LitList(Vec<Lit>),
    Str(String),
}

/// One argument in the input to the parameterize macro
#[derive(Clone, Debug)]
pub(crate) struct Argument {
    pub ident: Ident,
    pub value: ArgumentValue,
}

/// Parse a parenthesized list of types
fn parse_typelist(input: ParseStream) -> Option<syn::Result<ArgumentValue>> {
    let parse = || {
        let tt = input.parse::<syn::TypeTuple>()?;
        let entries: Vec<Type> = tt.elems.iter().cloned().collect();
        Ok(ArgumentValue::TypeList(entries))
    };

    // match on parentheses. Anything invalid after is an error
    input.peek(syn::token::Paren).then_some(parse())
}

/// Parse a bracketed list of literals
fn parse_litlist(input: ParseStream) -> Option<syn::Result<ArgumentValue>> {
    let parse = || {
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
        Ok(ArgumentValue::LitList(entries?))
    };

    // match on brackets. anything invalid after is an error
    input.peek(syn::token::Bracket).then_some(parse())
}

/// Parse a string argument
fn parse_str(input: ParseStream) -> Option<syn::Result<ArgumentValue>> {
    // no way for a string argument parse to fail, it either matches or it doesnt
    input
        .parse::<LitStr>()
        .ok()
        .map(|lit| Ok(ArgumentValue::Str(lit.value())))
}

impl Parse for Argument {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // parse the ident and equals sign
        let ident = input.parse::<Ident>()?;
        input.parse::<syn::token::Eq>()?;

        // iterate over the known parse functions for arguments
        [parse_typelist, parse_litlist, parse_str]
            .iter()
            .find_map(|f| {
                // fork the buffer, so we can rewind if there isnt a match
                let fork = input.fork();

                // if the parse function returns a match, return a syn::Result<Argument>,
                // otherwise None to advance to the next parse function
                if let Some(value) = (*f)(&fork) {
                    input.advance_to(&fork);
                    Some(value.map(|v| Self {
                        ident: ident.clone(),
                        value: v,
                    }))
                } else {
                    None
                }
            })
            .unwrap_or(Err(input.error("Unexpected or malformed argument.")))
    }
}

impl Argument {
    /// Get a user-friendly name for the type of argument this is
    pub fn short_type(&self) -> &str {
        match self.value {
            ArgumentValue::TypeList(_) => "type list",
            ArgumentValue::LitList(_) => "const list",
            ArgumentValue::Str(_) => "string",
        }
    }
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
    /// consume a paramlist from the argument list that matches the given generic parameter
    /// and return it.
    /// Returns an error if there is a type mismatch, or if there is not exactly one match
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
                        format!("Mismatched parameterization: Expected {} list but found {}", g_name, arg.short_type()),
                    ))),
                    /* fall through, in case theres a generic argument named for example "fmt". there probably shouldn't be though */
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
