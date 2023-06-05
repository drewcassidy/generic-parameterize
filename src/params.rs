/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use crate::params::Argument::{LitList, TypeList};
use quote::ToTokens;
use std::vec::Vec;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::{Bracket, Comma, Paren};
use syn::{Expr, Ident, Lit, Type};

trait Param: ToTokens + Clone {
    fn ident_safe(&self) -> String;
}

impl Param for Lit {
    fn ident_safe(&self) -> String {
        self.to_token_stream().to_string().replace(".", "p")
    }
}

impl Param for Type {
    fn ident_safe(&self) -> String {
        match self {
            Type::Array(syn::TypeArray { elem, len, .. }) => match &len {
                Expr::Lit(e) => {
                    format!("{}x{}", elem.ident_safe(), &e.lit.ident_safe())
                }
                _ => format! {"{}Array", elem.ident_safe()},
            },
            Type::BareFn(_) => "Fn".to_string(),
            Type::Never(_) => "Never".to_string(),
            Type::Path(syn::TypePath { path, .. }) => {
                let ident = path.get_ident().expect("Expected an identifier");
                ident
                    .to_string()
                    .replace("::", "_")
                    .replace("<", "_")
                    .replace(">", "")
            }
            Type::Reference(syn::TypeReference { elem, .. }) => {
                format!("{}Ref", elem.ident_safe())
            }
            Type::Slice(syn::TypeSlice { elem, .. }) => {
                format!("{}Slice", elem.ident_safe())
            }
            Type::Tuple(_) => "Tuple".to_string(),
            _ => "Unknown".to_string(),
        }
    }
}

trait ParamList {
    fn try_parse(input: ParseStream) -> Option<syn::Result<Self>>
    where
        Self: Sized;
}

impl ParamList for Vec<Type> {
    fn try_parse(input: ParseStream) -> Option<syn::Result<Self>> {
        if input.peek(Paren) {
            fn parse(input: ParseStream) -> syn::Result<Vec<Type>> {
                let tt = input.parse::<syn::TypeTuple>()?;
                let entries: Vec<Type> = tt.elems.iter().cloned().collect();
                Ok(entries)
            }
            return Some(parse(input));
        } else {
            None
        }
    }
}

impl ParamList for Vec<Lit> {
    fn try_parse(input: ParseStream) -> Option<syn::Result<Self>> {
        if input.peek(Bracket) {
            fn expr_to_lit(expr: &Expr) -> syn::Result<Lit> {
                return if let Expr::Lit(lit) = expr {
                    Ok(lit.lit.clone())
                } else {
                    Err(syn::Error::new(expr.span(), "Expression is not a literal"))
                };
            }
            fn parse(input: ParseStream) -> syn::Result<Vec<Lit>> {
                let exprs = input.parse::<syn::ExprArray>()?;
                let entries: syn::Result<Vec<Lit>> = exprs.elems.iter().map(expr_to_lit).collect();
                Ok(entries?)
            }
            return Some(parse(input));
        } else {
            None
        }
    }
}

/// One argument in the input to the parameterize macro
#[derive(Clone)]
enum Argument {
    TypeList(Ident, Vec<Type>),
    LitList(Ident, Vec<Lit>),
}

impl Argument {
    /// a paramlist or None. for filter-mapping the list of args
    fn as_param_list(&self) -> Option<(Ident, Box<dyn ParamList>)> {
        return match self {
            TypeList(id, tl) => Some((id.clone(), Box::new(tl.clone()))),
            LitList(id, ll) => Some((id.clone(), Box::new(ll.clone()))),
            _ => None,
        };
    }
}

impl Parse for Argument {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.parse::<syn::Ident>()?;
        input.parse::<syn::token::Eq>()?;

        return if let Some(res) = Vec::<Type>::try_parse(input) {
            Ok(TypeList(ident, res?))
        } else if let Some(res) = Vec::<Lit>::try_parse(input) {
            Ok(LitList(ident, res?))
        } else {
            Err(syn::Error::new(
                input.span(),
                "Unexpected token while parsing macro argument",
            ))
        };
    }
}

/// A list of arguments input to the macro
struct ArgumentList {
    args: Vec<Argument>,
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
