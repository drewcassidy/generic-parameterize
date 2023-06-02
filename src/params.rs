/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use proc_macro::TokenStream;
use proc_macro2;
use quote::{format_ident, ToTokens};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::vec::Vec;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::{Bracket, Comma, Paren};
use syn::{parse_macro_input, Expr, Ident, ItemFn, Lit, Type, TypeBareFn};

/// Generate an identifier-safe string form of a literal
fn lit_to_ident_safe(lit: &Lit) -> String {
    lit.to_token_stream().to_string().replace(".", "_")
}

/// Generate an identifier-safe string form of a type name
fn type_to_ident_safe(ty: &Type) -> String {
    match ty {
        Type::Array(syn::TypeArray { elem, len, .. }) => match &len {
            Expr::Lit(e) => {
                format!("{}x{}", type_to_ident_safe(elem), lit_to_ident_safe(&e.lit))
            }
            _ => format! {"{}Array", type_to_ident_safe(elem)},
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
            format!("{}Ref", type_to_ident_safe(elem))
        }
        Type::Slice(syn::TypeSlice { elem, .. }) => {
            format!("{}Slice", type_to_ident_safe(elem))
        }
        Type::Tuple(_) => "Tuple".to_string(),
        _ => "Unknown".to_string(),
    }
}

struct TypeList {
    ident: Ident,
    types: Vec<Type>,
}

struct LitList {
    ident: Ident,
    lits: Vec<Lit>,
}

struct FormatArg {
    fmt: String,
}

enum ParamEntry {
    Type(Ident, Type),
    Lit(Ident, Lit),
}

impl Display for ParamEntry {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParamEntry::Type(_, ty) => {
                write!(f, "{}", type_to_ident_safe(ty))
            }
            ParamEntry::Lit(_, lit) => {
                write!(f, "{}", lit_to_ident_safe(lit))
            }
        }
    }
}

trait ParamList {
    fn entries(&self) -> Box<dyn Iterator<Item = ParamEntry> + '_>;
}

impl ParamList for TypeList {
    fn entries(&self) -> Box<dyn Iterator<Item = ParamEntry> + '_> {
        Box::new(
            self.types
                .iter()
                .map(move |t| ParamEntry::Type(self.ident.clone(), t.clone())),
        )
    }
}

impl ParamList for LitList {
    fn entries(&self) -> Box<dyn Iterator<Item = ParamEntry> + '_> {
        Box::new(
            self.lits
                .iter()
                .map(move |l| ParamEntry::Lit(self.ident.clone(), l.clone())),
        )
    }
}

enum Argument {
    TypeList(TypeList),
    ConstList(LitList),
    Format(FormatArg),
}

struct ArgumentList {
    args: Vec<Argument>,
}

impl Parse for ArgumentList {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        todo!()
    }
}
