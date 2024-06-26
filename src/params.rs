/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use proc_macro2::TokenStream;
use quote::ToTokens;
use std::fmt::{Display, Formatter};
use std::hash::{DefaultHasher, Hash, Hasher};
use strum::EnumDiscriminants;
use syn::{Expr, Lit, Type};

/// A Param is a value we are parameterizing over. It can be a generic type, a generic const,
/// or a function argument
#[derive(Clone, Debug, EnumDiscriminants)]
#[strum_discriminants(name(ParamType))]
pub(crate) enum Param {
    GenericType(Type),
    GenericConst(Lit),
    FnArg(Expr),
}

impl Display for ParamType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParamType::GenericType => f.write_str("type generic"),
            ParamType::GenericConst => f.write_str("const generic"),
            ParamType::FnArg => f.write_str("argument"),
        }
    }
}

impl ToTokens for Param {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Param::GenericType(ty) => ty.to_tokens(tokens),
            Param::GenericConst(lit) => lit.to_tokens(tokens),
            Param::FnArg(expr) => expr.to_tokens(tokens),
        }
    }
}

impl Display for Param {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.ident_safe().as_str())
    }
}

fn calculate_hash<T: Hash>(t: &T) -> String {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    let hex = format!("{:X}", s.finish());
    return hex[0..4].to_string();
}

impl Param {
    fn ident_safe(&self) -> String {
        fn lit_ident_safe(lit: &Lit) -> String {
            let lit = lit.to_token_stream().to_string().replace(".", "p");
            let lit = lit.replace("\"", "");
            lit
        }
        fn type_ident_safe(ty: &Type) -> String {
            match ty {
                Type::Array(syn::TypeArray { elem, len, .. }) => match &len {
                    Expr::Lit(e) => {
                        format!("{}x{}", type_ident_safe(elem), lit_ident_safe(&e.lit))
                    }
                    _ => format! {"{}Array", type_ident_safe(elem)},
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
                    format!("{}Ref", type_ident_safe(elem))
                }
                Type::Slice(syn::TypeSlice { elem, .. }) => {
                    format!("{}Slice", type_ident_safe(elem))
                }
                Type::Tuple(_) => "Tuple".to_string(),
                _ => "Unknown".to_string(),
            }
        }
        match self {
            Param::GenericType(ty) => type_ident_safe(ty),
            Param::GenericConst(lit) => lit_ident_safe(lit),
            Param::FnArg(Expr::Lit(lit)) => lit_ident_safe(&lit.lit),
            Param::FnArg(expr) => format!("expr{}", calculate_hash(expr)),
        }
    }
}
