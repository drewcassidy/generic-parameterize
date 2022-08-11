//! This crate provides the [test_parameterize] macro for expanding generic test functions

use Default;
use itertools::Itertools;
use proc_macro2;
use proc_macro::TokenStream;
use quote::{format_ident, ToTokens};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::{Bracket, Comma, Paren};
use syn::{Expr, Lit, Type, Ident, parse_macro_input};


/// Generate an identifier-safe string form of a literal
fn lit_to_ident_safe(lit: &Lit) -> String {
    lit.to_token_stream().to_string().replace(".", "_")
}

/// Generate an identifier-safe string form of a type name
fn type_to_ident_safe(ty: &Type) -> String {
    match ty {
        Type::Array(syn::TypeArray { elem, len, .. }) => {
            match &len {
                Expr::Lit(e) => {
                    format!("{}x{}", type_to_ident_safe(elem), lit_to_ident_safe(&e.lit))
                }
                _ => format! {"{}Array", type_to_ident_safe(elem)}
            }
        }
        Type::BareFn(_) => { "Fn".to_string() }
        Type::Never(_) => { "Never".to_string() }
        Type::Path(syn::TypePath { path, .. }) => {
            let ident = path.get_ident().expect("Expected an identifier");
            ident.to_string()
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
        Type::Tuple(_) => { "Tuple".to_string() }
        _ => { "Unknown".to_string() }
    }
}

/// A single entry for parameterization, either a literal or a type
enum ParameterEntry {
    Lit { lit: Lit },
    Type { ty: Type },
}

impl ParameterEntry {
    fn from_expr(expr: &Expr) -> Result<Self, syn::Error> {
        return if let Expr::Lit(lit) = expr {
            Ok(Self::Lit { lit: lit.lit.clone() })
        } else {
            Err(syn::Error::new(expr.span(), "Expression is not a literal"))
        };
    }
}

impl ToTokens for ParameterEntry {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            ParameterEntry::Lit { lit } => { lit.to_tokens(tokens); }
            ParameterEntry::Type { ty } => { ty.to_tokens(tokens); }
        }
    }
}

impl Display for ParameterEntry {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParameterEntry::Lit { lit } => { write!(f, "{}", lit_to_ident_safe(lit)) }
            ParameterEntry::Type { ty } => { write!(f, "{}", type_to_ident_safe(ty)) }
        }
    }
}

/// A list of parameter entries, consisting of an identifier (for a generic parameter) and a
/// list of entries, which are either types or literal values
struct ParameterEntryList {
    ident: Ident,
    entries: Vec<ParameterEntry>,
}

impl Parse for ParameterEntryList {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.parse::<syn::Ident>()?;
        input.parse::<syn::token::Eq>()?;

        return if input.peek(Paren) {
            let tt = input.parse::<syn::TypeTuple>()?;
            let entries =
                tt.elems.iter()
                    .map(|ty| ParameterEntry::Type { ty: ty.clone() })
                    .collect();

            Ok(Self { ident, entries })
        } else if input.peek(Bracket) {
            let exprs = input.parse::<syn::ExprArray>()?;
            let entries: Result<Vec<ParameterEntry>, syn::Error> =
                exprs.elems.iter()
                    .map(|e| { Ok(ParameterEntry::from_expr(e)?) })
                    .collect();
            Ok(Self { ident, entries: entries? })
        } else {
            Err(syn::Error::new(input.span(), "Unknown parameter entry"))
        };
    }
}

/// A matrix of parameters to be expanded.
/// The cartesian product of all entry lists is used when expanding
struct ParameterMatrix {
    params: HashMap<Ident, ParameterEntryList>,
}

impl Parse for ParameterMatrix {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut result = ParameterMatrix { params: Default::default() };
        let entries = Punctuated::<ParameterEntryList, Comma>::parse_terminated(input)?;

        for entry in entries {
            let ident = &entry.ident;

            result.params.insert(ident.clone(), entry);
        }

        Ok(result)
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
/// use crate::generic_parameterize::test_parameterize;
/// #[test_parameterize(T = (i32, f32), N = [4,5,6])]
/// fn test_array<T: Default, const N : usize>() {
///     let foo: [T;N] = Default::default();
///     println!("{}", foo);
/// }
/// ```
///
/// expands to:
///
/// ```ignore
/// #[cfg(test)]
/// mod test_array {
///     use std::println;
///     fn test_array<T, const N : usize>() {
///         let foo: [T;N] = Default::default();
///         println!("{}", foo);
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
pub fn test_parameterize(_args: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::ItemFn);
    let mut args = parse_macro_input!(_args as ParameterMatrix);

    let mut param_matrix = Vec::<Vec<ParameterEntry>>::new();

    for param in &input.sig.generics.params {
        let ident = match param {
            syn::GenericParam::Type(t) => { &t.ident }
            syn::GenericParam::Const(c) => { &c.ident }
            syn::GenericParam::Lifetime(_) => { panic!("Lifetimes are unsupported"); }
        };

        let entry = args.params.remove(ident)
            .expect(&*format!("Generic parameter {} is not parameterized!", ident));

        param_matrix.push(entry.entries);
    }

    let mod_ident = input.sig.ident.clone();
    let mut fns = Vec::<syn::ItemFn>::new();

    for params in param_matrix.iter().multi_cartesian_product() {
        let fn_ident = format_ident!("{}_{}", input.sig.ident, params.iter().join("_"));
        let func: syn::ItemFn = syn::parse_quote! {
            #[test]
            #[allow(non_snake_case)]
            pub fn #fn_ident() {
                #mod_ident::<#(#params,)*>();
            }
        };

        fns.push(func);
    }

    let module: syn::ItemMod = syn::parse_quote! {
        #[cfg(test)]
        /// Autogenerated test module for a generic test function
        pub mod #mod_ident {
            #input

            #(#fns)*
        }
    };

    module.into_token_stream().into()
}

