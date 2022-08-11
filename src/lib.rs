extern crate proc_macro;
extern crate core;
extern crate quote;

use proc_macro2;

use proc_macro::TokenStream;
use syn::{parse_macro_input, ItemFn, ExprAssign, GenericParam, Expr, Ident, ExprPath, Type, Lit, TypeTuple, ExprArray, Path, Token, ExprLit, TypeReference, TypeSlice, TypePath, LitStr, TypeArray, Visibility, Attribute, ExprType, ExprTuple};
use quote::{format_ident, ToTokens};
use syn::punctuated::Punctuated;
use syn::token::{Bracket, Comma, Paren, Token};
use Default;
use std::collections::HashMap;
use std::fmt::{Display, format, Formatter, write};
use std::ops::Deref;
use std::string::ParseError;
use itertools::Itertools;
use syn::BinOp::Eq;
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use quote::quote;

// /// Example of [function-like procedural macro][1].
// ///
// /// [1]: https://doc.rust-lang.org/reference/procedural-macros.html#function-like-procedural-macros
// #[proc_macro]
// pub fn my_macro(input: TokenStream) -> TokenStream {
//     let input = parse_macro_input!(input as DeriveInput);
//
//     let tokens = quote! {
//         #input
//
//         struct Hello;
//     };
//
//     tokens.into()
// }
//
// /// Example of user-defined [derive mode macro][1]
// ///
// /// [1]: https://doc.rust-lang.org/reference/procedural-macros.html#derive-mode-macros
// #[proc_macro_derive(MyDerive)]
// pub fn my_derive(_input: TokenStream) -> TokenStream {
//     let tokens = quote! {
//         struct Hello;
//     };
//
//     tokens.into()
// }
//
// /// Example of user-defined [procedural macro attribute][1].
// ///
// /// [1]: https://doc.rust-lang.org/reference/procedural-macros.html#attribute-macros
// #[proc_macro_attribute]
// pub fn my_attribute(_args: TokenStream, input: TokenStream) -> TokenStream {
//     let input = parse_macro_input!(input as DeriveInput);
//
//     let tokens = quote! {
//         #input
//
//         struct Hello;
//     };
//
//     tokens.into()
// }

fn lit_to_ident_safe(lit: &Lit) -> String {
    lit.to_token_stream().to_string().replace(".", "_")
}

fn type_to_ident_safe(ty: &Type) -> String {
    match ty {
        Type::Array(TypeArray { elem, len, .. }) => {
            match &len {
                Expr::Lit(e) => { format!("{}x{}", type_to_ident_safe(elem), lit_to_ident_safe(&e.lit)) }
                _ => format! {"{}Array", type_to_ident_safe(elem)}
            }
        }
        Type::BareFn(_) => { "Fn".to_string() }
        Type::Never(_) => { "Never".to_string() }
        Type::Path(TypePath { path, .. }) => {
            let ident = path.get_ident().expect("Expected an identifier");
            ident.to_string().replace("::", "_").replace("<", "_").replace(">", "")
        }
        Type::Reference(TypeReference { elem, .. }) => { format!("{}Ref", type_to_ident_safe(elem)) }
        Type::Slice(TypeSlice { elem, .. }) => { format!("{}Slice", type_to_ident_safe(elem)) }
        Type::Tuple(_) => { "Tuple".to_string() }
        _ => { "Unknown".to_string() }
    }
}

fn ident_safe(expr: &Expr) -> String {
    match expr {
        Expr::Lit(l) => {
            lit_to_ident_safe(&l.lit)
        }
        Expr::Type(t) => {
            type_to_ident_safe(&t.ty)
        }
        _ => "Unknown".to_string()
    }
}

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

struct ParameterEntryList {
    ident: Ident,
    entries: Vec<ParameterEntry>,
}

impl Parse for ParameterEntryList {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.parse::<syn::Ident>()?;
        input.parse::<syn::token::Eq>()?;

        return if input.peek(Paren) {
            let tt = input.parse::<TypeTuple>()?;
            let entries =
                tt.elems.iter()
                    .map(|ty| ParameterEntry::Type { ty: ty.clone() })
                    .collect();

            Ok(Self { ident, entries })
        } else if input.peek(Bracket) {
            let exprs = input.parse::<ExprArray>()?;
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

struct ParameterMatrix {
    params: HashMap<Ident, ParameterEntryList>,
}

impl Parse for ParameterMatrix {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut result = ParameterMatrix { params: Default::default() };
        let entries = Punctuated::<ParameterEntryList, Comma>::parse_terminated(input)?;

        for entry in entries {
            let ident = &entry.ident;

            println!("parsing entry for {:?}", ident);

            result.params.insert(ident.clone(), entry);
        }

        Ok(result)
    }
}

#[proc_macro_attribute]
pub fn parameterize(_args: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as ItemFn);
    let mut args = parse_macro_input!(_args as ParameterMatrix);

    let mut gparam_matrix = Vec::<Vec::<ParameterEntry>>::new();

    for gparam in &input.sig.generics.params {
        let ident = match gparam {
            GenericParam::Type(t) => { &t.ident }
            GenericParam::Const(c) => { &c.ident }
            GenericParam::Lifetime(_) => { panic!("Lifetimes are unsupported"); }
        };

        let entry = args.params.remove(ident)
            .expect(&*format!("Generic parameter {} is not parametrized!", ident));

        gparam_matrix.push(entry.entries);

        println!("doing stuff with {}", ident);
    }

    // let test_attr = TokenStream::from(quote!("#[cfg(test)]"));
    // let test_attr = parse_macro_input!(test_attr with Attribute::parse_outer);

    let mod_ident = input.sig.ident.clone();
    let mut fns = Vec::<syn::ItemFn>::new();

    for params in gparam_matrix.iter().multi_cartesian_product() {
        // println!("{:?}", gparam_token_matrix);
        let fn_ident = input.sig.ident.clone();
        let fn_ident = format_ident!("{}_{}",fn_ident, params.iter().join("_"));
        println!("{}", fn_ident);
        let mut func: syn::ItemFn = syn::parse_quote! {
            #[test]
            pub fn #fn_ident() {
                #mod_ident::<#(#params,)*>();
            }
        };

        fns.push(func);
    }


    let f1 = fns[0].clone();
    let mut module: syn::ItemMod = syn::parse_quote! {
        #[cfg(test)]
        /// Autogenerated test module for a generic test function
        pub mod #mod_ident {
            #input

            #(#fns)*
        }
    };


    println!("{:?}", module.to_token_stream());
    module.into_token_stream().into()
    //
    // let mut gparam_idents: Vec<(Ident, Vec<Expr>)> = Default::default();
    //
    // for gparam in gparams {
    //     let param_i = match gparam {
    //         GenericParam::Type(t) => {
    //             //
    //             &t.ident
    //         }
    //         GenericParam::Const(c) => { &c.ident }
    //         GenericParam::Lifetime(_) => { panic!("Lifetimes are unsupported"); }
    //     };
    //
    //     for param in &args {
    //         if let Path(p) = &param.left.deref() {
    //             if let Some(i) = p.path.get_ident() {
    //                 if i == param_i {
    //                     if let Array(vals) = &param.right.deref() {
    //                         println!("{:?} found", i.to_string());
    //
    //                         for v in &vals.elems {
    //                             println!("{:?} found", v.to_token_stream());
    //                         }
    //
    //                         gparam_idents.push((i.clone(), vals.elems.iter().cloned().collect()
    //                         ))
    //                     } else {}
    //                 }
    //             }
    //         }
    //     }
    // }
    //
    // for param in args {
    //     println!("{:?}", param.into_token_stream());
    // }
    // input.into_token_stream().into()
}

