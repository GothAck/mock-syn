use proc_macro2::Ident;
use quote::{quote, ToTokens};
use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    token, Attribute, Error, Expr, ExprCall, ExprLit, ExprPath, ExprStruct, Result, Token,
};

use crate::common::syn::IdentIndex;

#[derive(Debug, Default)]
pub struct MockSynDeriveFieldAttr {
    pub transform: Option<MockSynDeriveFieldAttrTransform>,
    pub skip: Option<MockSynDeriveFieldAttrSkip>,
    pub source: Option<IdentIndex>,
}

impl MockSynDeriveFieldAttr {
    pub fn is_match(attr: &Attribute) -> bool {
        attr.path.is_ident("mock_syn")
    }

    pub fn merge(attrs: Vec<Self>) -> Result<Self> {
        let mut iter = attrs.into_iter();
        let mut merged = iter.next().unwrap_or_default();

        for attr in iter {
            if let Some(transform) = attr.transform {
                merged.transform.replace(transform);
            }
            if let Some(skip) = attr.skip {
                merged.skip.replace(skip);
            }
            if let Some(source) = attr.source {
                merged.source.replace(source);
            }
        }

        Ok(merged)
    }
}

impl TryFrom<Attribute> for MockSynDeriveFieldAttr {
    type Error = Error;
    fn try_from(value: Attribute) -> Result<Self> {
        if !Self::is_match(&value) {
            return Err(Error::new_spanned(
                &value,
                format!(
                    "Attribute '{:?}' not supported",
                    value.path.to_token_stream().to_string()
                ),
            ));
        }
        value.parse_args()
    }
}

impl Parse for MockSynDeriveFieldAttr {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut transform = None;
        // let mut iter = None;
        let mut skip = None;
        let mut source = None;

        while !input.is_empty() {
            let ident: Ident = input.parse()?;
            match ident.to_string().as_str() {
                "transform" => {
                    let content;
                    let _ = parenthesized!(content in input);
                    transform = Some(content.parse()?);
                }
                "skip" => {
                    skip = Some(input.parse()?);
                }
                "source" => {
                    let content;
                    let _ = parenthesized!(content in input);
                    source = Some(content.parse()?);
                }
                unknown => {
                    return Err(Error::new_spanned(
                        ident,
                        format!("Unknown attribute '{}'", unknown),
                    ))
                }
            }
            if !input.is_empty() {
                let _: Token![,] = input.parse()?;
            }
        }

        Ok(Self {
            transform,
            // iter,
            skip,
            source,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MockSynDeriveFieldAttrSkip {
    StdDefault,
    Nothing,
    Expr(MockSynDeriveFieldAttrSkipExpr),
}

impl Parse for MockSynDeriveFieldAttrSkip {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(token::Paren) {
            let content;
            let _ = parenthesized!(content in input);
            if content.is_empty() {
                Ok(Self::Nothing)
            } else {
                Ok(Self::Expr(content.parse()?))
            }
        } else {
            Ok(Self::StdDefault)
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MockSynDeriveFieldAttrSkipExpr {
    Call(ExprCall),
    Path(ExprPath),
    Lit(ExprLit),
    Struct(ExprStruct),
}

impl Parse for MockSynDeriveFieldAttrSkipExpr {
    fn parse(input: ParseStream) -> Result<Self> {
        let expr: Expr = input.parse()?;
        match expr {
            Expr::Call(value) => Ok(Self::Call(value)),
            Expr::Path(value) => Ok(Self::Path(value)),
            Expr::Lit(value) => Ok(Self::Lit(value)),
            Expr::Struct(value) => Ok(Self::Struct(value)),
            _ => Err(Error::new_spanned(expr, "Invalid expression")),
        }
    }
}

#[derive(Debug)]
pub enum MockSynDeriveFieldAttrTransform {
    Clone,
    ValueMap(Box<Expr>),
    Iter(MockSynDeriveFieldAttrIter),
}

impl Parse for MockSynDeriveFieldAttrTransform {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident: Ident = input.parse()?;
        Ok(match ident.to_string().as_str() {
            "clone" => Self::Clone,
            "value_map" => {
                let content;
                let _ = parenthesized!(content in input);
                Self::ValueMap(content.parse()?)
            }
            "iter" => {
                if input.peek(token::Paren) {
                    let content;
                    let _ = parenthesized!(content in input);
                    Self::Iter(content.parse()?)
                } else {
                    Self::Iter(MockSynDeriveFieldAttrIter::ValueToValue)
                }
            }
            unknown => {
                return Err(Error::new_spanned(
                    ident,
                    format!("Unknown attribute '{}'", unknown),
                ))
            }
        })
    }
}

#[derive(Debug)]
pub enum MockSynDeriveFieldAttrIter {
    ValueToValue,
    ValueToValueIndexed,
}

impl Parse for MockSynDeriveFieldAttrIter {
    fn parse(input: ParseStream) -> Result<Self> {
        let first: Ident = input.parse()?;
        let thin_arrow_token: Token![->] = input.parse()?;
        let second: Ident = input.parse()?;

        if first == "v" && second == "v" {
            if !input.is_empty() {
                let extension: Ident = input.parse()?;
                if extension != "indexed" {
                    let tokens = quote! { #first #thin_arrow_token #second #extension };
                    return Err(Error::new_spanned(
                        tokens,
                        format!("Invalid iter value '{} -> {} {}'", first, second, extension),
                    ));
                }
                return Ok(Self::ValueToValueIndexed);
            }
            return Ok(Self::ValueToValue);
        }

        let tokens = quote! { #first #thin_arrow_token #second };
        Err(Error::new_spanned(
            tokens,
            format!("Invalid iter value '{} -> {}", first, second),
        ))
    }
}
