use proc_macro2::{Ident, TokenStream};
use quote::{quote, ToTokens};
use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token, Attribute, Error, Expr, ExprCall, ExprLit, ExprPath, ExprStruct, LitStr, Result, Token,
};

use crate::common::syn::{IdentIndex, Parenthesized};

#[derive(Debug, Default)]
pub struct MockSynDeriveFieldAttr {
    pub transform: Option<MockSynDeriveFieldAttrTransforms>,
    pub skip: Option<MockSynDeriveFieldAttrSkip>,
    pub source: Option<Parenthesized<IdentIndex>>,
}

impl MockSynDeriveFieldAttr {
    pub fn is_match(attr: &Attribute) -> bool {
        attr.path.is_ident("mock_syn")
    }

    pub fn merge(attrs: Vec<Self>) -> Result<Self> {
        let mut iter = attrs.into_iter();
        let mut merged = iter.next().unwrap_or_default();

        for attr in iter {
            merged.transform = attr.transform.or(merged.transform);
            merged.skip = attr.skip.or(merged.skip);
            merged.source = attr.source.or(merged.source);
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
        let mut this = Self::default();

        while !input.is_empty() {
            let ident: Ident = input.parse()?;
            match ident.to_string().as_str() {
                "transform" => {
                    this.transform = Some(input.parse()?);
                }
                "skip" => {
                    this.skip = Some(input.parse()?);
                }
                "source" => {
                    this.source = Some(input.parse()?);
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

        Ok(this)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MockSynDeriveFieldAttrSkip {
    StdDefault,
    Nothing,
    Expr(MockSynDeriveFieldAttrSkipExpr),
}

impl MockSynDeriveFieldAttrSkip {
    pub fn to_tokens_default(&self) -> Option<TokenStream> {
        match self {
            Self::StdDefault => Some(quote! { std::default::Default::default() }),
            Self::Nothing => None,
            Self::Expr(expr) => Some(expr.to_tokens_default()),
        }
    }
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

impl MockSynDeriveFieldAttrSkipExpr {
    pub fn to_tokens_default(&self) -> TokenStream {
        match self {
            Self::Call(expr) => quote! { #expr },
            Self::Path(expr) => quote! { #expr() },
            Self::Lit(expr) => quote! { #expr },
            Self::Struct(expr) => quote! { #expr },
        }
    }
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

#[derive(Clone, Debug)]
pub struct MockSynDeriveFieldAttrTransforms {
    pub transforms: Punctuated<MockSynDeriveFieldAttrTransform, Token![,]>,
}

impl MockSynDeriveFieldAttrTransforms {
    pub fn to_tokens_from(&self) -> TokenStream {
        if self.transforms.is_empty() {
            Self::default().to_tokens_from()
        } else {
            self.transforms
                .iter()
                .map(MockSynDeriveFieldAttrTransform::to_tokens_from)
                .map(|t| quote! { let value = { #t }; })
                .collect()
        }
    }

    pub fn to_tokens_from_default() -> TokenStream {
        Self::default().to_tokens_from()
    }
}

impl Default for MockSynDeriveFieldAttrTransforms {
    fn default() -> Self {
        Self {
            transforms: Punctuated::from_iter([MockSynDeriveFieldAttrTransform::TryFrom]),
        }
    }
}

impl Parse for MockSynDeriveFieldAttrTransforms {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        let _ = parenthesized!(content in input);
        let transforms = content.parse_terminated(MockSynDeriveFieldAttrTransform::parse)?;

        Ok(if transforms.is_empty() {
            Self::default()
        } else {
            Self { transforms }
        })
    }
}

#[derive(Clone, Debug)]
pub enum MockSynDeriveFieldAttrTransform {
    TryFrom,
    Clone,
    ValueMap(Parenthesized<Box<Expr>>),
    OkOrElse(Parenthesized<LitStr>),
    Iter(MockSynDeriveFieldAttrIter),
}

impl MockSynDeriveFieldAttrTransform {
    pub fn to_tokens_from(&self) -> TokenStream {
        match self {
            Self::TryFrom => quote! { TryFrom::try_from(value)? },
            Self::Clone => quote! { value.clone() },
            Self::ValueMap(value_map) => quote! { #value_map },
            Self::OkOrElse(lit_str) => quote! {
                value
                    .as_ref()
                    .ok_or_else(|| ::syn::Error::new(
                        ::syn::spanned::Spanned::span(&__wrapped),
                        #lit_str
                    ))?
                    .clone()
            },
            Self::Iter(iter) => iter.to_tokens_from(),
        }
    }
}

impl Parse for MockSynDeriveFieldAttrTransform {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident: Ident = input.parse()?;
        Ok(match ident.to_string().as_str() {
            "try_from" => Self::TryFrom,
            "clone" => Self::Clone,
            "value_map" => Self::ValueMap(input.parse()?),
            "ok_or_else" => Self::OkOrElse(input.parse()?),
            "iter" => Self::Iter(input.parse()?),
            unknown => {
                return Err(Error::new_spanned(
                    ident,
                    format!("Unknown attribute '{}'", unknown),
                ))
            }
        })
    }
}

#[derive(Clone, Debug)]
pub enum MockSynDeriveFieldAttrIter {
    ValueToValue,
    ValueToValueIndexed,
}

impl MockSynDeriveFieldAttrIter {
    pub fn to_tokens_from(&self) -> TokenStream {
        match self {
            Self::ValueToValue => quote! {
                value.into_iter()
                    .map(TryFrom::try_from)
                    .collect::<syn::Result<_>>()?
            },
            Self::ValueToValueIndexed => quote! {
                value.into_iter()
                    .enumerate()
                    .map(TryFrom::try_from)
                    .collect::<syn::Result<_>>()?
            },
        }
    }
}

impl Parse for MockSynDeriveFieldAttrIter {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(token::Paren) {
            let content;
            let _ = parenthesized!(content in input);

            let first: Ident = content.parse()?;
            let thin_arrow_token: Token![->] = content.parse()?;
            let second: Ident = content.parse()?;

            if first == "v" && second == "v" {
                if !content.is_empty() {
                    let extension: Ident = content.parse()?;
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
        } else {
            Ok(Self::ValueToValue)
        }
    }
}
