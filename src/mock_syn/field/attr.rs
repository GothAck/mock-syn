use std::fmt;

use proc_macro2::Ident;
use quote::{quote, ToTokens};
use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    token, Attribute, Error, Expr, Result, Token,
};

#[derive(Default)]
pub struct MockSynDeriveFieldAttr {
    pub transform: Option<MockSynDeriveFieldAttrTransform>,
    pub skip: Option<Option<Option<Expr>>>,
    pub source: Option<Ident>,
}

impl fmt::Debug for MockSynDeriveFieldAttr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("MockSynDeriveFieldAttr")
            .field("transform", &self.transform)
            .field(
                "skip",
                &self.skip.as_ref().map(|o| {
                    o.as_ref()
                        .map(ToTokens::into_token_stream)
                        .as_ref()
                        .map(ToString::to_string)
                }),
            )
            .field("source", &self.source)
            .finish()
    }
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
                    skip = Some(None);
                    if input.peek(token::Paren) {
                        let content;
                        let _ = parenthesized!(content in input);
                        if content.is_empty() {
                            skip = Some(Some(None));
                        } else {
                            skip = Some(Some(Some(content.parse()?)));
                        }
                    }
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

pub enum MockSynDeriveFieldAttrTransform {
    Clone,
    ValueMap(Box<Expr>),
    Iter(MockSynDeriveFieldAttrIter),
}

impl fmt::Debug for MockSynDeriveFieldAttrTransform {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Clone => f.write_str("Clone"),
            Self::ValueMap(expr) => f
                .debug_tuple("ValueMap")
                .field(&expr.to_token_stream().to_string())
                .finish(),
            Self::Iter(iter) => f.debug_tuple("Iter").field(iter).finish(),
        }
    }
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

pub enum MockSynDeriveFieldAttrIter {
    ValueToValue,
    ValueToValueIndexed,
}

impl fmt::Debug for MockSynDeriveFieldAttrIter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ValueToValue => f.write_str("ValueToValue"),
            Self::ValueToValueIndexed => f.write_str("ValueToValueIndexed"),
        }
    }
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
