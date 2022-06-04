use proc_macro2::Ident;
use quote::ToTokens;
use syn::{
    parenthesized,
    parse::{Nothing, Parse, ParseStream},
    Attribute, Error, Result, Token,
};

#[derive(Debug, Default)]
pub struct MockSynDeriveAttr {
    pub try_from: Option<MockSynDeriveAttrTryFrom>,
    pub no_deref: Option<Nothing>,
    pub no_parse: Option<Nothing>,
    pub enum_todo: Option<Nothing>,
}

impl MockSynDeriveAttr {
    pub fn is_match(attribute: &Attribute) -> bool {
        attribute.path.is_ident("mock_syn")
    }

    pub fn merge(attrs: Vec<Self>) -> Result<Self> {
        let mut iter = attrs.into_iter();
        let mut merged = iter.next().unwrap_or_default();

        for attr in iter {
            merged.try_from = attr.try_from.or(merged.try_from);
            merged.no_deref = attr.no_deref.or(merged.no_deref);
            merged.no_parse = attr.no_parse.or(merged.no_parse);
            merged.enum_todo = attr.enum_todo.or(merged.enum_todo);
        }

        Ok(merged)
    }
}

impl TryFrom<Attribute> for MockSynDeriveAttr {
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

impl Parse for MockSynDeriveAttr {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut ret = MockSynDeriveAttr::default();

        while !input.is_empty() {
            let ident: Ident = input.parse()?;
            match ident.to_string().as_str() {
                "try_from" => {
                    let content;
                    let _ = parenthesized!(content in input);
                    ret.try_from = Some(content.parse()?);
                }
                "no_deref" => {
                    ret.no_deref = Some(input.parse()?);
                }
                "no_parse" => {
                    ret.no_parse = Some(input.parse()?);
                }
                "enum_todo" => {
                    ret.enum_todo = Some(input.parse()?);
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

        Ok(ret)
    }
}

#[derive(Clone, Debug)]
pub enum MockSynDeriveAttrTryFrom {
    Disable,
    Enable { indexed: Option<()> },
}

impl Default for MockSynDeriveAttrTryFrom {
    fn default() -> Self {
        Self::Enable { indexed: None }
    }
}

impl Parse for MockSynDeriveAttrTryFrom {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut ret = Self::default();

        while !input.is_empty() {
            let ident: Ident = input.parse()?;
            match ident.to_string().as_ref() {
                "disable" => {
                    if let Self::Enable { .. } = ret {
                        ret = Self::Disable;
                    }
                }
                "indexed" => {
                    if let Self::Disable = &ret {
                        ret = Self::default();
                    }
                    if let Self::Enable { indexed, .. } = &mut ret {
                        *indexed = Some(());
                    }
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

        Ok(ret)
    }
}
