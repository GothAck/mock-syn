use proc_macro2::Ident;
use quote::ToTokens;
use syn::{
    parse::{Parse, ParseStream},
    Attribute, Error, Result, Token,
};

#[derive(Debug, Default)]
pub struct MockSynDeriveVariantAttr {
    pub skip: Option<()>,
}

impl MockSynDeriveVariantAttr {
    pub fn is_match(attr: &Attribute) -> bool {
        attr.path.is_ident("mock_syn")
    }

    pub fn merge(attrs: Vec<Self>) -> Result<Self> {
        let mut iter = attrs.into_iter();
        let mut merged = iter.next().unwrap_or_default();

        for attr in iter {
            merged.skip = attr.skip.or(merged.skip);
        }

        Ok(merged)
    }
}

impl TryFrom<Attribute> for MockSynDeriveVariantAttr {
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

impl Parse for MockSynDeriveVariantAttr {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut skip = None;

        while !input.is_empty() {
            let ident: Ident = input.parse()?;
            match ident.to_string().as_str() {
                "skip" => {
                    skip = Some(());
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

        Ok(Self { skip })
    }
}
