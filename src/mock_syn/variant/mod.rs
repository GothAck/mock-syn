mod attr;

use proc_macro2::{Ident, TokenStream};
use quote::{quote, ToTokens, TokenStreamExt};
use syn::{
    parse::{Parse, ParseStream},
    Attribute, Error, Expr, Result, Token, Visibility,
};

use self::attr::*;
use super::*;

#[derive(Debug)]
pub struct MockSynDeriveVariant {
    pub attrs: Vec<Attribute>,
    pub attr: MockSynDeriveVariantAttr,
    pub ident: Ident,
    pub fields: MockSynDeriveFields,
    pub discriminant: Option<(Token![=], Expr)>,
}

impl MockSynDeriveVariant {
    pub fn to_tokens_match_try_from(
        &self,
        enum_ident: &Ident,
        as_ident: &Ident,
    ) -> Result<TokenStream> {
        if self.attr.skip.is_some() {
            Ok(quote! {})
        } else {
            let ident = &self.ident;

            let fields_get = self.to_tokens_match_try_from_fields_get()?;
            let fields_calc = self.to_tokens_match_try_from_fields_calc()?;
            let fields_set = self.to_tokens_match_try_from_fields_set()?;

            Ok(match &self.fields {
                MockSynDeriveFields::Named(..) => quote! {
                    #enum_ident::#ident { #fields_get } => {
                        #fields_calc

                        #as_ident::#ident { #fields_set }
                    },
                },
                MockSynDeriveFields::Unnamed(..) => quote! {
                    #enum_ident::#ident(#fields_get) => {
                        #fields_calc

                        #as_ident::#ident(#fields_set)
                    },
                },
                MockSynDeriveFields::Unit => quote! {
                    #enum_ident::#ident => #as_ident::#ident,
                },
            })
        }
    }

    fn to_tokens_match_try_from_fields_get(&self) -> Result<TokenStream> {
        match &self.fields {
            MockSynDeriveFields::Unnamed(_, fields) => Ok(fields
                .iter()
                .map(MockSynDeriveFieldUnnamed::to_tokens_get)
                .collect()),
            MockSynDeriveFields::Unit => Ok(quote! {}),
            MockSynDeriveFields::Named(..) => Err(Error::new_spanned(
                &self.ident,
                "Named variants are not supported",
            )),
        }
    }

    fn to_tokens_match_try_from_fields_calc(&self) -> Result<TokenStream> {
        match &self.fields {
            MockSynDeriveFields::Unnamed(_, fields) => Ok(fields
                .iter()
                .map(MockSynDeriveFieldUnnamed::to_tokens_calc)
                .collect()),
            MockSynDeriveFields::Unit => Ok(quote! {}),
            MockSynDeriveFields::Named(..) => Err(Error::new_spanned(
                &self.ident,
                "Named variants are not supported",
            )),
        }
    }

    fn to_tokens_match_try_from_fields_set(&self) -> Result<TokenStream> {
        match &self.fields {
            MockSynDeriveFields::Unnamed(_, fields) => Ok(fields
                .iter()
                .map(MockSynDeriveFieldUnnamed::to_tokens_set)
                .collect()),
            MockSynDeriveFields::Unit => Ok(quote! {}),
            MockSynDeriveFields::Named(..) => Err(Error::new_spanned(
                &self.ident,
                "Named variants are not supported",
            )),
        }
    }
}

impl Parse for MockSynDeriveVariant {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let (our_attrs, attrs): (Vec<_>, Vec<_>) = attrs
            .into_iter()
            .partition(MockSynDeriveVariantAttr::is_match);
        let attr = our_attrs
            .into_iter()
            .map(MockSynDeriveVariantAttr::try_from)
            .collect::<Result<Vec<_>>>()
            .and_then(MockSynDeriveVariantAttr::merge)?;
        let _visibility: Visibility = input.parse()?;
        let ident: Ident = input.parse()?;
        let fields = input.parse()?;
        let discriminant = if input.peek(Token![=]) {
            let eq_token: Token![=] = input.parse()?;
            let discriminant: Expr = input.parse()?;
            Some((eq_token, discriminant))
        } else {
            None
        };
        Ok(Self {
            attrs,
            attr,
            ident,
            fields,
            discriminant,
        })
    }
}

impl ToTokens for MockSynDeriveVariant {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append_all(&self.attrs);
        self.ident.to_tokens(tokens);
        self.fields.to_tokens(tokens);
        if let Some((eq_token, disc)) = &self.discriminant {
            eq_token.to_tokens(tokens);
            disc.to_tokens(tokens);
        }
    }
}
