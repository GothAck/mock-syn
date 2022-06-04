mod attr;

use proc_macro2::{Ident, TokenStream};
use quote::{quote, ToTokens, TokenStreamExt};
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token, Attribute, Error, Expr, Result, Token, Visibility,
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

            let fields_match = self.to_tokens_match_try_from_fields_match()?;
            let fields_into = self.to_tokens_match_try_from_fields_into()?;

            Ok(match &self.fields {
                MockSynDeriveFields::Named(..) => quote! {
                    #enum_ident::#ident { #fields_match } => #as_ident::#ident { #fields_into },
                },
                MockSynDeriveFields::Unnamed(..) => quote! {
                    #enum_ident::#ident(#fields_match) => #as_ident::#ident(#fields_into),
                },
                MockSynDeriveFields::Unit => quote! {
                    #enum_ident::#ident => #as_ident::#ident,
                },
            })
        }
    }

    fn to_tokens_match_try_from_fields_match(&self) -> Result<TokenStream> {
        match &self.fields {
            MockSynDeriveFields::Unnamed(_, fields) => {
                let ids = fields
                    .iter()
                    .map(|f| f.ident_localized())
                    .collect::<Punctuated<_, token::Comma>>();
                Ok(quote! { #ids })
            }
            MockSynDeriveFields::Unit => Ok(quote! {}),
            MockSynDeriveFields::Named(..) => Err(Error::new_spanned(
                &self.ident,
                "Named variants are not supported",
            )),
        }
    }

    fn to_tokens_match_try_from_fields_into(&self) -> Result<TokenStream> {
        match &self.fields {
            MockSynDeriveFields::Unnamed(_, fields) => {
                let into = fields
                    .iter()
                    .map(|f| f.to_tokens_value())
                    .collect::<Result<Punctuated<_, token::Comma>>>()?;
                Ok(quote! { #into })
            }
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
