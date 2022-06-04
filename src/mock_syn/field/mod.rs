mod attr;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, IdentFragment, ToTokens};
use syn::{
    braced, parenthesized,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    spanned::Spanned,
    token, Attribute, Error, Index, Result, Token, Type, Visibility,
};

use self::attr::*;

pub trait MockSynDeriveFieldCommon {
    type Ident: IdentFragment;
    const PREFIX: &'static str = "__mock_syn";

    fn localize_ident<I: IdentFragment + ToTokens>(ident: &I) -> Ident {
        format_ident!("{}_{}", Self::PREFIX, ident, span = Spanned::span(ident))
    }

    fn localize_ident_additional<I: IdentFragment + ToTokens>(
        ident: &I,
        additional: &str,
    ) -> Ident {
        format_ident!(
            "{}_{}_{}",
            Self::PREFIX,
            additional,
            ident,
            span = Spanned::span(ident)
        )
    }

    fn attr_source(&self) -> &Self::Ident;
    fn ident_localized(&self) -> Ident;
    fn source_localized(&self) -> Ident;
}

#[derive(Debug)]
pub struct MockSynDeriveFieldNamed {
    pub attrs: Vec<Attribute>,
    pub attr: MockSynDeriveFieldAttr,
    pub attr_source: Option<Ident>,
    pub vis: Visibility,
    pub ident: Ident,
    pub colon_token: Token![:],
    pub ty: Type,
}

impl MockSynDeriveFieldCommon for MockSynDeriveFieldNamed {
    type Ident = Ident;

    fn ident_localized(&self) -> Ident {
        Self::localize_ident(&self.ident)
    }

    fn attr_source(&self) -> &Self::Ident {
        self.attr_source.as_ref().unwrap_or(&self.ident)
    }

    fn source_localized(&self) -> Ident {
        Self::localize_ident(self.attr_source())
    }
}
impl MockSynDeriveFieldNamed {
    pub fn to_tokens_value(&self) -> Result<TokenStream> {
        let ident_localized = self.ident_localized();

        if let Some(skip) = self.attr.skip.as_ref() {
            let default = skip.to_tokens_default();

            Ok(quote! {
                let #ident_localized = {
                    #default
                };
            })
        } else {
            let source = self.attr_source();

            let from = self.to_tokens_from()?;

            Ok(quote! {
                let #ident_localized = {
                    let value = &__wrapped.#source;
                    { #from }
                };
            })
        }
    }

    pub fn to_tokens_set(&self) -> Result<TokenStream> {
        let ident = &self.ident;
        let ident_localized = &self.ident_localized();

        Ok(quote! {
            #ident: #ident_localized,
        })
    }

    fn to_tokens_from(&self) -> Result<TokenStream> {
        Ok(self
            .attr
            .transform
            .as_ref()
            .map(MockSynDeriveFieldAttrTransform::to_tokens_from)
            .unwrap_or_else(|| quote! { TryFrom::try_from(value)? }))
    }
}

impl Parse for MockSynDeriveFieldNamed {
    fn parse(input: ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let (our_attrs, attrs): (Vec<_>, Vec<_>) = attrs
            .into_iter()
            .partition(MockSynDeriveFieldAttr::is_match);
        let attr = our_attrs
            .into_iter()
            .map(MockSynDeriveFieldAttr::try_from)
            .collect::<Result<Vec<_>>>()
            .and_then(MockSynDeriveFieldAttr::merge)?;
        let attr_source = attr.source.as_ref().map(TryInto::try_into).transpose()?;
        let vis = input.parse()?;
        let ident = input.parse()?;
        let colon_token = input.parse()?;
        let ty = input.parse()?;

        Ok(Self {
            attrs,
            attr,
            attr_source,
            vis,
            ident,
            colon_token,
            ty,
        })
    }
}

impl ToTokens for MockSynDeriveFieldNamed {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for attr in &self.attrs {
            attr.to_tokens(tokens);
        }
        self.vis.to_tokens(tokens);
        ToTokens::to_tokens(&self.ident, tokens);
        self.colon_token.to_tokens(tokens);
        self.ty.to_tokens(tokens);
    }
}

#[derive(Debug)]
pub struct MockSynDeriveFieldUnnamed {
    pub index: Index,

    pub attrs: Vec<Attribute>,
    pub attr: MockSynDeriveFieldAttr,
    pub attr_source: Option<Index>,
    pub vis: Visibility,
    pub ty: Type,
}

impl MockSynDeriveFieldCommon for MockSynDeriveFieldUnnamed {
    type Ident = Index;

    fn ident_localized(&self) -> Ident {
        Self::localize_ident(&self.index)
    }

    fn attr_source(&self) -> &Self::Ident {
        self.attr_source.as_ref().unwrap_or(&self.index)
    }

    fn source_localized(&self) -> Ident {
        Self::localize_ident(self.attr_source())
    }
}

impl MockSynDeriveFieldUnnamed {
    pub fn to_tokens_value(&self) -> Result<TokenStream> {
        let ident_localized = self.ident_localized();

        if let Some(skip) = self.attr.skip.as_ref() {
            let default = skip.to_tokens_default();

            Ok(quote! { { #default } })
        } else {
            let from = self.to_tokens_from()?;

            Ok(quote! { { let value = #ident_localized; { #from } } })
        }
    }
    fn to_tokens_from(&self) -> Result<TokenStream> {
        Ok(self
            .attr
            .transform
            .as_ref()
            .map(MockSynDeriveFieldAttrTransform::to_tokens_from)
            .unwrap_or_else(|| quote! { TryFrom::try_from(value)? }))
    }
}

impl MockSynDeriveFieldUnnamed {
    fn parse_indexed(input: ParseStream, index: u32) -> Result<Self> {
        let index = Index {
            index,
            span: input.span(),
        };
        let attrs = input.call(Attribute::parse_outer)?;
        let (our_attrs, attrs): (Vec<_>, Vec<_>) = attrs
            .into_iter()
            .partition(MockSynDeriveFieldAttr::is_match);
        let attr = our_attrs
            .into_iter()
            .map(MockSynDeriveFieldAttr::try_from)
            .collect::<Result<Vec<_>>>()
            .and_then(MockSynDeriveFieldAttr::merge)?;
        let attr_source = attr.source.as_ref().map(TryInto::try_into).transpose()?;
        let vis = input.parse()?;
        let ty = input.parse()?;

        Ok(Self {
            index,
            attrs,
            attr,
            attr_source,
            vis,
            ty,
        })
    }
}

impl ToTokens for MockSynDeriveFieldUnnamed {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for attr in &self.attrs {
            attr.to_tokens(tokens);
        }
        self.vis.to_tokens(tokens);
        self.ty.to_tokens(tokens);
    }
}

#[derive(Debug)]
pub enum MockSynDeriveFields {
    Named(token::Brace, Punctuated<MockSynDeriveFieldNamed, Token![,]>),
    Unnamed(
        token::Paren,
        Punctuated<MockSynDeriveFieldUnnamed, Token![,]>,
    ),
    Unit,
}

impl MockSynDeriveFields {
    pub fn parse_terminated_with_indexed<T, P>(
        input: ParseStream,
        parser: fn(ParseStream, u32) -> Result<T>,
    ) -> Result<Punctuated<T, P>>
    where
        P: Parse,
    {
        let mut punctuated = Punctuated::new();

        let mut index: u32 = 0;
        loop {
            if input.is_empty() {
                break;
            }
            let value = parser(input, index)?;
            punctuated.push_value(value);
            if input.is_empty() {
                break;
            }
            let punct = input.parse()?;
            punctuated.push_punct(punct);
            index = index
                .checked_add(1)
                .ok_or_else(|| Error::new(input.span(), "Integer overflow"))?;
        }

        Ok(punctuated)
    }
}

impl Parse for MockSynDeriveFields {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok({
            if input.peek(token::Brace) {
                let content;
                let token_brace = braced!(content in input);
                Self::Named(
                    token_brace,
                    content.parse_terminated(MockSynDeriveFieldNamed::parse)?,
                )
            } else if input.peek(token::Paren) {
                let content;
                let token_paren = parenthesized!(content in input);
                Self::Unnamed(
                    token_paren,
                    Self::parse_terminated_with_indexed(
                        &content,
                        MockSynDeriveFieldUnnamed::parse_indexed,
                    )?,
                )
            } else {
                Self::Unit
            }
        })
    }
}

impl ToTokens for MockSynDeriveFields {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Named(brace_token, fields) => {
                brace_token.surround(tokens, |tokens| fields.to_tokens(tokens))
            }
            Self::Unnamed(paren_token, fields) => {
                paren_token.surround(tokens, |tokens| fields.to_tokens(tokens))
            }
            Self::Unit => {}
        }
    }
}
