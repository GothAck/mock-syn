mod attr;

use std::fmt;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::{
    braced, parenthesized,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token, Attribute, Error, Expr, Index, Result, Token, Type, Visibility,
};

use self::attr::*;

pub struct MockSynDeriveFieldNamed {
    pub attrs: Vec<Attribute>,
    pub attr: MockSynDeriveFieldAttr,
    pub vis: Visibility,
    pub ident: Ident,
    pub colon_token: Token![:],
    pub ty: Type,
}

impl fmt::Debug for MockSynDeriveFieldNamed {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("MockSynDeriveFieldNamed")
            .field("attr", &self.attr)
            .field("ident", &self.ident)
            .finish()
    }
}

impl MockSynDeriveFieldNamed {
    pub fn to_tokens_value(&self) -> Result<TokenStream> {
        let ident = &self.ident;
        if let Some(skip) = self.attr.skip.as_ref() {
            let default = match skip {
                Some(Some(Expr::Call(expr_call))) => quote! { #expr_call },
                Some(Some(Expr::Path(expr_path))) => quote! { #expr_path() },
                Some(Some(Expr::Lit(expr_lit))) => quote! { #expr_lit },
                Some(Some(Expr::Struct(expr_struct))) => quote! { #expr_struct },
                Some(Some(expr)) => {
                    return Err(Error::new_spanned(
                        expr,
                        format!(
                            "Invalid expression '{:?}'",
                            ToTokens::into_token_stream(expr).to_string()
                        ),
                    ))
                }
                Some(None) => return Ok(quote! {}),
                None => quote! { std::default::Default::default() },
            };

            Ok(quote! {
                let #ident = {
                    #default
                };
            })
        } else {
            let source = self.attr.source.as_ref().unwrap_or(ident);

            let from = self.to_tokens_from()?;

            Ok(quote! {
                let #ident = {
                    let value = &__wrapped.#source;
                    { #from }
                };
            })
        }
    }

    fn to_tokens_from(&self) -> Result<TokenStream> {
        Ok(match &self.attr.transform {
            None => quote! { TryFrom::try_from(value)? },
            Some(MockSynDeriveFieldAttrTransform::Clone) => quote! { value.clone() },
            Some(MockSynDeriveFieldAttrTransform::ValueMap(value_map)) => quote! { #value_map },
            Some(MockSynDeriveFieldAttrTransform::Iter(
                MockSynDeriveFieldAttrIter::ValueToValue,
            )) => quote! {
                value.into_iter()
                    .map(TryFrom::try_from)
                    .collect::<syn::Result<_>>()?
            },
            Some(MockSynDeriveFieldAttrTransform::Iter(
                MockSynDeriveFieldAttrIter::ValueToValueIndexed,
            )) => quote! {
                value.into_iter()
                    .enumerate()
                    .map(TryFrom::try_from)
                    .collect::<syn::Result<_>>()?
            },
        })
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
        let vis = input.parse()?;
        let ident = input.parse()?;
        let colon_token = input.parse()?;
        let ty = input.parse()?;

        Ok(Self {
            attrs,
            attr,
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

pub struct MockSynDeriveFieldUnnamed {
    pub index: Index,

    pub attrs: Vec<Attribute>,
    pub attr: MockSynDeriveFieldAttr,
    pub vis: Visibility,
    pub ty: Type,
}

impl fmt::Debug for MockSynDeriveFieldUnnamed {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("MockSynDeriveFieldUnnamed")
            .field("index", &self.index.index)
            .field("attr", &self.attr)
            .finish()
    }
}

impl MockSynDeriveFieldUnnamed {
    pub fn ident_index(&self) -> Ident {
        format_ident!("index_{}", self.index)
    }
    pub fn to_tokens_value(&self) -> Result<TokenStream> {
        let ident_index = self.ident_index();
        if let Some(skip) = self.attr.skip.as_ref() {
            let default = match skip {
                Some(Some(Expr::Call(expr_call))) => quote! { #expr_call },
                Some(Some(Expr::Path(expr_path))) => quote! { #expr_path() },
                Some(Some(Expr::Lit(expr_lit))) => quote! { #expr_lit },
                Some(Some(expr)) => {
                    return Err(Error::new_spanned(
                        expr,
                        format!(
                            "Invalid expression '{:?}'",
                            ToTokens::into_token_stream(expr).to_string()
                        ),
                    ))
                }
                Some(None) => return Ok(quote! {}),
                None => quote! { std::default::Default::default() },
            };

            Ok(quote! { { #default } })
        } else {
            let from = self.to_tokens_from()?;

            Ok(quote! { { let value = #ident_index; { #from } } })
        }
    }
    fn to_tokens_from(&self) -> Result<TokenStream> {
        Ok(match &self.attr.transform {
            None => quote! { TryFrom::try_from(value)? },
            Some(MockSynDeriveFieldAttrTransform::Clone) => quote! { value.clone() },
            Some(MockSynDeriveFieldAttrTransform::ValueMap(value_map)) => quote! { #value_map },
            Some(MockSynDeriveFieldAttrTransform::Iter(
                MockSynDeriveFieldAttrIter::ValueToValue,
            )) => quote! {
                value.into_iter()
                    .map(TryFrom::try_from)
                    .collect::<syn::Result<_>>()?
            },
            Some(MockSynDeriveFieldAttrTransform::Iter(
                MockSynDeriveFieldAttrIter::ValueToValueIndexed,
            )) => quote! {
                value.into_iter()
                    .enumerate()
                    .map(TryFrom::try_from)
                    .collect::<syn::Result<_>>()?
            },
        })
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
        let vis = input.parse()?;
        let ty = input.parse()?;

        Ok(Self {
            index,
            attrs,
            attr,
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

pub enum MockSynDeriveFields {
    Named(token::Brace, Punctuated<MockSynDeriveFieldNamed, Token![,]>),
    Unnamed(
        token::Paren,
        Punctuated<MockSynDeriveFieldUnnamed, Token![,]>,
    ),
    Unit,
}

impl fmt::Debug for MockSynDeriveFields {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Named(_, fields_named) => f
                .debug_tuple("MockSynDeriveFields::Named")
                .field(&fields_named.iter().collect::<Vec<_>>())
                .finish(),
            Self::Unnamed(_, fields_unnamed) => f
                .debug_tuple("MockSynDeriveFields::Unnamed")
                .field(&fields_unnamed.iter().collect::<Vec<_>>())
                .finish(),
            Self::Unit => f.write_str("MockSynDeriveFields::Unit"),
        }
    }
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
