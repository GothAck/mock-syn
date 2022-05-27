use std::fmt;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens, TokenStreamExt};
use syn::{
    braced, parenthesized,
    parse::{Nothing, Parse, ParseStream},
    punctuated::Punctuated,
    token, Attribute, Error, Expr, Generics, Index, Result, Token, Type, Visibility,
};

pub struct MockSynDerive {
    attrs: Vec<Attribute>,
    attr: MockSynDeriveAttr,
    vis: Visibility,
    ident: Ident,
    #[allow(dead_code)]
    as_token: Token![as],
    as_ident: Ident,
    generics: Generics,

    data: MockSynDeriveData,
}

impl fmt::Debug for MockSynDerive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("MockSynDerive")
            .field("attr", &self.attr)
            .field("ident", &self.ident)
            .field("data", &self.data)
            .finish()
    }
}

impl MockSynDerive {
    pub fn compile(&self) -> Result<TokenStream> {
        match &self.data {
            MockSynDeriveData::Struct {
                struct_token,
                fields_named,
                ..
            } => self.to_tokens_struct(struct_token, fields_named),
            MockSynDeriveData::Enum {
                enum_token,
                variants,
                ..
            } => self.to_tokens_enum(enum_token, variants),
        }
    }

    fn to_tokens_struct(
        &self,
        struct_token: &Token![struct],
        fields_named: &Punctuated<MockSynDeriveFieldNamed, Token![,]>,
    ) -> Result<TokenStream> {
        let MockSynDerive {
            attrs,
            vis,
            ident,
            as_ident,
            generics,
            ..
        } = self;

        let tokens_struct_impl_try_from = self.to_tokens_struct_impl_try_from(fields_named)?;
        let tokens_struct_impl_deref = self.to_tokens_struct_impl_deref()?;
        let tokens_struct_impl_parse = self.to_tokens_struct_impl_parse()?;

        Ok(quote! {
            #(#attrs)*
            #[derive(Clone)]
            #vis #struct_token #as_ident #generics {
                __wrapped: #ident,
                #fields_named
            }
            #tokens_struct_impl_try_from
            #tokens_struct_impl_deref
            #tokens_struct_impl_parse
        })
    }

    fn to_tokens_struct_impl_try_from(
        &self,
        fields_named: &Punctuated<MockSynDeriveFieldNamed, Token![,]>,
    ) -> Result<Option<TokenStream>> {
        if let MockSynDeriveAttrTryFrom::Enable { indexed } =
            self.attr.try_from.clone().unwrap_or_default()
        {
            let as_ident = &self.as_ident;
            let ident = &self.ident;
            let (impl_generics, ty_generics, where_clause) = self.generics.split_for_impl();

            let try_from_ty = indexed
                .map(|_| quote! { (usize, &#ident) })
                .unwrap_or_else(|| quote! { &#ident });

            let try_from_pat = indexed
                .map(|_| quote! { (index, value) })
                .unwrap_or_else(|| quote! { value });

            let fields_def = fields_named
                .iter()
                .map(MockSynDeriveFieldNamed::to_tokens_value)
                .collect::<Result<TokenStream>>()?;

            let fields: TokenStream = fields_named
                .iter()
                .map(|f| &f.ident)
                .map(|ident| quote!( #ident, ))
                .collect();

            Ok(Some(quote! {
                impl #impl_generics TryFrom<#try_from_ty> for #as_ident #ty_generics #where_clause {
                    type Error = syn::Error;
                    fn try_from(#try_from_pat: #try_from_ty) -> syn::Result<Self> {
                        let __wrapped = value.clone();

                        #fields_def

                        Ok(Self {
                            __wrapped,
                            #fields
                        })
                    }
                }
            }))
        } else {
            Ok(None)
        }
    }

    fn to_tokens_struct_impl_deref(&self) -> Result<Option<TokenStream>> {
        if self.attr.no_deref.is_none() {
            let as_ident = &self.as_ident;
            let ident = &self.ident;
            let (impl_generics, ty_generics, where_clause) = self.generics.split_for_impl();

            Ok(Some(quote! {
                impl #impl_generics std::ops::Deref for #as_ident #ty_generics #where_clause {
                    type Target = #ident;
                    fn deref(&self) -> &Self::Target {
                        &self.__wrapped
                    }
                }
                impl #impl_generics std::ops::DerefMut for #as_ident #ty_generics #where_clause {
                    fn deref_mut(&mut self) -> &mut Self::Target {
                        &mut self.__wrapped
                    }
                }
            }))
        } else {
            Ok(None)
        }
    }

    fn to_tokens_struct_impl_parse(&self) -> Result<Option<TokenStream>> {
        if self.attr.no_parse.is_none() {
            let as_ident = &self.as_ident;
            let ident = &self.ident;
            let (impl_generics, ty_generics, where_clause) = self.generics.split_for_impl();

            Ok(Some(quote! {
                impl #impl_generics syn::parse::Parse for #as_ident #ty_generics #where_clause {
                    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
                        let from: #ident = input.parse()?;

                        Self::try_from(&from)
                    }
                }
            }))
        } else {
            Ok(None)
        }
    }

    fn to_tokens_enum(
        &self,
        enum_token: &Token![enum],
        variants: &Punctuated<MockSynDeriveVariant, Token![,]>,
    ) -> Result<TokenStream> {
        let MockSynDerive {
            attrs,
            vis,
            as_ident,
            generics,
            ..
        } = self;

        let tokens_enum_impl_try_from = self.to_tokens_enum_impl_try_from(variants)?;
        // let tokens_enum_impl_deref = self.to_tokens_enum_impl_deref()?;
        // let tokens_enum_impl_parse = self.to_tokens_enum_impl_parse()?;

        Ok(quote! {
            #(#attrs)*
            #[derive(Clone)]
            #vis #enum_token #as_ident #generics {
                #variants
            }
            #tokens_enum_impl_try_from
            // #tokens_enum_impl_deref
            // #tokens_enum_impl_parse
        })
    }

    fn to_tokens_enum_impl_try_from(
        &self,
        variants: &Punctuated<MockSynDeriveVariant, Token![,]>,
    ) -> Result<Option<TokenStream>> {
        if let MockSynDeriveAttrTryFrom::Enable { indexed } =
            self.attr.try_from.clone().unwrap_or_default()
        {
            let as_ident = &self.as_ident;
            let ident = &self.ident;
            let (impl_generics, ty_generics, where_clause) = self.generics.split_for_impl();

            let try_from_ty = indexed
                .map(|_| quote! { (usize, &#ident) })
                .unwrap_or_else(|| quote! { &#ident });

            let try_from_pat = indexed
                .map(|_| quote! { (index, value) })
                .unwrap_or_else(|| quote! { value });

            let variant_matches = variants
                .iter()
                .map(|v| v.to_tokens_match_try_from(ident, as_ident))
                .collect::<Result<TokenStream>>()?;

            let enum_todo = self.attr.enum_todo.as_ref().map(|_| {
                quote! {
                    _ => todo!(),
                }
            });

            // let fields_def = fields_named
            //     .iter()
            //     .map(MockSynDeriveFieldNamed::to_tokens_value)
            //     .collect::<Result<TokenStream>>()?;

            // let fields: TokenStream = fields_named
            //     .iter()
            //     .map(|f| &f.ident)
            //     .map(|ident| quote!( #ident, ))
            //     .collect();

            Ok(Some(quote! {
                impl #impl_generics TryFrom<#try_from_ty> for #as_ident #ty_generics #where_clause {
                    type Error = syn::Error;
                    fn try_from(#try_from_pat: #try_from_ty) -> syn::Result<Self> {
                        Ok(match value {
                            #variant_matches
                            #enum_todo
                        })
                    }
                }
            }))
        } else {
            Ok(None)
        }
    }
}

impl Parse for MockSynDerive {
    fn parse(input: ParseStream) -> Result<Self> {
        let (our_attrs, attrs): (Vec<_>, Vec<_>) = input
            .call(Attribute::parse_outer)?
            .into_iter()
            .partition(MockSynDeriveAttr::is_match);

        let attr = our_attrs
            .into_iter()
            .map(MockSynDeriveAttr::try_from)
            .collect::<Result<_>>()
            .and_then(MockSynDeriveAttr::merge)?;

        let vis = input.parse()?;

        let lookahead = input.lookahead1();

        if lookahead.peek(Token![struct]) {
            let struct_token = input.parse::<Token![struct]>()?;
            let ident = input.parse::<Ident>()?;
            let as_token = input.parse::<Token![as]>()?;
            let as_ident = input.parse::<Ident>()?;
            let generics = input.parse::<Generics>()?;

            let content;
            let brace_token = braced!(content in input);
            let fields_named = content.parse_terminated(MockSynDeriveFieldNamed::parse)?;

            Ok(Self {
                attrs,
                attr,
                vis,
                ident,
                as_token,
                as_ident,
                generics,

                data: MockSynDeriveData::Struct {
                    struct_token,
                    brace_token,
                    fields_named,
                },
            })
        } else if lookahead.peek(Token![enum]) {
            let enum_token = input.parse::<Token![enum]>()?;
            let ident = input.parse::<Ident>()?;
            let as_token = input.parse::<Token![as]>()?;
            let as_ident = input.parse::<Ident>()?;
            let generics = input.parse::<Generics>()?;
            let content;
            let brace_token = braced!(content in input);
            let variants = content.parse_terminated(MockSynDeriveVariant::parse)?;

            Ok(Self {
                attrs,
                attr,
                vis,
                ident,
                as_token,
                as_ident,
                generics,
                data: MockSynDeriveData::Enum {
                    enum_token,
                    brace_token,
                    variants,
                },
            })
        } else {
            Err(lookahead.error())
        }
    }
}

enum MockSynDeriveData {
    Struct {
        struct_token: Token![struct],
        #[allow(dead_code)]
        brace_token: token::Brace,
        fields_named: Punctuated<MockSynDeriveFieldNamed, Token![,]>,
    },
    Enum {
        enum_token: Token![enum],
        #[allow(dead_code)]
        brace_token: token::Brace,
        variants: Punctuated<MockSynDeriveVariant, Token![,]>,
    },
}

impl fmt::Debug for MockSynDeriveData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Struct { fields_named, .. } => f
                .debug_struct("Struct")
                .field("fields_named", &fields_named.iter().collect::<Vec<_>>())
                .finish(),
            Self::Enum { variants, .. } => f
                .debug_struct("Enum")
                .field("variants", &variants.iter().collect::<Vec<_>>())
                .finish(),
        }
    }
}

struct MockSynDeriveFieldNamed {
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
    fn to_tokens_value(&self) -> Result<TokenStream> {
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

struct MockSynDeriveFieldUnnamed {
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
    fn ident_index(&self) -> Ident {
        format_ident!("index_{}", self.index)
    }
    fn to_tokens_value(&self) -> Result<TokenStream> {
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

enum MockSynDeriveFields {
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

struct MockSynDeriveVariant {
    pub attrs: Vec<Attribute>,
    pub ident: Ident,
    pub fields: MockSynDeriveFields,
    pub discriminant: Option<(Token![=], Expr)>,
}

impl fmt::Debug for MockSynDeriveVariant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("MockSynDeriveVariant")
            .field("ident", &self.ident)
            .field("fields", &self.fields)
            .finish()
    }
}

impl MockSynDeriveVariant {
    fn to_tokens_match_try_from(
        &self,
        enum_ident: &Ident,
        as_ident: &Ident,
    ) -> Result<TokenStream> {
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

    fn to_tokens_match_try_from_fields_match(&self) -> Result<TokenStream> {
        match &self.fields {
            MockSynDeriveFields::Unnamed(_, fields) => {
                let ids = fields
                    .iter()
                    .map(|f| f.ident_index())
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

#[derive(Default)]
struct MockSynDeriveAttr {
    try_from: Option<MockSynDeriveAttrTryFrom>,
    no_deref: Option<Nothing>,
    no_parse: Option<Nothing>,
    enum_todo: Option<Nothing>,
}

impl fmt::Debug for MockSynDeriveAttr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("MockSynDeriveAttr")
            .field("try_from", &self.try_from)
            .field("no_deref", &self.no_deref.is_some())
            .field("no_parse", &self.no_parse.is_some())
            .field("enum_todo", &self.enum_todo.is_some())
            .finish()
    }
}

impl MockSynDeriveAttr {
    fn is_match(attribute: &Attribute) -> bool {
        attribute.path.is_ident("mock_syn")
    }

    fn merge(attrs: Vec<Self>) -> Result<Self> {
        let mut iter = attrs.into_iter();
        let mut merged = iter.next().unwrap_or_default();

        for attr in iter {
            if let Some(try_from) = attr.try_from {
                merged.try_from.replace(try_from);
            }
            if let Some(no_deref) = attr.no_deref {
                merged.no_deref.replace(no_deref);
            }
            if let Some(no_parse) = attr.no_parse {
                merged.no_parse.replace(no_parse);
            }
            if let Some(enum_todo) = attr.enum_todo {
                merged.enum_todo.replace(enum_todo);
            }
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

#[derive(Clone)]
enum MockSynDeriveAttrTryFrom {
    Disable,
    Enable { indexed: Option<()> },
}

impl fmt::Debug for MockSynDeriveAttrTryFrom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Disable => f.write_str("Disable"),
            Self::Enable { indexed } => f
                .debug_struct("Enable")
                .field("indexed", &indexed.is_some())
                .finish(),
        }
    }
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

#[derive(Default)]
struct MockSynDeriveFieldAttr {
    transform: Option<MockSynDeriveFieldAttrTransform>,
    skip: Option<Option<Option<Expr>>>,
    source: Option<Ident>,
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
    fn is_match(attr: &Attribute) -> bool {
        attr.path.is_ident("mock_syn")
    }

    fn merge(attrs: Vec<Self>) -> Result<Self> {
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

enum MockSynDeriveFieldAttrTransform {
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

enum MockSynDeriveFieldAttrIter {
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

#[cfg(test)]
mod test {
    use syn::parse_str;

    use mock_syn_test_common::*;

    use super::MockSynDerive;

    #[test]
    fn test() {
        let input = r#"
            struct ItemEnum as MyItemEnum {
                #[mo]
                generics: MyGenerics,
                #[mock_syn(transform(iter))]
                variants: Vec<MyVariant>,
            }
        "#;
        let derive: MockSynDerive = parse_str(input).print_syn_error_spanned(input).unwrap();

        println!("{:?}", derive);

        // assert!(false);
    }

    #[test]
    fn test_enum() {
        let input = r#"
            enum RarEnum as MyRarEnum {
                Variant0,
                Variant1(Blah),
                Variant2(YayRar),
            }
        "#;
        let derive: MockSynDerive = parse_str(input).print_syn_error_spanned(input).unwrap();

        println!("{:?}", derive);

        let tokens = derive
            .compile()
            .print_syn_error_spanned(input)
            .unwrap()
            .to_string();

        println!("{}", rustfmt(tokens));

        // assert!(false);
    }
}
