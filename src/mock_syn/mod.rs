mod attr;
mod data;
mod field;
mod variant;

use proc_macro2::{Ident, TokenStream};
use quote::quote;
use syn::{
    braced,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    Attribute, Generics, Result, Token, Visibility,
};

use self::attr::*;
use self::data::*;
use self::field::*;
use self::variant::*;

#[derive(Debug)]
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

            let fields_get = fields_named
                .iter()
                .map(MockSynDeriveFieldNamed::to_tokens_get)
                .collect::<TokenStream>();

            let fields_calc = fields_named
                .iter()
                .map(MockSynDeriveFieldNamed::to_tokens_calc)
                .collect::<TokenStream>();

            let fields_set = fields_named
                .iter()
                .map(MockSynDeriveFieldNamed::to_tokens_set)
                .collect::<TokenStream>();

            Ok(Some(quote! {
                impl #impl_generics TryFrom<#try_from_ty> for #as_ident #ty_generics #where_clause {
                    type Error = syn::Error;
                    fn try_from(#try_from_pat: #try_from_ty) -> syn::Result<Self> {
                        let __wrapped = value.clone();

                        #fields_get

                        #fields_calc

                        Ok(Self {
                            __wrapped,
                            #fields_set
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
