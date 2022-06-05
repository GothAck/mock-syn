use std::ops::Deref;

use proc_macro2::{Ident, TokenStream};
use quote::{IdentFragment, ToTokens};
use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    Error, Index, Result,
};

#[derive(Clone, Debug)]
pub struct Parenthesized<T: Parse>(T);

impl<T: Parse> Deref for Parenthesized<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Parse> Parse for Parenthesized<T> {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        let _ = parenthesized!(content in input);
        content.parse().map(Self)
    }
}

impl<T: Parse + ToTokens> ToTokens for Parenthesized<T> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.0.to_tokens(tokens);
    }
}

macro_rules! parenthesized_try_into {
    ($local:ty as $remote:ty) => {
        impl From<$remote> for Parenthesized<$local>
        where
            $local: From<$remote>,
        {
            fn from(value: $remote) -> Parenthesized<$local> {
                Parenthesized(From::from(value))
            }
        }

        impl TryInto<$remote> for Parenthesized<$local>
        where
            $local: TryInto<$remote>,
        {
            type Error = <$local as TryInto<$remote>>::Error;

            fn try_into(self) -> std::result::Result<$remote, Self::Error> {
                self.0.try_into()
            }
        }

        impl<'a> TryInto<$remote> for &'a Parenthesized<$local>
        where
            &'a $local: TryInto<$remote>,
        {
            type Error = <&'a $local as TryInto<$remote>>::Error;

            fn try_into(self) -> std::result::Result<$remote, Self::Error> {
                self.deref().try_into()
            }
        }

        impl<'a> TryInto<&'a $remote> for &'a Parenthesized<$local>
        where
            &'a $local: TryInto<&'a $remote>,
        {
            type Error = <&'a $local as TryInto<&'a $remote>>::Error;

            fn try_into(self) -> std::result::Result<&'a $remote, Self::Error> {
                self.deref().try_into()
            }
        }
    };
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum IdentIndex {
    Ident(Ident),
    Index(Index),
}

impl IdentIndex {
    pub fn as_ident(&self) -> Result<&Ident> {
        match self {
            Self::Ident(v) => Ok(v),
            Self::Index(v) => Err(Error::new(v.span, "Expected an Ident")),
        }
    }

    pub fn as_index(&self) -> Result<&Index> {
        match self {
            Self::Ident(v) => Err(Error::new(v.span(), "Expected an Ident")),
            Self::Index(v) => Ok(v),
        }
    }
}

impl IdentFragment for IdentIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Ident(v) => <Ident as IdentFragment>::fmt(v, f),
            Self::Index(v) => <Index as IdentFragment>::fmt(v, f),
        }
    }
}

impl Parse for IdentIndex {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(syn::Ident) {
            input.parse().map(Self::Ident)
        } else if input.peek(syn::LitInt) {
            input.parse().map(Self::Index)
        } else {
            Err(input.error("expected identifier or integer"))
        }
    }
}

impl ToTokens for IdentIndex {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Ident(v) => v.to_tokens(tokens),
            Self::Index(v) => v.to_tokens(tokens),
        }
    }
}

impl PartialEq<str> for IdentIndex {
    fn eq(&self, other: &str) -> bool {
        match self {
            Self::Ident(v) => v == other,
            Self::Index(v) => v.index.to_string() == other,
        }
    }
}

impl PartialEq<u32> for IdentIndex {
    fn eq(&self, other: &u32) -> bool {
        match self {
            Self::Ident(..) => false,
            Self::Index(Index { index, .. }) => index == other,
        }
    }
}

macro_rules! convert {
    ($local:ident::$method:ident as $remote:ident) => {
        impl From<$remote> for $local {
            fn from(value: $remote) -> Self {
                Self::$remote(value)
            }
        }

        impl TryInto<$remote> for $local {
            type Error = Error;

            fn try_into(self) -> Result<$remote> {
                self.$method().cloned()
            }
        }

        impl TryInto<$remote> for &$local {
            type Error = Error;

            fn try_into(self) -> Result<$remote> {
                self.$method().cloned()
            }
        }

        impl<'a> TryInto<&'a $remote> for &'a $local {
            type Error = Error;

            fn try_into(self) -> Result<&'a $remote> {
                self.$method()
            }
        }
    };
}

convert!(IdentIndex::as_ident as Ident);
convert!(IdentIndex::as_index as Index);

parenthesized_try_into!(IdentIndex as Ident);
parenthesized_try_into!(IdentIndex as Index);
