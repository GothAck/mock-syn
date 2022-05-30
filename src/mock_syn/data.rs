use std::fmt;

use syn::{punctuated::Punctuated, token, Token};

use super::*;

pub enum MockSynDeriveData {
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
