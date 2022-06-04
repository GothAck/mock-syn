use syn::{punctuated::Punctuated, token, Token};

use super::*;

#[derive(Debug)]
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
