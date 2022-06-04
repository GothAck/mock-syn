mod common;
mod mock_syn;

use proc_macro::TokenStream;
use syn::{parse_macro_input, Error};

use self::mock_syn::MockSynDerive;

#[proc_macro]
pub fn mock_syn(input: TokenStream) -> TokenStream {
    parse_macro_input!(input as MockSynDerive)
        .compile()
        .unwrap_or_else(Error::into_compile_error)
        .into()
}
