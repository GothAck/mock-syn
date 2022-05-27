use mock_syn::mock_syn;
use proc_macro2::{Ident, Span};
use syn::{
    parse_str, spanned::Spanned, Data, DataEnum, DeriveInput, Error, Field, Fields, FieldsNamed,
    FieldsUnnamed, Index, Result, Variant,
};

mock_syn! {
    pub struct DeriveInput as DInput {
        pub data: DDataEnum,
    }
}

mock_syn! {
    #[mock_syn(no_parse)]
    pub struct DataEnum as DDataEnum {
        #[mock_syn(transform(iter))]
        pub variants: Vec<DVariant>,
    }
}

impl TryFrom<&Data> for DDataEnum {
    type Error = Error;

    fn try_from(value: &Data) -> Result<Self> {
        match value {
            Data::Struct(..) => Err(Error::new(Span::call_site(), "Structs are not supported")),
            Data::Enum(data) => data.try_into(),
            Data::Union(..) => Err(Error::new(Span::call_site(), "Unions are not supported")),
        }
    }
}

mock_syn! {
    pub struct Variant as DVariant {
        pub fields: DFields,
    }
}

mock_syn! {
    pub enum Fields as DFields {
        Named(DFieldsNamed),
        Unnamed(DFieldsUnnamed),
        Unit,
    }
}

mock_syn! {
    pub struct FieldsNamed as DFieldsNamed {
        #[mock_syn(transform(iter))]
        pub named: Vec<DFieldNamed>,
    }
}

mock_syn! {
    pub struct FieldsUnnamed as DFieldsUnnamed {
        #[mock_syn(transform(iter(v -> v indexed)))]
        pub unnamed: Vec<DFieldUnnamed>,
    }
}

mock_syn! {
    #[mock_syn(no_parse)]
    pub struct Field as DFieldNamed {
        #[mock_syn(transform(value_map(value.clone().ok_or_else(|| syn::Error::new_spanned(&__wrapped, "Fields should be named"))?)))]
        pub ident: Ident,
    }
}

mock_syn! {
    #[mock_syn(no_parse, try_from(indexed))]
    pub struct Field as DFieldUnnamed {
        #[mock_syn(skip(Index { index: index as u32, span: __wrapped.ty.span() }))]
        pub index: Index,
        #[mock_syn(transform(value_map(if value.is_some() { return Err(syn::Error::new_spanned(&__wrapped, "Fields should not be named")) } else { NoIdent } )))]
        pub ident: NoIdent,
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct NoIdent;

#[test]
fn test() {
    let source = r#"
        enum Rar {
            Named {
                field: Ty,
                another: Yay,
            },
            Unnamed(One, Two, Three),
            Unit,
        }
    "#;

    let input: DInput = parse_str(source).unwrap();

    assert_eq!(input.ident, "Rar");
    assert_eq!(input.data.variants.len(), 3);

    let variant = &input.data.variants[0];

    assert_eq!(variant.ident, "Named");
    assert!(matches!(variant.fields, DFields::Named(..)));

    if let DFields::Named(DFieldsNamed { named, .. }) = &variant.fields {
        assert_eq!(named.len(), 2);

        let field = &named[0];

        assert_eq!(field.ident, "field");

        let field = &named[1];

        assert_eq!(field.ident, "another");
    }

    let variant = &input.data.variants[1];

    assert_eq!(variant.ident, "Unnamed");
    assert!(matches!(variant.fields, DFields::Unnamed(..)));

    if let DFields::Unnamed(DFieldsUnnamed { unnamed, .. }) = &variant.fields {
        assert_eq!(unnamed.len(), 3);

        let field = &unnamed[0];

        assert_eq!(field.index.index, 0);
        assert_eq!(field.ident, NoIdent);

        let field = &unnamed[1];

        assert_eq!(field.index.index, 1);
        assert_eq!(field.ident, NoIdent);

        let field = &unnamed[2];

        assert_eq!(field.index.index, 2);
        assert_eq!(field.ident, NoIdent);
    }

    let variant = &input.data.variants[2];

    assert_eq!(variant.ident, "Unit");
    assert!(matches!(variant.fields, DFields::Unit));
}
