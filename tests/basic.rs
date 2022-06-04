use proc_macro2::{Ident, Span};
use syn::{parse_str, Field, Fields, FieldsNamed, FieldsUnnamed, Index, ItemStruct};

use mock_syn::mock_syn;

mock_syn! {
    struct ItemStruct as MyItemStruct {
        #[mock_syn(skip)]
        skipped: Vec<u64>,
        #[mock_syn(skip(defaulted_skip))]
        defaulted_skip: u32,
        fields: MyFields,
    }
}

fn defaulted_skip() -> u32 {
    9
}

mock_syn! {
    enum Fields as MyFields {
        Named(#[mock_syn(source(0))]MyFieldsNamed),
        Unnamed(MyFieldsUnnamed),
        Unit,
        #[mock_syn(skip)]
        #[allow(dead_code)]
        Skipped,
    }
}

mock_syn! {
    struct FieldsNamed as MyFieldsNamed {
        #[mock_syn(source(named), transform(iter(v -> v)))]
        fields: Vec<MyFieldNamed>,
    }
}

mock_syn! {
    struct FieldsUnnamed as MyFieldsUnnamed {
        #[mock_syn(source(unnamed), transform(iter(v -> v indexed)))]
        fields: Vec<MyFieldUnnamed>,
    }
}

mock_syn! {
    #[mock_syn(no_parse)]
    struct Field as MyFieldNamed {
        #[mock_syn(transform(ok_or_error("ident is required")))]
        ident: Ident,
    }
}

mock_syn! {
    #[mock_syn(no_parse, try_from(indexed))]
    struct Field as MyFieldUnnamed {
        #[mock_syn(source(ident), transform(value_map(Index { index: index as u32, span: Span::call_site() })))]
        index: Index,
    }
}

#[test]
fn test_struct_named() {
    let input = r#"
        struct MyStruct {
            field: Here,
        }
    "#;
    let item: ItemStruct = parse_str(input).unwrap();

    let my_item = MyItemStruct::try_from(&item).unwrap();

    assert_eq!(my_item.skipped, vec![]);
    assert_eq!(my_item.defaulted_skip, 9);
    assert_eq!(my_item.ident, "MyStruct");
    assert!(matches!(my_item.fields, MyFields::Named(..)));
    if let MyFields::Named(MyFieldsNamed { fields, .. }) = &my_item.fields {
        assert_eq!(fields.len(), 1);
        let MyFieldNamed { ident, .. } = &fields[0];
        assert_eq!(ident, "field");
    }
}

#[test]
fn test_struct_unnamed() {
    let input = r#"
        struct MyStruct (
            Here,
        );
    "#;
    let item: ItemStruct = parse_str(input).unwrap();

    let my_item = MyItemStruct::try_from(&item).unwrap();

    assert_eq!(my_item.skipped, vec![]);
    assert_eq!(my_item.defaulted_skip, 9);
    assert_eq!(my_item.ident, "MyStruct");
    assert!(matches!(my_item.fields, MyFields::Unnamed(..)));
    if let MyFields::Unnamed(MyFieldsUnnamed { fields, .. }) = &my_item.fields {
        assert_eq!(fields.len(), 1);
        let MyFieldUnnamed { index, .. } = &fields[0];
        assert_eq!(index.index, 0);
    }
}
