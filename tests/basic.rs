use syn::{parse_str, ItemStruct};

use mock_syn::mock_syn;

mock_syn! {
    struct ItemStruct as MyItemStruct {
        #[mock_syn(skip)]
        skipped: Vec<u64>,
        #[mock_syn(skip(defaulted_skip))]
        defaulted_skip: u32,
    }
}

fn defaulted_skip() -> u32 {
    9
}

#[test]
fn test() {
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
}
