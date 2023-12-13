use std::{fs::File, io::Read, path::PathBuf};

use crate::ir::{ops::DataType, parser::data_type};

#[test]
fn data_type_test() {
    assert!(matches!(data_type("i64"), Ok(("", DataType::I64))));
    assert!(matches!(data_type("f64"), Ok(("", DataType::F64))));
    assert!(matches!(data_type("bool"), Ok(("", DataType::Bool))));
    assert!(matches!(data_type("void"), Ok(("", DataType::Void))));
    assert!(matches!(data_type("[i64,5]"), 
        Ok(("", DataType::Array(b, 5))) if matches!(*b, DataType::I64)));
    let complex = data_type("{i64,[bool,3],{{i64}}}");
    assert!(matches!(
        complex,
        Ok(("", DataType::Struct(v))) if matches!(
            &v[..],
            [DataType::I64,
             DataType::Array(b, 3),
             DataType::Struct(c)
            ] if matches!(
                **b, DataType::Bool
            ) && matches!(
                &c[..], [DataType::Struct(e)] if matches!(
                    &e[..], [DataType::I64]
                )
            )
        )
    ))
}
