#[cfg(test)]
mod tests {
    use std::{fs::File, io::Read, path::PathBuf};
    use test_case::test_case;

    use crate::kalk_value::KalkValue;

    fn eval_file(name: &str) -> KalkValue {
        let mut path = PathBuf::new();
        path.push(env!("CARGO_MANIFEST_DIR"));
        path.push("..");
        path.push("tests");
        path.push(name);
        path.set_extension("kalker");

        let mut file_content = String::new();
        File::open(path)
            .unwrap()
            .read_to_string(&mut file_content)
            .unwrap();
        let mut context = crate::parser::Context::new();

        #[cfg(feature = "rug")]
        return crate::parser::eval(&mut context, &file_content, 58)
            .unwrap()
            .unwrap()
            .get_value();

        #[cfg(not(feature = "rug"))]
        crate::parser::eval(&mut context, &file_content)
            .unwrap()
            .unwrap()
            .get_value()
    }

    fn is_true(x: KalkValue) -> bool {
        if let KalkValue::Boolean(boolean) = x {
            boolean
        } else {
            false
        }
    }

    #[test_case("ambiguities/comparison_in_function")]
    #[test_case("ambiguities/fn_call_no_parenthesis")]
    #[test_case("basics")]
    #[test_case("comparisons")]
    #[test_case("comprehensions")]
    #[test_case("equations")]
    #[test_case("derivation")]
    #[test_case("functions")]
    #[test_case("groups")]
    #[test_case("integration")]
    #[test_case("matrices/operations")]
    #[test_case("matrices/transpose")]
    #[test_case("radix")]
    #[test_case("recursion")]
    #[test_case("redefining")]
    #[test_case("sum")]
    #[test_case("variables")]
    #[test_case("vectors")]
    fn test_file(name: &str) {
        assert!(is_true(eval_file(name)));
    }
}
