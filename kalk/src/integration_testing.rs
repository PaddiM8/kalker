#[cfg(test)]
mod tests {
    use std::{fs::File, io::Read, path::PathBuf};

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

    #[test]
    fn test_basics() {
        assert!(is_true(eval_file("basics")));
    }

    #[test]
    fn test_radix() {
        assert!(is_true(eval_file("radix")));
    }

    #[test]
    fn test_variables() {
        assert!(is_true(eval_file("variables")));
    }
}
