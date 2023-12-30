// use serde::Deserialize;
use crate::interpret_file_contents;
extern crate pretty_assertions;
#[allow(unused)]
use self::pretty_assertions::{assert_eq, assert_ne, assert_str_eq};

#[datatest::files("test_data/", {
  // Pattern is defined via `in` operator. Every file from the `directory` above will be matched
  // against this regular expression and every matched file will produce a separate test.
  input in r"^(.*)\.input\.cm",
  // Template defines a rule for deriving dependent file name based on captures of the pattern.
  output = r"${1}.output.txt",
})]
#[test]
fn test_interpret_output(input: &str, output: &str) {
    assert_str_eq!(interpret_file_contents(input).unwrap(), output);
}