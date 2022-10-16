use std::collections::HashMap;
use parser_rust_simple::parser_trait::*;

#[derive(Debug)]
enum JSON {
    JNull,
    JString(String),
    JNumber(f64),
    JBool(bool),
    JArray(Vec<JSON>),
    JObject(HashMap<String, JSON>)
}

fn json_parser(input: &str) -> (Option<f64>, &str) {
    let whitespace = ParseRegex(r"\s*");
    let head = whitespace >> Token("{") << whitespace;
    let tail = whitespace >> Token("}") << whitespace;
    let double = ParseRegex(r"[-+]?([0-9]*\.)?[0-9]+([eE][-+]?[0-9]+)?").map(|x| x.parse::<f64>().unwrap());
    let escaped_quoted = Token("\"") >> ParseRegex(r#"([^"]*)"#) << Token("\"");
    
    let ret = head >> double << tail;
    //ret.map(|x| JSON::JNumber(x)).run(input)
    ret.run_with_out(input)
}

fn main() {
    println!("Hello, world!");
    let input = r#"
{
  "Company name" : "Microsoft Corporation",
  "Ticker"  : "MSFT",
  "Active"  : true,
  "Price"   : 30.66,
  "Shares outstanding" : 8.38e9,
  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
}
"#;
    let test = r#"
    {
        30.66
    }
    "#;
    let ret = json_parser(test);
    println!("{:?}", ret)
}
