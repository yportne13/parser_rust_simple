use std::collections::HashMap;
use parser_rust_simple::parser_trait::*;
use parser_rust_simple::location::*;

#[derive(Debug)]
enum JSON {
    JNull,
    JString(String),
    JNumber(f64),
    JBool(bool),
    JArray(Vec<JSON>),
    JObject(HashMap<String, JSON>)
}

fn json_parser(input: &str) -> (Option<JSON>, &str, Location) {
    let whitespace = ParseRegex(r"\s*");
    let head = whitespace >> Token("{") << whitespace;
    let tail = whitespace >> Token("}") << whitespace;
    let double = ParseRegex(r"[-+]?([0-9]*\.)?[0-9]+([eE][-+]?[0-9]+)?").map(|x| x.parse::<f64>().unwrap());
    let escaped_quoted = Token("\"") >> ParseRegex(r#"([^"]*)"#) << Token("\"");
    
    let lit_temp =
        (Token("null") << whitespace).map(|_| JSON::JNull) |
        (double << whitespace).map(JSON::JNumber) |
        (escaped_quoted << whitespace).map(JSON::JString) |
        (Token("true") << whitespace).map(|_| JSON::JBool(true)) |
        (Token("false") << whitespace).map(|_| JSON::JBool(false));
    let lit = whitespace >> lit_temp << whitespace;
    let array = 
        (whitespace >> Token("[") << whitespace) >>
        Many(lit, Some(",")).map(JSON::JArray)//TODO: lit => value
        << (whitespace >> Token("]") << whitespace);
    let value_temp =
        whitespace >> ((escaped_quoted << whitespace << Token(":")) *
        (lit | array)) << whitespace;//TODO: (lit | array) => (lit | array | obj)
    let value = whitespace >> value_temp << whitespace;
    let obj =
        head >>
        Many(value, Some(",")).map(|x| JSON::JObject(x.into_iter().collect::<HashMap<String, JSON>>()))
        << tail;

    obj.run_with_out(input, Location::new())
}

fn main() {
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
    let ret = json_parser(input);
    println!("{:?}", ret.0);
    println!("{:?}", ret.1);
    println!("{:?}", ret.2);
}
