#![allow(dead_code)]
use std::collections::HashMap;
use crate::parser_trait::*;
use crate::prelude::*;
use crate::tobox;

#[derive(Debug)]
enum JSON {
    JNull,
    JString(String),
    JNumber(f64),
    JBool(bool),
    JArray(Vec<JSON>),
    JObject(HashMap<String, JSON>)
}

fn head<'a>() -> impl Parser<Out = &'a str> {
    whitespace() >> token("{")
}

fn tail<'a>() -> impl Parser<Out = &'a str> {
    whitespace() >> token("}")
}

fn lit_temp() -> impl Parser<Out = JSON> {
    token("null").map(|_| JSON::JNull) |
        float().left(whitespace()).map(JSON::JNumber) |
        escaped_quoted().left(whitespace()).map(JSON::JString) |
        token("true").map(|_| JSON::JBool(true)) |
        token("false").map(|_| JSON::JBool(false))
}

fn lit() -> impl Parser<Out = JSON> {
    whitespace() >> lit_temp()
}

fn array() -> impl Parser<Out = JSON> {
    (whitespace() >> token("[")) >>
        Many(tobox!(value()), Some(",")).map(JSON::JArray)//TODO: lit => value
        << (whitespace() >> token("]"))
}

fn value() -> impl Parser<Out = JSON> {
    lit().or(array()).or(tobox!(obj()))
}

fn key_value() -> impl Parser<Out = (String, JSON)> {
    whitespace() >> ((escaped_quoted().left(whitespace()) << token(":")) *
        value())
}

fn obj() -> impl Parser<Out = JSON> {
    head().right(
        Many(key_value(), Some(",")).map(|x| JSON::JObject(x.into_iter().collect::<HashMap<String, JSON>>()))
    ) << tail()
}

fn json_parser(input: &str) -> (Result<JSON, Location>, &str, Location) {
    obj().run_with_out(input, Location::new())
}

#[test]
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
