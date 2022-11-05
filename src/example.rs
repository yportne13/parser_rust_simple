#![allow(dead_code)]
use std::collections::HashMap;
use crate::{parser::*, prelude::Location};

#[derive(Debug)]
enum JSON {
    JNull,
    JString(String),
    JNumber(f64),
    JBool(bool),
    JArray(Vec<JSON>),
    JObject(HashMap<String, JSON>)
}

fn head<'a>() -> Parser<'a, &'a str> {
    whitespace() >> token("{")
}

fn tail<'a>() -> Parser<'a, &'a str> {
    whitespace() >> token("}")
}

fn lit_temp<'a>() -> Parser<'a, JSON> {
    token("null").map(|_| JSON::JNull) |
        (float() << whitespace()).map(JSON::JNumber) |
        (escaped_quoted() << whitespace()).map(|s| JSON::JString(s.to_string())) |
        token("true").map(|_| JSON::JBool(true)) |
        token("false").map(|_| JSON::JBool(false))
}

fn lit<'a>() -> Parser<'a, JSON> {
    whitespace() >> lit_temp() << whitespace()
}

fn array<'a>() -> Parser<'a, JSON> {
    (whitespace() >> token("[")) >>
        //Many(lit(), Some(",")).map(JSON::JArray)//TODO: lit => value
        many(
            value(),
            Some(",")).map(JSON::JArray)
        << (whitespace() >> token("]"))
}

fn value<'a>() -> Parser<'a, JSON> {
    lit() | call(array) | call(obj)
}

fn key_value<'a>() -> Parser<'a, (&'a str, JSON)> {
    whitespace() >> (
        (escaped_quoted() << whitespace() << token(":")) * value()
        ) << whitespace()
}

fn obj<'a>() -> Parser<'a, JSON> {
    head() >>
        many(key_value(), Some(","))
            .map(|x| JSON::JObject(x.into_iter().map(|x| (x.0.to_string(), x.1)).collect::<HashMap<String, JSON>>()))
    << tail()
}

fn json_parser(input: &str) -> (Result<JSON, Location>, &str, Location) {    
    obj().run_with_out(input, Location::new())
}

#[test]
fn test() {
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
