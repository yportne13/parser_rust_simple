#![allow(dead_code)]
use std::collections::HashMap;
use crate::parser_trait::*;
use crate::prelude::*;
use crate::tobox;

#[derive(Debug)]
enum Json {
    Null,
    String(String),
    Number(f64),
    Bool(bool),
    Array(Vec<Json>),
    Object(HashMap<String, Json>)
}

fn head<'a>() -> impl Parser<Out = &'a str> {
    whitespace() >> token("{")
}

fn tail<'a>() -> impl Parser<Out = &'a str> {
    whitespace() >> token("}")
}

fn lit_temp() -> impl Parser<Out = Json> {
    token("null").map(|_| Json::Null) |
        float().left(whitespace()).map(Json::Number) |
        escaped_quoted().left(whitespace()).map(Json::String) |
        token("true").map(|_| Json::Bool(true)) |
        token("false").map(|_| Json::Bool(false))
}

fn lit() -> impl Parser<Out = Json> {
    whitespace() >> lit_temp()
}

fn array() -> impl Parser<Out = Json> {
    (whitespace() >> token("[")) >>
        Many(tobox!(value()), Some(",")).map(Json::Array)//TODO: lit => value
        << (whitespace() >> token("]"))
}

fn value() -> impl Parser<Out = Json> {
    lit().or(array()).or(tobox!(obj()))
}

fn key_value() -> impl Parser<Out = (String, Json)> {
    whitespace() >> ((escaped_quoted().left(whitespace()) << token(":")) *
        value())
}

fn obj() -> impl Parser<Out = Json> {
    head().right(
        Many(key_value(), Some(",")).map(|x| Json::Object(x.into_iter().collect::<HashMap<String, Json>>()))
    ) << tail()
}

fn json_parser(input: &str) -> (Result<Json, Location>, &str, Location) {
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

    println!("{}", std::mem::size_of_val(&obj()))
}
