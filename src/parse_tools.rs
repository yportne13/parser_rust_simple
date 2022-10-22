use crate::parser_trait::*;

/// parse white space
pub fn whitespace<'a>() -> ParseRegex<'a> {
    ParseRegex(r"\s*")
}

pub fn token(s: &'_ str) -> ParserLeft<Token<'_>, ParseRegex<'_>> {
    Token(s) << ParseRegex(r"\s*")
}

pub fn int() -> impl Parser<Out = i64> {
    ParseRegex(r"[-+]?[0-9]+").map(|x| x.parse::<i64>().unwrap())
}

pub fn float() -> impl Parser<Out = f64> {
    ParseRegex(r"[-+]?([0-9]*\.)?[0-9]+([eE][-+]?[0-9]+)?").map(|x| x.parse::<f64>().unwrap())
}

pub fn escaped_quoted() -> impl Parser<Out = String> {
    Token("\"") >> ParseRegex(r#"([^"]*)"#) << Token("\"")
}


#[test]
fn test() {
    let input = "   abc";
    let parser = whitespace() >> Token("abc");
    println!("{:?}", parser.run(input));
    let input = "abc   def";
    let parser = token("abc") << Token("def");
    println!("{:?}", parser.run(input));
    let input = "data: 2992";
    let parser = token("data:") >> int();
    println!("{:?}", parser.run(input));
    let input = "data: 3.1415";
    let parser = token("data:") >> float();
    println!("{:?}", parser.run(input));
    let input = r#"a string: "abc""#;
    let parser = token("a string:") >> escaped_quoted();
    println!("{:?}", parser.run(input));
}
