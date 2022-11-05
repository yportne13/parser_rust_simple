pub mod parser_trait;
pub mod location;
pub mod parse_tools;
pub mod parser;
pub mod prelude;
mod example;
mod example2;

#[cfg(test)]
mod tests {
    use crate::prelude::*;

    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
        let s = "abc \n def";
        let parse = DontConsume(Token("abc \n def")) * (token("abc") >> Token("def"));
        println!("{:?}", parse.run(s))
    }
}

#[test]
fn test() {
    use location::Location;
    use parser::*;

    let input = "abcdef";
    let parse = token("abc") * token("def");
    println!("{:?}", parse.run_with_out(input, Location { line: 1, col: 1 }))

}

#[test]
fn rec() {
    use prelude::*;
    let input = "[[[0]]]";
    fn zero<'a>() -> impl Parser<Out = &'a str> {
        token("0")
    }
    fn array<'a>() -> impl Parser<Out = &'a str> {
        token("[").right(
            zero()
                //.or(tobox(|input, loc| array().run_with_out(input, loc)))
                .or(tobox!(array()))
        ) << token("]")
    }
    let parser = array();
    println!("{:?}", parser.run(input))
}

#[test]
fn rec2() {
    use parser::*;
    let input = "[[[0]]]";
    fn zero<'a>() -> Parser<'a, &'a str> {
        token("0")
    }
    fn array<'a>() -> Parser<'a, &'a str> {
        token("[") >> (zero() | call(array)) << token("]")
    }
    let parser = array();
    println!("{:?}", parser.run(input))
}
