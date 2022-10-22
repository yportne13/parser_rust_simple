pub mod parser_trait;
pub mod location;
pub mod parse_tools;
pub mod prelude;

#[cfg(test)]
mod tests {
    use crate::prelude::*;

    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
        let s = "abc \n def";
        let parse = token("abc") >> Token("def");
        println!("{:?}", parse.run(s))
    }
}
