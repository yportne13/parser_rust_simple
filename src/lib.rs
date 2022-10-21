pub mod parser_trait;
pub mod location;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
        let s = "abc \n def";
        let o = s.as_bytes().iter().fold((0,0), |(c, l),&i| {
            if i == b'\n' {
                (0, l+1)
            }else {
                (c+1, l)
            }
        });
        println!("{:?}", o)
    }
}
