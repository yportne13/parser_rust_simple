use std::ops::{Mul, Shl, Shr, BitOr};
use regex::Regex;
use crate::location::Location;

type ParseFunction<'a, A> = dyn Fn(&'a str, Location) -> (Result<A, Location>, &'a str, Location) + 'a;
pub struct Parser<'a, A>(pub Box<ParseFunction<'a, A>>);

impl<'a, A: 'a> Parser<'a, A> {

    pub fn new<F>(f: F) -> Self
    where
        F: Fn(&'a str, Location) -> (Result<A, Location>, &'a str, Location) + 'a
    {
        Self(Box::new(f))
    }    
    pub fn run_with_out(&self, s: &'a str, loc: Location) -> (Result<A, Location>, &'a str, Location) {
        self.0(s, loc)
    }
    pub fn run(&self, s: &'a str) -> Result<A, Location> {
        self.0(s, Location::new()).0
    }
    pub fn map<F: 'a, B: 'a>(self, f: F) -> Parser<'a, B>
    where
        F: Fn(A) -> B + Copy
    {
        Parser(Box::new(move |input, loc| {
            let ret = self.run_with_out(input, loc);
            (ret.0.map(f), ret.1, ret.2)
        }))
    }

}

impl<'a, A: 'a, B: 'a> Mul<Parser<'a, B>> for Parser<'a, A> {

    type Output = Parser<'a, (A, B)>;

    fn mul(self, rhs: Parser<'a, B>) -> Self::Output {
        Parser(Box::new(
            move |input: &'a str, loc: Location| {
                let oa = self.run_with_out(input, loc);
                match oa.0 {
                    Ok(a) => {
                        let ob = rhs.run_with_out(oa.1, oa.2);
                        (ob.0.map(|b| (a,b)), ob.1, ob.2)
                    }
                    Err(e) => {
                        (Err(e), oa.1, oa.2)
                    }
                }
            }
        ))
    }
}

impl<'a, A: 'a, B: 'a> Shl<Parser<'a, B>> for Parser<'a, A> {
    type Output = Parser<'a, A>;

    fn shl(self, rhs: Parser<'a, B>) -> Self::Output {
        Parser(Box::new(move |input, loc| {
            let (lefto, lefts, loc_left) = self.run_with_out(input, loc);
            match lefto {
                Ok(l) => {
                    let (righto, rights, loc_right) = rhs.run_with_out(lefts, loc_left);
                    match righto {
                        Ok(_) => (Ok(l), rights, loc_right),
                        Err(e) => (Err(e), input, loc)
                    }
                }
                Err(e) => (Err(e), input, loc)
            }
        }))
    }
}

impl<'a, A: 'a, B: 'a> Shr<Parser<'a, B>> for Parser<'a, A> {
    type Output = Parser<'a, B>;

    fn shr(self, rhs: Parser<'a, B>) -> Self::Output {
        Parser(Box::new(move |input: &str, loc: Location| {
            let (lefto, lefts, loc_left) = self.run_with_out(input, loc);
            match lefto {
                Ok(_) => {
                    let (righto, rights, loc_right) = rhs.run_with_out(lefts, loc_left);
                    match righto {
                        Ok(r) => (Ok(r), rights, loc_right),
                        Err(e) => (Err(e), input, loc)
                    }
                }
                Err(e) => (Err(e), input, loc)
            }
        }))
    }
}

impl<'a, A: 'a> BitOr<Parser<'a, A>> for Parser<'a, A> {
    type Output = Parser<'a, A>;

    fn bitor(self, rhs: Parser<'a, A>) -> Self::Output {
        Parser(Box::new(move |input, loc| {
            let (lefto, lefts, loc_left) = self.run_with_out(input, loc);
            match lefto {
                Ok(l) => (Ok(l), lefts, loc_left),
                Err(_) => rhs.run_with_out(input, loc_left),
            }
        }))
    }
}

pub fn regex(s: &str) -> Parser<&str> {
    Parser(Box::new(move |input: &str, loc: Location| {
        let re = Regex::new(s).unwrap();
        let cap = re.find(input).map(|x| x.as_str());
        let o = cap.and_then(|x| input.strip_prefix(x));
        match o {
            Some(output) => {
                let loc_parse = loc.update(cap.unwrap());
                (cap.ok_or(loc_parse.0), output, loc_parse.0)
            },
            None => (Err(loc), input, loc)
        }
    }))
}

pub fn token_single(s: &str) -> Parser<&str> {
    Parser(Box::new(move |input: &str, loc: Location| {
        if let Some(o) = input.strip_prefix(s) {
            let loc_parse = loc.update(s);
            (Ok(s), o, loc_parse.0)
        } else {
            (Err(loc), input, loc)
        }
    }))
}

pub fn totry<'a, A: 'a>(p: Parser<'a, A>) -> Parser<'a, Option<A>> {
    Parser(Box::new(move |input, loc| {
        let (out, next_str, loc_parse) = p.run_with_out(input, loc);
        (Ok(out.ok()), next_str, loc_parse)
    }))
}

pub fn dont_consume<'a, A: 'a>(p: Parser<'a, A>) -> Parser<'a, A> {
    Parser(Box::new(move |input, loc| {
        let (out, _, _) = p.run_with_out(input, loc);
        (out, input, loc)
    }))
}

pub fn many<'a, A: 'a>(p: Parser<'a, A>, sep: Option<&'a str>) -> Parser<'a, Vec<A>> {
    Parser(Box::new(move |input, loc| {
        let mut ret = Vec::new();
        let mut text = input;
        let mut loc_parse = loc;
        loop {
            let parse = p.run_with_out(text, loc_parse);
            match parse.0 {
                Ok(item) => {
                    ret.push(item);
                    match sep {
                        Some(sep) => {
                            match parse.1.strip_prefix(sep) {
                                Some(t) => {
                                    text = t;
                                    let loc_parse_sep = parse.2.update(sep);
                                    loc_parse = loc_parse_sep.0;
                                },
                                None => {
                                    text = parse.1;
                                    loc_parse = parse.2;
                                    break
                                },
                            }
                        },
                        None => {
                            text = parse.1;
                            loc_parse = parse.2;
                        },
                    }
                },
                Err(_) => break,
            }
        }
        (Ok(ret), text, loc_parse)
    }))
}

pub fn token(s: &str) -> Parser<&str> {
    token_single(s) << whitespace()
}

pub fn call<'a, A: 'a, F>(parser_factory: F) -> Parser<'a, A>
where
	F: Fn() -> Parser<'a, A> + 'a,
{
	Parser::new(move |input, loc| {
		let parser = parser_factory();
		parser.run_with_out(input, loc)
	})
}

pub fn float<'a>() -> Parser<'a, f64> {
    regex(r"[-+]?([0-9]*\.)?[0-9]+([eE][-+]?[0-9]+)?").map(|x| x.parse::<f64>().unwrap())
}

pub fn escaped_quoted<'a>() -> Parser<'a, &'a str> {
    token_single("\"") >> regex(r#"([^"]*)"#) << token_single("\"")
}

pub fn whitespace<'a>() -> Parser<'a, &'a str> {
    regex(r"\s*")
}
