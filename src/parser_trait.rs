use std::ops::{Mul, Shl, Shr, BitOr};
use regex::Regex;

use crate::location::Location;

#[derive(Clone, Copy)]
pub struct Token<'a>(pub &'a str);
#[derive(Clone, Copy)]
pub struct ParseRegex<'a>(pub &'a str);

#[derive(Clone, Copy)]
pub struct Try<A>(pub A);
#[derive(Clone, Copy)]
pub struct Many<'a, A>(pub A, pub Option<&'a str>);

#[derive(Clone, Copy)]
pub struct ParserMap<I, F>(pub I, pub F);
#[derive(Clone, Copy)]
pub struct ParserProduct<A, B>(pub A, pub B);
#[derive(Clone, Copy)]
pub struct ParserLeft<A, B>(pub A, pub B);
#[derive(Clone, Copy)]
pub struct ParserRight<A, B>(pub A, pub B);
#[derive(Clone, Copy)]
pub struct ParserOr<A, B>(pub A, pub B);

pub trait Parser:
    Mul + Shr + Shl + std::marker::Sized + Copy
{
    type Out;
    fn run_with_out(self, input: &str, loc: Location) -> (Result<Self::Out, Location>, &str, Location);
    fn run(self, input: &str) -> Result<Self::Out, Location>
    where
        Self: Sized
    {
        self.run_with_out(input, Location::new()).0
    }
    fn map<B, F>(self, f: F) -> ParserMap<Self, F>
    where
        F: Fn(Self::Out) -> B,
        Self: Sized
    {
        ParserMap(self, f)
    }

    /// for a Parser<Out = A> and Parser<Out = B>, get Parser<Out = (A,B)>
    /// same as *
    fn zip<B>(self, rhs: B) -> ParserProduct<Self, B> {
        ParserProduct(self, rhs)
    }
    /// throw rhs
    /// same as <<
    fn left<B>(self, rhs: B) -> ParserLeft<Self, B> {
        ParserLeft(self, rhs)
    }
    /// throw self
    /// same as >>
    fn right<B>(self, rhs: B) -> ParserRight<Self, B> {
        ParserRight(self, rhs)
    }
}

impl<'a> Parser for Token<'a> {
    type Out = &'a str;

    fn run_with_out(self, input: &str, loc: Location) -> (Result<Self::Out, Location>, &str, Location) {
        if let Some(o) = input.strip_prefix(&self.0) {
            let loc_parse = loc.update(self.0);
            (Ok(self.0), o, loc_parse.0)
        } else {
            (Err(loc), input, loc)
        }
    }
}

impl<'a> Parser for ParseRegex<'a> {
    type Out = String;//&'a str;TODO:

    fn run_with_out(self, input: &str, loc: Location) -> (Result<Self::Out, Location>, &str, Location) {
        let re = Regex::new(self.0).unwrap();
        let cap = re.find(input).map(|x| x.as_str());
        let o = cap.and_then(|x| input.strip_prefix(x));
        match o {
            Some(output) => {
                let loc_parse = loc.update(cap.unwrap());
                (cap.map(|x| x.to_string()).ok_or(loc_parse.0), output, loc_parse.0)
            },
            None => (Err(loc), input, loc)
        }
    }
}

impl<A: Parser> Parser for Try<A> {
    type Out = Option<A::Out>;

    fn run_with_out(self, input: &str, loc: Location) -> (Result<Self::Out, Location>, &str, Location) {
        let (out, next_str, loc_parse) = self.0.run_with_out(input, loc);
        match out {
            Ok(x) => (Ok(Some(x)),next_str, loc_parse),
            Err(_) => (Ok(None), input, loc),
        }
    }
}

impl<'a, A: Parser + Copy> Parser for Many<'a, A> {
    type Out = Vec<A::Out>;

    fn run_with_out(self, input: &str, loc: Location) -> (Result<Self::Out, Location>, &str, Location) {
        let mut ret = Vec::new();
        let mut text = input;
        let mut loc_parse = loc;
        loop {
            let parse = self.0.run_with_out(text, loc_parse);
            match parse.0 {
                Ok(item) => {
                    ret.push(item);
                    match self.1 {
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
    }
}

impl<B, I: Parser, F> Parser for ParserMap<I, F>
where
    F: Fn(I::Out) -> B + Copy
{
    type Out = B;

    fn run_with_out(self, input: &str, loc: Location) -> (Result<Self::Out, Location>, &str, Location) {
        let (o, s, loc_parse) = self.0.run_with_out(input, loc);
        (o.map(self.1), s, loc_parse)
    }
}

impl<A: Parser, B: Parser> Parser for ParserProduct<A, B> {
    type Out = (A::Out, B::Out);

    fn run_with_out(self, input: &str, loc: Location) -> (Result<Self::Out, Location>, &str, Location) {
        let (lefto, lefts, loc_left) = self.0.run_with_out(input, loc);
        let (righto, rights, loc_right) = self.1.run_with_out(lefts, loc_left);
        (lefto.and_then(|x| righto.map(|y| (x,y))), rights, loc_right)
        //(self.0.run(input), self.1.run())
    }    
}

impl<A: Parser, B: Parser> Parser for ParserLeft<A, B> {
    type Out = A::Out;

    fn run_with_out(self, input: &str, loc: Location) -> (Result<Self::Out, Location>, &str, Location) {
        let (lefto, lefts, loc_left) = self.0.run_with_out(input, loc);
        let (righto, rights, loc_right) = self.1.run_with_out(lefts, loc_left);
        match lefto {
            Ok(l) => {
                match righto {
                    Ok(_) => (Ok(l), rights, loc_right),
                    Err(e) => (Err(e), input, loc)
                }
            }
            Err(e) => (Err(e), input, loc)
        }
    }
}

impl<A: Parser, B: Parser> Parser for ParserRight<A, B> {
    type Out = B::Out;

    fn run_with_out(self, input: &str, loc: Location) -> (Result<Self::Out, Location>, &str, Location) {
        let (lefto, lefts, loc_left) = self.0.run_with_out(input, loc);
        let (righto, rights, loc_right) = self.1.run_with_out(lefts, loc_left);
        match lefto {
            Ok(_) => {
                match righto {
                    Ok(r) => (Ok(r), rights, loc_right),
                    Err(e) => (Err(e), input, loc)
                }
            }
            Err(e) => (Err(e), input, loc)
        }
    }
}

impl<O, A: Parser<Out = O>, B: Parser<Out = O>> Parser for ParserOr<A, B> {
    type Out = O;

    fn run_with_out(self, input: &str, loc: Location) -> (Result<Self::Out, Location>, &str, Location) {
        let (lefto, lefts, loc_left) = self.0.run_with_out(input, loc);
        let ro = self.1.run_with_out(input, loc_left);
        match lefto {
            Ok(l) => (Ok(l), lefts, loc_left),
            Err(_) => ro,
        }
    }
}

macro_rules! op_impl {
    ($($t:ty,$tr1:ty,$tr2:ident,$tt1:ty,$tt2:ident)*) => {$(
        impl<'a, B: Parser> $tr1 for $t
        where
            Self: Sized
        {
            type Output = $tt1;

            fn $tr2(self, rhs: B) -> Self::Output
            where
                Self: Sized
            {
                $tt2(self, rhs)
            }
        }
    )*};
    ($($i1:ident,$t:ty,$tr1:ty,$tr2:ident,$tt1:ty,$tt2:ident)*) => {$(
        impl<$i1, B: Parser> $tr1 for $t
        where
            Self: Sized
        {
            type Output = $tt1;

            fn $tr2(self, rhs: B) -> Self::Output
            where
                Self: Sized
            {
                $tt2(self, rhs)
            }
        }
    )*};
    ($($i1:ident,$i2:ident,$t:ty,$tr1:ty,$tr2:ident,$tt1:ty,$tt2:ident)*) => {$(
        impl<$i1, $i2, B: Parser> $tr1 for $t
        where
            Self: Sized
        {
            type Output = $tt1;

            fn $tr2(self, rhs: B) -> Self::Output
            where
                Self: Sized
            {
                $tt2(self, rhs)
            }
        }
    )*};
}

macro_rules! ops_impl {
    ($($t:ty)+) => {$(
        op_impl!($t, Mul<B>, mul, ParserProduct<Self, B>, ParserProduct);
        op_impl!($t, Shr<B>, shr, ParserRight<Self, B>, ParserRight);
        op_impl!($t, Shl<B>, shl, ParserLeft<Self, B>, ParserLeft);
        op_impl!($t, BitOr<B>, bitor, ParserOr<Self, B>, ParserOr);
    )*};
    ($($i1:ident,$t:ty)*) => {$(
        op_impl!($i1, $t, Mul<B>, mul, ParserProduct<Self, B>, ParserProduct);
        op_impl!($i1, $t, Shr<B>, shr, ParserRight<Self, B>, ParserRight);
        op_impl!($i1, $t, Shl<B>, shl, ParserLeft<Self, B>, ParserLeft);
        op_impl!($i1, $t, BitOr<B>, bitor, ParserOr<Self, B>, ParserOr);
    )*};
    ($($i1:ident,$i2:ident,$t:ty)*) => {$(
        op_impl!($i1, $i2, $t, Mul<B>, mul, ParserProduct<Self, B>, ParserProduct);
        op_impl!($i1, $i2, $t, Shr<B>, shr, ParserRight<Self, B>, ParserRight);
        op_impl!($i1, $i2, $t, Shl<B>, shl, ParserLeft<Self, B>, ParserLeft);
        op_impl!($i1, $i2, $t, BitOr<B>, bitor, ParserOr<Self, B>, ParserOr);
    )*};
}

macro_rules! opmany_impl {
    ($($i1:ident,$t:ty,$tr1:ty,$tr2:ident,$tt1:ty,$tt2:ident)*) => {$(
        impl<'a, $i1, B: Parser> $tr1 for $t
        where
            Self: Sized
        {
            type Output = $tt1;

            fn $tr2(self, rhs: B) -> Self::Output
            where
                Self: Sized
            {
                $tt2(self, rhs)
            }
        }
    )*};
}

ops_impl!(Token<'a>);
ops_impl!(ParseRegex<'a>);
ops_impl!(A, Try<A>);
ops_impl!(I, F, ParserMap<I, F>);
ops_impl!(I, F, ParserProduct<I, F>);
ops_impl!(I, F, ParserLeft<I, F>);
ops_impl!(I, F, ParserRight<I, F>);
ops_impl!(I, F, ParserOr<I, F>);

opmany_impl!(A, Many<'a,A>, Mul<B>, mul, ParserProduct<Self, B>, ParserProduct);
opmany_impl!(A, Many<'a,A>, Shr<B>, shr, ParserRight<Self, B>, ParserRight);
opmany_impl!(A, Many<'a,A>, Shl<B>, shl, ParserLeft<Self, B>, ParserLeft);
