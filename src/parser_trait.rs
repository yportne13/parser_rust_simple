use std::ops::{Mul, Shl, Shr};
use regex::Regex;

pub struct Token<'a>(pub &'a str);
#[derive(Clone, Copy)]
pub struct ParseRegex<'a>(pub &'a str);

pub struct Try<A>(pub A);
pub struct Many<'a, A>(pub A, pub Option<&'a str>);

pub struct ParserMap<I, F>(pub I, pub F);
pub struct ParserProduct<A, B>(pub A, pub B);
pub struct ParserLeft<A, B>(pub A, pub B);
pub struct ParserRight<A, B>(pub A, pub B);

pub trait Parser:
    Mul + Shr + Shl + std::marker::Sized
{
    type Out;
    fn run_with_out(self, input: &str) -> (Option<Self::Out>, &str);
    fn run(self, input: &str) -> Option<Self::Out>
    where
        Self: Sized
    {
        self.run_with_out(input).0
    }
    fn map<B, F>(self, f: F) -> ParserMap<Self, F>
    where
        F: Fn(Self::Out) -> B,
        Self: Sized
    {
        ParserMap(self, f)
    }
}

impl<'a> Parser for Token<'a> {
    type Out = &'a str;

    fn run_with_out(self, input: &str) -> (Option<Self::Out>, &str) {
        if let Some(o) = input.strip_prefix(&self.0) {
            (Some(self.0), o)
        } else {
            (None, input)
        }
    }
}

impl<'a> Parser for ParseRegex<'a> {
    type Out = String;//&'a str;TODO:

    fn run_with_out(self, input: &str) -> (Option<Self::Out>, &str) {
        let re = Regex::new(self.0).unwrap();
        let cap = re.find(input).map(|x| x.as_str());
        let o = cap.and_then(|x| input.strip_prefix(x));
        (cap.map(|x| x.to_string()), o.unwrap_or(input))
    }
}

impl<A: Parser> Parser for Try<A> {
    type Out = Option<A::Out>;

    fn run_with_out(self, input: &str) -> (Option<Self::Out>, &str) {
        let (out, next_str) = self.0.run_with_out(input);
        match out {
            Some(x) => (Some(Some(x)),next_str),
            None => (Some(None), input),
        }
    }
}

impl<'a, A: Parser + Copy> Parser for Many<'a, A> {
    type Out = Vec<A::Out>;

    fn run_with_out(self, input: &str) -> (Option<Self::Out>, &str) {
        let mut ret = Vec::new();
        let mut text = input;
        loop {
            let parse = self.0.run_with_out(text);
            match parse.0 {
                Some(item) => {
                    ret.push(item);
                    match self.1 {
                        Some(sep) => {
                            match parse.1.strip_prefix(sep) {
                                Some(t) => text = t,
                                None => {text = parse.1; break},
                            }
                        },
                        None => text = parse.1,
                    }
                },
                None => break,
            }
        }
        (Some(ret), text)
    }
}

impl<B, I: Parser, F> Parser for ParserMap<I, F>
where
    F: Fn(I::Out) -> B
{
    type Out = B;

    fn run_with_out(self, input: &str) -> (Option<Self::Out>, &str) {
        let (o, s) = self.0.run_with_out(input);
        (o.map(self.1), s)
    }
}

impl<A: Parser, B: Parser> Parser for ParserProduct<A, B> {
    type Out = (A::Out, B::Out);

    fn run_with_out(self, input: &str) -> (Option<Self::Out>, &str) {
        let (lefto, lefts) = self.0.run_with_out(input);
        let (righto, rights) = self.1.run_with_out(lefts);
        (lefto.and_then(|x| righto.map(|y| (x,y))), rights)
        //(self.0.run(input), self.1.run())
    }    
}

impl<A: Parser, B: Parser> Parser for ParserLeft<A, B> {
    type Out = A::Out;

    fn run_with_out(self, input: &str) -> (Option<Self::Out>, &str) {
        let (lefto, lefts) = self.0.run_with_out(input);
        let (righto, rights) = self.1.run_with_out(lefts);
        match lefto {
            Some(l) => {
                match righto {
                    Some(_) => (Some(l), rights),
                    None => (None, input)
                }
            }
            None => (None,input)
        }
    }
}

impl<A: Parser, B: Parser> Parser for ParserRight<A, B> {
    type Out = B::Out;

    fn run_with_out(self, input: &str) -> (Option<Self::Out>, &str) {
        let (lefto, lefts) = self.0.run_with_out(input);
        let (righto, rights) = self.1.run_with_out(lefts);
        match lefto {
            Some(_) => {
                match righto {
                    Some(r) => (Some(r), rights),
                    None => (None, input)
                }
            }
            None => (None,input)
        }
    }
}

macro_rules! shr_impl {
    ($($t:ty)*) => {$(
        impl<'a, B: Parser> Shr<B> for $t
        where
            Self: Sized
        {
            type Output = ParserRight<Self, B>;

            fn shr(self, rhs: B) -> Self::Output
            where
                Self: Sized
            {
                ParserRight(self, rhs)
            }
        }
    )*};
    ($($i1:ident,$t:ty)*) => {$(
        impl<$i1, B: Parser> Shr<B> for $t
        where
            Self: Sized
        {
            type Output = ParserRight<Self, B>;

            fn shr(self, rhs: B) -> Self::Output
            where
                Self: Sized
            {
                ParserRight(self, rhs)
            }
        }
    )*};
    ($($i1:ident,$i2:ident,$t:ty)*) => {$(
        impl<$i1, $i2, B: Parser> Shr<B> for $t
        where
            Self: Sized
        {
            type Output = ParserRight<Self, B>;

            fn shr(self, rhs: B) -> Self::Output
            where
                Self: Sized
            {
                ParserRight(self, rhs)
            }
        }
    )*}
}

macro_rules! shl_impl {
    ($($t:ty)*) => {$(
        impl<'a, B: Parser> Shl<B> for $t
        where
            Self: Sized
        {
            type Output = ParserLeft<Self, B>;

            fn shl(self, rhs: B) -> Self::Output
            where
                Self: Sized
            {
                ParserLeft(self, rhs)
            }
        }
    )*};
    ($($i1:ident,$t:ty)*) => {$(
        impl<$i1, B: Parser> Shl<B> for $t
        where
            Self: Sized
        {
            type Output = ParserLeft<Self, B>;

            fn shl(self, rhs: B) -> Self::Output
            where
                Self: Sized
            {
                ParserLeft(self, rhs)
            }
        }
    )*};
    ($($i1:ident,$i2:ident,$t:ty)*) => {$(
        impl<$i1, $i2, B: Parser> Shl<B> for $t
        where
            Self: Sized
        {
            type Output = ParserLeft<Self, B>;

            fn shl(self, rhs: B) -> Self::Output
            where
                Self: Sized
            {
                ParserLeft(self, rhs)
            }
        }
    )*}
}

macro_rules! mul_impl {
    ($($t:ty)*) => {$(
        impl<'a, B: Parser> Mul<B> for $t
        where
            Self: Sized
        {
            type Output = ParserProduct<Self, B>;

            fn mul(self, rhs: B) -> Self::Output
            where
                Self: Sized
            {
                ParserProduct(self, rhs)
            }
        }
    )*};
    ($($i1:ident,$t:ty)*) => {$(
        impl<$i1, B: Parser> Mul<B> for $t
        where
            Self: Sized
        {
            type Output = ParserProduct<Self, B>;

            fn mul(self, rhs: B) -> Self::Output
            where
                Self: Sized
            {
                ParserProduct(self, rhs)
            }
        }
    )*};
    ($($i1:ident,$i2:ident,$t:ty)*) => {$(
        impl<$i1, $i2, B: Parser> Mul<B> for $t
        where
            Self: Sized
        {
            type Output = ParserProduct<Self, B>;

            fn mul(self, rhs: B) -> Self::Output
            where
                Self: Sized
            {
                ParserProduct(self, rhs)
            }
        }
    )*}
}

macro_rules! ops_impl {
    ($($t:ty)+) => {$(
        shr_impl!($t);
        shl_impl!($t);
        mul_impl!($t);
    )*};
    ($($i1:ident,$t:ty)*) => {$(
        shr_impl!($i1, $t);
        shl_impl!($i1, $t);
        mul_impl!($i1, $t);
    )*};
    ($($i1:ident,$i2:ident,$t:ty)*) => {$(
        shr_impl!($i1, $i2, $t);
        shl_impl!($i1, $i2, $t);
        mul_impl!($i1, $i2, $t);
    )*};
}

macro_rules! op_impl {
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
//ops_impl!('a, A, Many<'a, A>);
ops_impl!(I, F, ParserMap<I, F>);
ops_impl!(I, F, ParserProduct<I, F>);
ops_impl!(I, F, ParserLeft<I, F>);
ops_impl!(I, F, ParserRight<I, F>);

op_impl!(A, Many<'a,A>, Mul<B>, mul, ParserProduct<Self, B>, ParserProduct);
op_impl!(A, Many<'a,A>, Shr<B>, shr, ParserRight<Self, B>, ParserRight);
op_impl!(A, Many<'a,A>, Shl<B>, shl, ParserLeft<Self, B>, ParserLeft);