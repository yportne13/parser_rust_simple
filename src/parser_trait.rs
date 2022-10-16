use std::ops::{Mul, Shl, Shr, BitOr};
use regex::Regex;

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
        match o {
            Some(output) => (cap.map(|x| x.to_string()), output),
            None => (None, input)
        }
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

impl<O, A: Parser<Out = O>, B: Parser<Out = O>> Parser for ParserOr<A, B> {
    type Out = O;

    fn run_with_out(self, input: &str) -> (Option<Self::Out>, &str) {
        let (lefto, lefts) = self.0.run_with_out(input);
        let ro = self.1.run_with_out(input);
        match lefto {
            Some(l) => (Some(l), lefts),
            None => ro,
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
