
#[derive(Copy, Clone, Debug)]
pub struct Location {
    pub line: usize,
    pub col: usize,
}

impl Location {
    pub fn new() -> Self {
        Self {
            line: 1,
            col: 1,
        }
    }
    pub fn update(self, input: &str) -> (Self, Self) {
        let (col_inc, line_inc) = input
            .as_bytes()
            .iter()
            .fold((0,0), |(c, l),&i| {
                if i == b'\n' {
                    (1, l+1)
                }else {
                    (c+1, l)
                }
            });
        (Location{line: self.line+line_inc, col: if line_inc == 0 {self.col+col_inc} else {col_inc}},
            Location{line: line_inc, col: col_inc})
    }
}

impl Default for Location {
    fn default() -> Self {
        Self::new()
    }
}

#[test]
fn test_loc() {
    let s = "abc \n bcd";
    let loc = Location::new();
    let ret = loc.update(s);
    println!("{},{}", ret.1.line, ret.1.col);
}
