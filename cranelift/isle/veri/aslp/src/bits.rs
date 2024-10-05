pub struct Bits {
    pub segments: Vec<Segment>,
}

impl Bits {
    pub fn from_u32(x: u32) -> Self {
        Bits {
            segments: vec![Segment::from_u32(x)],
        }
    }

    pub fn width(&self) -> usize {
        self.segments.iter().map(|s| s.width()).sum()
    }
}

impl std::fmt::Display for Bits {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.segments
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
                .join("|")
        )
    }
}

pub enum Segment {
    Symbolic(String, usize),
    Constant(u32, usize),
}

impl Segment {
    pub fn from_u32(x: u32) -> Self {
        Segment::Constant(x, 32)
    }

    pub fn width(&self) -> usize {
        match self {
            Segment::Symbolic(_, w) | Segment::Constant(_, w) => *w,
        }
    }
}

impl std::fmt::Display for Segment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Segment::Symbolic(s, w) => write!(f, "{s}:{w}"),
            Segment::Constant(c, w) if w % 4 == 0 => {
                write!(f, "0x{c:0>nibbles$x}", nibbles = w / 4)
            }
            Segment::Constant(c, w) => write!(f, "{c:#x}:{w}"),
        }
    }
}
