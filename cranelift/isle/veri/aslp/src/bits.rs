use anyhow::{bail, Result};

#[derive(Clone)]
pub struct Bits {
    pub segments: Vec<Segment>,
}

impl Bits {
    pub fn empty() -> Self {
        Bits {
            segments: Vec::new(),
        }
    }

    pub fn from_u32(x: u32) -> Self {
        Bits {
            segments: vec![Segment::from_u32(x)],
        }
    }

    pub fn width(&self) -> usize {
        self.segments.iter().map(|s| s.width()).sum()
    }

    pub fn splice(base: &Bits, insert: &Bits, offset: usize) -> Result<Bits> {
        let mut result = Bits::empty();
        if offset > 0 {
            let prefix = base.extract(0, offset)?;
            result.append(prefix);
        }
        result.append(insert.clone());
        if result.width() < base.width() {
            let suffix = base.extract(result.width(), base.width())?;
            result.append(suffix);
        }
        Ok(result)
    }

    pub fn append(&mut self, other: Bits) {
        self.segments.extend(other.segments);
    }

    pub fn extract(&self, lo: usize, hi: usize) -> Result<Bits> {
        let mut result = Bits::empty();
        let mut offset = 0usize;
        for segment in &self.segments {
            // Intersection of this interval with extraction interval.
            let start = std::cmp::max(lo, offset);
            let end = std::cmp::min(hi, offset + segment.width());

            // If the intersection is non-empty, add a segment.
            if start < end {
                result
                    .segments
                    .push(segment.extract(start - offset, end - offset)?);
            }

            // Advance offset.
            offset += segment.width();
        }
        Ok(result)
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

#[derive(Clone)]
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

    pub fn extract(&self, lo: usize, hi: usize) -> Result<Segment> {
        match *self {
            Segment::Symbolic(_, w) => {
                if !(lo == 0 && hi == w) {
                    bail!("symbolic segments must remain whole");
                }
                Ok(self.clone())
            }
            Segment::Constant(c, w) => {
                if !(lo < hi && hi <= w) {
                    bail!("invalid extraction interval");
                }
                let w = hi - lo;
                let mask = (1 << w) - 1;
                Ok(Segment::Constant((c >> lo) & mask, w))
            }
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
