use std::fmt;
use std::ops::{Add, AddAssign};

/// ソースコード上の位置。(改行の個数, 最終行の長さ) と考えると、広がりを表しているともいえる。
///
/// (ZERO, +) はモノイドをなす。
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Position {
    /// 行番号。0 から数える。文字列に含まれる改行の個数に一致する。
    pub(crate) line: usize,

    /// 列番号。0 から数える。最後の改行より後にある文字列の長さに一致する。
    /// LSP/DAP の都合に合わせて、UTF-16 基準で数える。
    pub(crate) character: usize,
}

impl Position {
    pub(crate) const ZERO: Position = Position {
        line: 0,
        character: 0,
    };

    pub(crate) fn new(line: usize, character: usize) -> Position {
        Position { line, character }
    }
}

impl From<char> for Position {
    fn from(c: char) -> Position {
        if c == '\n' {
            Position {
                line: 1,
                character: 0,
            }
        } else {
            Position {
                line: 0,
                character: c.len_utf16(),
            }
        }
    }
}

impl From<&'_ str> for Position {
    fn from(s: &str) -> Position {
        s.chars().map(Position::from).sum::<Position>()
    }
}

impl AddAssign for Position {
    fn add_assign(&mut self, other: Self) {
        // 改行があるなら列番号はリセットする。
        if other.line >= 1 {
            self.character = 0;
        }

        self.line += other.line;
        self.character += other.character;
    }
}

impl Add for Position {
    type Output = Self;

    fn add(mut self, other: Self) -> Self {
        self += other;
        self
    }
}

impl std::iter::Sum for Position {
    fn sum<I: Iterator<Item = Position>>(iter: I) -> Position {
        iter.fold(Position::default(), Add::add)
    }
}

impl fmt::Debug for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.character + 1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from_str() {
        assert_eq!(Position::from("hello"), Position::new(0, 5));

        assert_eq!(Position::from("\r\nworld\r\n"), Position::new(2, 0));

        assert_eq!(
            Position::from("hello world\nこんにちは世界"),
            Position::new(1, 7)
        )
    }
}
