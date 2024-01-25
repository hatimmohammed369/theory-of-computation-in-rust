use crate::automata::nfa::AlphabetSymbol;
use std::collections::HashSet;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenName {
    EmptyString,
    Symbol,
    RightParen,   // )
    LeftParen,    // (
    RightBracket, // ]
    LeftBracket,  // [
    Star,
    Pipe,
    Dot,
    RangeHyphen, // Minus/hyphene -, to be used inside character classes
    // Minus defines ranges inside character classes
    // when preceeded by nothing or succeeded by nothing it's treated
    // like any other character inside the character class
    Caret, // ^, to be used inside character classes
    // Caret inverts the character classes omly when its after the opening [
    // otherwise, it's treated like any other character inside the character class
    // aslo it's an error write ^ alone inside [ and ]
    EscapedSlash,        // \\
    EscapedRightParen,   // \)
    EscapedLeftParen,    // \(
    EscapedStar,         // \*
    EscapedPipe,         // \|
    EscapedRightBracket, // \], to be used inside character classes
    EscapedLeftBracket,  // \[, to be used inside character classes
    EscapedMinus,        // \-, to be used inside character classes
    EscapedCaret,        // \^, to be used inside character classes
}

#[derive(Debug, Clone)]
pub struct Token {
    pub name: TokenName,
    pub lexeme: char,
    pub position: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RangePart {
    None,
    LowerLimit,
    Hyphen, // -
    UpperLimit,
}

pub struct Scanner {
    pub chars: Vec<char>,
    pub alphabet: HashSet<AlphabetSymbol>,
    empty_string_found: bool,
    inside_character_class: bool,
    // field (inside_character_class) is bool because we can't nest character classes
    range_part: RangePart,
    // field (range)
    current: usize,
}

use RangePart::*;

impl Scanner {
    pub fn new(source: &str) -> Scanner {
        Scanner {
            chars: source.chars().collect::<_>(),
            alphabet: HashSet::new(),
            empty_string_found: false,
            inside_character_class: false,
            range_part: None,
            current: 0,
        }
    }
}

impl Iterator for Scanner {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let mut next = Option::None;

        let peek = {
            if self.current >= self.chars.len() {
                '\0'
            } else {
                self.chars[self.current]
            }
        };

        let prev = {
            if self.current == 0 {
                '\0'
            } else {
                self.chars[self.current - 1]
            }
        };

        let prev_escaped = {
            if self.current >= 2 {
                *self.chars.get(self.current - 2).unwrap_or(&'\0') == '\\'
            } else {
                false
            }
        };

        if !prev_escaped && !self.empty_string_found {
            if (prev == '\0' && (peek == '\0' || peek == '|'))
                || (prev == '(' && (peek == '|' || peek == ')'))
                || (prev == '|' && (peek == '|' || peek == ')' || peek == '\0'))
                || (prev == '[' && peek == ']')
            {
                if prev == '[' && peek == ']' {
                    self.inside_character_class = true;
                }
                self.alphabet.insert(AlphabetSymbol::EmptyString);
                next = Some(Token {
                    name: TokenName::EmptyString,
                    lexeme: '\0',
                    position: self.current,
                });
            }
            self.empty_string_found = true;
        }

        if next.is_none() && self.current < self.chars.len() {
            use TokenName::*;
            self.empty_string_found = false;
            next = Some(Token {
                name: LeftParen,
                lexeme: peek,
                position: self.current,
            });
            let tok = next.as_mut().unwrap();

            let next_char = *self.chars.get(self.current + 1).unwrap_or(&'\0');
            match peek {
                '(' => {
                    tok.name = LeftParen;
                }
                ')' => {
                    tok.name = RightParen;
                }
                '[' => {
                    if !self.inside_character_class {
                        self.inside_character_class = true;
                        tok.name = LeftBracket;
                    } else {
                        use std::panic;
                        let pos = self.current;
                        let mut caret = String::new();
                        while caret.len() < pos {
                            caret.push(' ');
                        }
                        caret.push('^');
                        let pattern = self
                            .chars
                            .iter()
                            .fold(String::new(), |a, b| format!("{a}{b}"));

                        eprint!(
                            "Error in position {pos}:\nYou can't nest character classes\n\
			     To match [ and ] inside character classes use \\[ and \\]\n\
			     {pattern}\n{caret}\n"
                        );

                        panic::set_hook(Box::new(|_| {}));
                        panic!();
                    }
                }
                ']' => {
                    self.inside_character_class = false;
                    self.range_part = None;
                    tok.name = RightBracket;
                }
                '-' if self.inside_character_class
                    && self.range_part == LowerLimit
                    && next_char != ']'
                    && (prev != '[' || prev_escaped) // previous character is not an un-escaped [
                    && ((prev.is_ascii_digit() && next_char.is_ascii_digit()) // digits range
                        || /*alphabet range*/ (prev.is_alphabetic() && next_char.is_alphabetic())) =>
                {
                    let unordered = (prev as u8) > (next_char as u8);
                    let different_cases = {
                        (prev.is_lowercase() && next_char.is_uppercase())
                            || (prev.is_uppercase() && next_char.is_lowercase())
                    };
                    if unordered || different_cases {
                        use std::panic;
                        let pos = self.current;
                        let mut caret = String::new();
                        while caret.len() < pos - 1 {
                            caret.push(' ');
                        }
                        caret.push_str("^^^");
                        let pattern = self
                            .chars
                            .iter()
                            .fold(String::new(), |a, b| format!("{a}{b}"));

                        eprint!(
                            "Error in position {pos}:\nBad range\n\
                             valid ranges are n-m, where 0<= n <= m <= 9\n\
                             and x-y where x and y:\n\
                             both english alphabet letters\n\
			     x and y are both lowercase or both uppercase\n\
                             x comes before y in the english alphabet\n\
			     {pattern}\n{caret}\n"
                        );

                        panic::set_hook(Box::new(|_| {}));
                        panic!();
                    } else {
                        tok.name = RangeHyphen;
                        self.range_part = Hyphen;
                    }
                }
                '^' if self.inside_character_class && (prev == '[' && !prev_escaped) => {
                    if next_char == ']' {
                        use std::panic;
                        let pos = self.current;
                        let mut caret = String::new();
                        while caret.len() < pos {
                            caret.push(' ');
                        }
                        caret.push('^');
                        let pattern = self
                            .chars
                            .iter()
                            .fold(String::new(), |a, b| format!("{a}{b}"));

                        eprint!(
                            "Error in position {pos}:\nYou must add characters after ^ when it's preceeded by nothing inside character class\n\
			     To match ^ inside character class use \\^\n\
			     {pattern}\n{caret}\n"
                        );

                        panic::set_hook(Box::new(|_| {}));
                        panic!();
                    } else {
                        tok.name = Caret;
                    }
                }
                '*' => {
                    tok.name = Star;
                }
                '|' => {
                    tok.name = Pipe;
                }
                '.' => {
                    tok.name = Dot;
                }
                '\\' => {
                    // EscapedSlash,       \\
                    // EscapedRightParen,  \)
                    // EscapedLeftParen,   \(
                    // EscapedStar,        \*
                    // EscapedPipe,        \|
                    // EscapedRightBracket, \]
                    // EscapedLeftBracket,  \[
                    // EscapedMinus,        \-
                    tok.lexeme = next_char;
                    self.alphabet.insert(AlphabetSymbol::Character(next_char));
                    match next_char {
                        '\\' => {
                            tok.name = EscapedSlash;
                        }
                        '(' => {
                            tok.name = EscapedLeftParen;
                        }
                        ')' => {
                            tok.name = EscapedRightParen;
                        }
                        '*' => {
                            tok.name = EscapedStar;
                        }
                        '|' => {
                            tok.name = EscapedPipe;
                        }
                        '[' => {
                            tok.name = EscapedLeftBracket;
                        }
                        ']' => {
                            tok.name = EscapedRightBracket;
                        }
                        '-' => {
                            tok.name = EscapedMinus;
                        }
                        '^' => {
                            tok.name = EscapedCaret;
                        }
                        _ => {
                            use std::panic;
                            let pos = self.current;
                            let mut caret = String::new();
                            while caret.len() < pos {
                                caret.push(' ');
                            }
                            caret.push_str("^^");
                            let pattern = self
                                .chars
                                .iter()
                                .fold(String::new(), |a, b| format!("{a}{b}"));

                            eprint!(
                                "Error in position {pos}:\n\
                                 Un-recognized escape sequence `\\{next_char}`\n\
				 {pattern}\n{caret}\n"
                            );

                            panic::set_hook(Box::new(|_| {}));
                            panic!();
                        }
                    }
                    if self.inside_character_class {
                        self.range_part = None;
                    }
                    self.current += 1;
                }
                c => {
                    self.alphabet.insert(AlphabetSymbol::Character(c));
                    tok.name = Symbol;
                    if self.inside_character_class {
                        self.range_part = {
                            if c.is_alphanumeric() {
                                match self.range_part {
                                    None => LowerLimit,
                                    LowerLimit => LowerLimit,
                                    Hyphen => UpperLimit,
                                    UpperLimit => LowerLimit,
                                }
                            } else if c == '-' {
                                // In case - escaped from the arm handling it above
                                match self.range_part {
                                    LowerLimit => Hyphen,
                                    _ => None,
                                }
                            } else {
                                // Any other non-alphanumeric character that's also not -
                                None
                            }
                        }
                    }
                }
            }

            self.current += 1;
        }

        next
    }
}
