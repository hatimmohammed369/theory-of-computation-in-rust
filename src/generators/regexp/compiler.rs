use std::collections::HashSet;

#[derive(Debug)]
pub enum TokenType {
    EmptyString, Symbol,
    RightParen, LeftParen,
    Star, Pipe
}

#[derive(Debug)]
pub struct Token {
    pub name: TokenType,
    pub lexeme: char,
    pub position: usize
}

pub struct Scanner {
    chars: Vec<char>,
    alphabet: HashSet<char>,
    empty_string_found: bool,
    current: usize,
}

impl Scanner {
    pub fn new(source: &str) -> Scanner {
	Scanner {
	    chars: source.chars().collect::<_>(),
	    alphabet: HashSet::new(),
	    empty_string_found: false,
	    current: 0
	}
    }
}

impl Iterator for Scanner {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
	let mut next = None;

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
		self.chars[self.current-1]
	    }
	};

	if !self.empty_string_found {
	    if (prev == '\0' && (peek == '\0' || peek == '|')) ||
		(prev == '(' && (peek == '|' || peek == ')')) ||
		(prev=='|' && (peek=='|' || peek==')' || peek=='\0')) {
		    self.alphabet.insert('\0');
		    next = Some(
			Token {
			    name: TokenType::EmptyString,
			    lexeme: '\0',
			    position: self.current
			}
		    );
		}
	    self.empty_string_found = true;
	}

	if next.is_none() && self.current < self.chars.len() {
	    self.empty_string_found = false;

	    match peek {
		'(' => {
		    next = Some(
			Token {
			    name: TokenType::LeftParen,
			    lexeme: '(',
			    position: self.current
			}
		    );
		}
		')' => {
		    next = Some(
			Token {
			    name: TokenType::RightParen,
			    lexeme: ')',
			    position: self.current
			}
		    );
		}
		'*' => {
		    next = Some(
			Token {
			    name: TokenType::Star,
			    lexeme: '*',
			    position: self.current
			}
		    );
		}
		'|' => {
		    next = Some(
			Token {
			    name: TokenType::Pipe,
			    lexeme: '|',
			    position: self.current
			}
		    );
		}
		c => {
		    self.alphabet.insert(c);
		    next = Some(
			Token {
			    name: TokenType::Symbol,
			    lexeme: c,
			    position: self.current
			}
		    );
		}
	    }

	    self.current += 1;
	}

	next
    }
}
