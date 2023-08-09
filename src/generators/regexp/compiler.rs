use std::collections::HashSet;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    EmptyString, Symbol,
    RightParen, LeftParen,
    Star, Pipe
}

#[derive(Debug, Clone)]
pub struct Token {
    pub name: TokenType,
    pub lexeme: char,
    pub position: usize
}

pub struct Scanner {
    chars: Vec<char>,
    pub alphabet: HashSet<char>,
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

#[derive(Debug)]
pub enum ExpressionBase {
    EmptyString,
    Symbol {value: char},
    Grouping {inner_expr: Rc<ExpressionBase>},
    Star {inner_expr: Rc<ExpressionBase>},
    Union {items: Vec<Rc<ExpressionBase>>},
    Concat {items: Vec<Rc<ExpressionBase>>},
}

impl From<&ExpressionBase> for String {
    fn from(value: &ExpressionBase) -> Self {
	match value {
	    ExpressionBase::EmptyString => {
		String::new()
	    },
	    ExpressionBase::Symbol { value, .. } => {
		String::from(*value)
	    },
	    ExpressionBase::Grouping
	    { inner_expr, .. } => {
		format!(
		    "({})",
		    String::from(inner_expr.as_ref())
		)
	    },
	    ExpressionBase::Star { inner_expr, .. } => {
		let inner_expr = inner_expr.as_ref();
		match inner_expr {
		    ExpressionBase::Symbol {value} => {
			format!("{value}*")
		    },
		    _ => {
			format!(
			    "({})*",
			    String::from(inner_expr)
			)
		    }
		}
	    },
	    ExpressionBase::Union { items, .. } => {
		items
		    .iter()
		    .map(|item| {
			String::from(item.as_ref())
		    })
		    .collect::<Vec<String>>()
		    .join("|")
	    },
	    ExpressionBase::Concat { items, .. } => {
		items
		    .iter()
		    .fold(
			String::new(),
			|a, b| {
			    format!(
				"{}{}", a,
				String::from(b.as_ref())
			    )
			}
		    )
	    }
	}
    }
}

impl std::fmt::Display for &ExpressionBase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", String::from(*self))
    }
}

use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;
use crate::automata::nfa::NFA;

static mut COUNTER: usize = 0usize;

impl ExpressionBase {
    fn formatted(&self) -> String {
	format!("{}", self)
    }

    pub fn compile(&self, alphabet: &HashSet<char>) -> NFA {
	let id = format!("{:?}", self as *const ExpressionBase);
	let self_string = self.formatted();

	let start_state = unsafe {
	    let temp = COUNTER;
	    COUNTER += 1;
	    format!("(#{temp}<{self_string}>@{id}).<START>")
	};
	let mut states = HashSet::new();
	states.insert(String::from(&start_state));

	let mut transition_function =
	    HashMap::<String, HashMap<char, HashSet<String>>>::new();
	let mut accept_states = HashSet::new();

	match self {
	    Self::EmptyString { .. } => {
		accept_states.insert(String::from(&start_state));
	    },
	    ExpressionBase::Symbol { value, .. } => {
		let accept_state = unsafe {
		    let temp = COUNTER;
		    COUNTER += 1;
		    format!("(#{temp}<{self_string}>@{id}).<ACCEPT>")
		};
		states.insert(String::from(&accept_state));

		transition_function
		    .entry(String::from(&start_state))
		    .or_insert(HashMap::new())
		    .entry(*value)
		    .or_insert(HashSet::new())
		    .insert(String::from(&accept_state));

		accept_states.insert(String::from(&accept_state));
	    },
	    Self::Grouping { inner_expr, .. } => {
		return inner_expr.compile(alphabet);
	    },
	    ExpressionBase::Star { inner_expr, .. } => {
		let expr_nfa = inner_expr.compile(alphabet);
		return NFA::kleene_star(&expr_nfa, &start_state);
	    },
	    Self::Union { items , .. } => {
		let automata =
		    items
		    .iter()
		    .map(|expr| expr.compile(alphabet))
		    .collect::<Vec<NFA>>();
		return NFA::union(automata.iter(), &start_state);
	    },
	    Self::Concat { items , .. } => {
		let automata =
		    items
		    .iter()
		    .map(|expr| expr.compile(alphabet))
		    .collect::<Vec<NFA>>();

		let automata =
		    automata
		    .iter()
		    .collect::<Vec<&NFA>>();

		return NFA::concatenate(&automata[..]);
	    }
	}

	let alphabet = alphabet.clone();
	let is_deterministic = false;
	let dfa = RefCell::new(Rc::new(None));

	NFA::raw_new(
	    states,
	    alphabet,
	    transition_function,
	    start_state,
	    accept_states,
	    is_deterministic,
	    dfa
	)
    }
}

use std::rc::Weak;

#[derive(Debug)]
pub struct Expression {
    parent: RefCell<Option<Weak<Expression>>>,
    base: RefCell<Option<Rc<ExpressionBase>>>,
    pub children: RefCell<Vec<Rc<Expression>>>
}

impl From<&Expression> for String {
    fn from(value: &Expression) -> Self {
	let base = value.base.borrow();
	let base = base.as_ref().unwrap();
	let base = base.as_ref();
	String::from(base)
    }
}

impl Default for Expression {
    fn default() -> Self {
	Expression {
	    parent: RefCell::new(None),
	    base: RefCell::new(None),
	    children: RefCell::new(Vec::new())
	}
    }
}

impl std::fmt::Display for &Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	let base = self.base.borrow();
	let base = base.as_ref().unwrap();
	let base = base.as_ref();
	let base = String::from(base);

        match self.parent.borrow().as_ref() {
	    Some(parent) => {
		let parent = parent.upgrade().unwrap();
		let parent = parent.base.borrow();
		let parent = parent.as_ref().unwrap().as_ref();
		let parent = String::from(parent);
		write!(f, "({parent} <= {base})")
	    }
	    None => {
		write!(f, "{base}")
	    }
	}
    }
}

impl Expression {
    pub fn compile(&self, alphabet: &HashSet<char>) -> NFA {
	self.base.borrow().as_ref().unwrap().compile(alphabet)
    }
}

pub struct Parser {
    pub scanner: RefCell<Scanner>,
    previous: RefCell<Option<Token>>,
    current: RefCell<Option<Token>>
}

impl Parser {
    pub fn new(scanner: Scanner) -> Parser {
	let scanner = RefCell::new(scanner);
	let previous = RefCell::new(None);
	let current = RefCell::new(None);
	Parser {
	    scanner,
	    previous,
	    current
	}
    }

    fn read_current(&self) -> Option<Token> {
	self.current.borrow().clone()
    }

    fn advance(&self) {
	*self.previous.borrow_mut() = self.current.take();
	*self.current.borrow_mut() = self.scanner.borrow_mut().next();
    }

    fn consume(
	&self,
	name: TokenType,
	message: &str,
	add_one_to_position: bool
    ) -> Result<(), (String, usize)> {
	let message = String::from(message);
	match self.read_current() {
	    Some(tok) => {
		if tok.name != name {
		    let position = tok.position +
			if add_one_to_position {1} else {0};
		    Err((message, position))
		} else {
		    self.advance();
		    Ok(())
		}
	    },
	    None => Err((message, &self.scanner.borrow().chars.len()-1))
	}
    }

    fn match_current(&self, expected: TokenType) -> bool {
	if self.check(expected) {
	    self.advance();
	    true
	} else {
	    false
	}
    }

    fn check(&self, expected: TokenType) -> bool {
	match self.read_current() {
	    Some(tok) => tok.name == expected,
	    None => false
	}
    }

    fn format_error(&self, error: (String, usize)) -> String {
	let (msg, pos) = error;
	let msg = format!("{msg}\n");

	let mut pattern =
	    self
	    .scanner
	    .borrow()
	    .chars
	    .iter()
	    .fold(
		String::new(),
		|a, b| format!("{a}{b}")
	    );
	pattern.push('\n');

	let mut indent = String::new(); // 4 spaces
	indent.push(' ');
	indent.push(' ');
	indent.push(' ');
	indent.push(' ');

	let mut caret = String::new();
	caret.push_str(&indent);
	while caret.len() < pos+4 {caret.push(' ')}
	caret.push('^');
	caret.push('\n');

	format!(
	    "[Parsing Error at {pos}]: {msg}{indent}{pattern}{caret}"
	)
    }

    /*
    Expression => Union
    Union => Concat ( '|' Concat )? ( '|' Concat )*
    Concat => Star Star*
    Star => Primary ( '*' )?
    Primary => EMPTY_STRING | SYMBOL | '(' Expression ')'
     */
    pub fn parse(&self) -> Result<Rc<Expression>, String> {
	self.advance();
	match self.expression() {
	    Ok(expr) => Ok(expr),
	    Err(error_info) => Err(self.format_error(error_info))
	}
    }

    pub fn expression(&self) -> Result<Rc<Expression>, (String, usize)> {
	self.star()
    }

    pub fn star(&self) -> Result<Rc<Expression>, (String, usize)> {
	let primary = self.primary()?;
	if self.match_current(TokenType::Star) {
	    let star =
		ExpressionBase::Star {
		    inner_expr:
		    Rc::clone(
			primary
			    .base
			    .borrow()
			    .as_ref()
			    .unwrap()
		    )
		};
	    let star = Rc::new(star);

	    let returned_expression = {
		let parent =
		    RefCell::new(None);
		let base =
		    RefCell::new(Some(star));
		let children =
		    RefCell::new(vec![]);

		Expression {parent, base, children}
	    };
	    let returned_expression =
		Rc::new(returned_expression);

	    *primary.parent.borrow_mut() =
		Some(Rc::downgrade(&returned_expression));

	    returned_expression
		.children
		.borrow_mut()
		.push(primary);

	    Ok(returned_expression)
	} else {
	    Ok(primary)
	}
    }

    pub fn primary(&self) -> Result<Rc<Expression>, (String, usize)> {
	match self.read_current() {
	    Some(peek) => {
		if peek.name == TokenType::LeftParen {
		    self.advance();
		    let parsed_expr = self.expression()?;
		    self.consume(
			TokenType::RightParen,
			"Expected `)` after expression.",
			true
		    )?;

		    let grouping =
			ExpressionBase::Grouping {
			    inner_expr:
			    Rc::clone(
				parsed_expr
				    .base
				    .borrow()
				    .as_ref()
				    .unwrap()
			    )
			};
		    let grouping = Rc::new(grouping);

		    let returned_expression = {
			let parent =
			    RefCell::new(None);
			let base =
			    RefCell::new(Some(grouping));
			let children =
			    RefCell::new(vec![]);

			Expression {parent, base, children}
		    };
		    let returned_expression =
			Rc::new(returned_expression);

		    *parsed_expr.parent.borrow_mut() =
			Some(Rc::downgrade(&returned_expression));

		    returned_expression
			.children
			.borrow_mut()
			.push(parsed_expr);

		    Ok(returned_expression)
		} else if peek.name == TokenType::Symbol {
		    // Single-symbol expression
		    self.advance();
		    let parent =
			RefCell::new(None);
		    let symbol =
			ExpressionBase::Symbol {value: peek.lexeme};
		    let symbol = Rc::new(symbol);
		    let base =
			RefCell::new(Some(symbol));
		    let children =
			RefCell::new(Vec::new());
		    Ok(Rc::new(Expression {parent, base, children}))
		} else if peek.name == TokenType::EmptyString {
		    // Empty string expression
		    // Something like (), (|), ()|(), ...
		    self.advance();
		    let parent =
			RefCell::new(None);
		    let symbol =
			ExpressionBase::EmptyString;
		    let symbol = Rc::new(symbol);
		    let base =
			RefCell::new(Some(symbol));
		    let children =
			RefCell::new(Vec::new());
		    Ok(Rc::new(Expression {parent, base, children}))
		} else {
		    // Parsing Error: Un-expected character.
		    let msg = String::from("Un-expected character");
		    let pos = peek.position;
		    Err((msg, pos))
		}
	    }
	    None => {
		// End of input
		Ok(Rc::new(Expression::default()))
	    }
	}
    }
}
