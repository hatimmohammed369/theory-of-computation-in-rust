#![allow(unused)]
#![allow(dead_code)]

use std::collections::HashSet;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenName {
    EmptyString,
    Symbol,
    RightParen,
    LeftParen,
    Star,
    Pipe,
    EscapedSlash,      // \\
    EscapedRightParen, // \(
    EscapedLeftParen,  // \)
    EscapedStar,       // \*
    EscapedPipe,       // \|
}

#[derive(Debug, Clone)]
pub struct Token {
    pub name: TokenName,
    pub lexeme: char,
    pub position: usize,
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
            current: 0,
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
            {
                self.alphabet.insert('\0');
                next = Some(Token {
                    name: TokenName::EmptyString,
                    lexeme: '\0',
                    position: self.current,
                });
            }
            self.empty_string_found = true;
        }

        if next.is_none() && self.current < self.chars.len() {
            self.empty_string_found = false;

            match peek {
                '(' => {
                    next = Some(Token {
                        name: TokenName::LeftParen,
                        lexeme: '(',
                        position: self.current,
                    });
                }
                ')' => {
                    next = Some(Token {
                        name: TokenName::RightParen,
                        lexeme: ')',
                        position: self.current,
                    });
                }
                '*' => {
                    next = Some(Token {
                        name: TokenName::Star,
                        lexeme: '*',
                        position: self.current,
                    });
                }
                '|' => {
                    next = Some(Token {
                        name: TokenName::Pipe,
                        lexeme: '|',
                        position: self.current,
                    });
                }
                '\\' => {
                    // EscapedSlash,       \\
                    // EscapedRightParen,  \(
                    // EscapedLeftParen,   \)
                    // EscapedStar,        \*
                    // EscapedPipe,        \|

                    let next_char = *self.chars.get(self.current + 1).unwrap_or(&'\0');
                    self.alphabet.insert(next_char);
                    if next_char == '\\' {
                        next = Some(Token {
                            name: TokenName::EscapedSlash,
                            lexeme: next_char,
                            position: self.current,
                        });
                    } else if next_char == '(' {
                        next = Some(Token {
                            name: TokenName::EscapedLeftParen,
                            lexeme: next_char,
                            position: self.current,
                        });
                    } else if next_char == ')' {
                        next = Some(Token {
                            name: TokenName::EscapedRightParen,
                            lexeme: next_char,
                            position: self.current,
                        });
                    } else if next_char == '*' {
                        next = Some(Token {
                            name: TokenName::EscapedStar,
                            lexeme: next_char,
                            position: self.current,
                        });
                    } else if next_char == '|' {
                        next = Some(Token {
                            name: TokenName::EscapedPipe,
                            lexeme: next_char,
                            position: self.current,
                        });
                    } else {
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

                        let msg = format!(
                            "Un-recognized escape sequence\
				 `\\{next_char}` in position {pos}:\n\
				 {pattern}\n{caret}\n
				 "
                        );
                        eprint!("{msg}");

                        panic::set_hook(Box::new(|_| {}));
                        panic!();
                    }
                    self.current += 1;
                }
                c => {
                    self.alphabet.insert(c);
                    next = Some(Token {
                        name: TokenName::Symbol,
                        lexeme: c,
                        position: self.current,
                    });
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
    Symbol { value: char },
    Grouping { inner_expr: Rc<ExpressionBase> },
    Star { inner_expr: Rc<ExpressionBase> },
    Union { items: Vec<Rc<ExpressionBase>> },
    Concat { items: Vec<Rc<ExpressionBase>> },
}

impl From<&ExpressionBase> for String {
    fn from(value: &ExpressionBase) -> Self {
        match value {
            ExpressionBase::EmptyString => String::new(),
            ExpressionBase::Symbol { value, .. } => String::from(*value),
            ExpressionBase::Grouping { inner_expr, .. } => {
                format!("({})", String::from(inner_expr.as_ref()))
            }
            ExpressionBase::Star { inner_expr, .. } => {
                let inner_expr = inner_expr.as_ref();
                match inner_expr {
                    ExpressionBase::Symbol { value } => {
                        format!("{value}*")
                    }
                    _ => {
                        format!("({})*", String::from(inner_expr))
                    }
                }
            }
            ExpressionBase::Union { items, .. } => items
                .iter()
                .map(|item| String::from(item.as_ref()))
                .collect::<Vec<String>>()
                .join("|"),
            ExpressionBase::Concat { items, .. } => items.iter().fold(String::new(), |a, b| {
                format!("{}{}", a, String::from(b.as_ref()))
            }),
        }
    }
}

impl std::fmt::Display for &ExpressionBase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", String::from(*self))
    }
}

use crate::automata::nfa::NFA;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

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

        let mut transition_function = HashMap::<String, HashMap<char, HashSet<String>>>::new();
        let mut accept_states = HashSet::new();

        match self {
            Self::EmptyString { .. } => {
                accept_states.insert(String::from(&start_state));
            }
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
            }
            Self::Grouping { inner_expr, .. } => {
                return inner_expr.compile(alphabet);
            }
            ExpressionBase::Star { inner_expr, .. } => {
                let expr_nfa = inner_expr.compile(alphabet);
                return NFA::kleene_star(&expr_nfa, &start_state);
            }
            Self::Union { items, .. } => {
                let automata = items
                    .iter()
                    .map(|expr| expr.compile(alphabet))
                    .collect::<Vec<NFA>>();
                return NFA::union(automata.iter(), &start_state);
            }
            Self::Concat { items, .. } => {
                let automata = items
                    .iter()
                    .map(|expr| expr.compile(alphabet))
                    .collect::<Vec<NFA>>();

                let automata = automata.iter().collect::<Vec<&NFA>>();

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
            dfa,
        )
    }
}

use std::rc::Weak;

#[derive(Debug)]
pub struct Expression {
    parent: RefCell<Option<Weak<Expression>>>,
    base: RefCell<Option<Rc<ExpressionBase>>>,
    pub children: RefCell<Vec<Rc<Expression>>>,
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
            children: RefCell::new(Vec::new()),
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

pub struct RegexpParseError {
    message: String,
    position: usize,
    post_message: String,
}

impl RegexpParseError {
    fn format_error(&self, pattern: &str) -> String {
        let msg = format!("{}\n", self.message);

        let position = self.position;
        let mut caret = String::new();
        while caret.len() < position {
            caret.push(' ')
        }
        caret.push('^');
        caret.push('\n');
        let post_message = &self.post_message;

        format!("[Parsing Error at {position}]: {msg}{pattern}\n{caret}\n{post_message}")
    }
}

enum GroupingType {
    Parentheses, // ()
    Range,       // []
    Multiplier,  // {m, n}
}

pub struct Parser {
    pub scanner: RefCell<Scanner>,
    previous: RefCell<Option<Token>>,
    current: RefCell<Option<Token>>,
    groupings: RefCell<Vec<GroupingType>>,
}

impl Parser {
    pub fn new(scanner: Scanner) -> Parser {
        let scanner = RefCell::new(scanner);
        let previous = RefCell::new(None);
        let current = RefCell::new(None);
        let groupings = RefCell::new(Vec::new());
        Parser {
            scanner,
            previous,
            current,
            groupings,
        }
    }

    fn read_current(&self) -> Option<Token> {
        self.current.borrow().clone()
    }

    fn read_previous(&self) -> Option<Token> {
        self.previous.borrow().clone()
    }

    fn advance(&self) {
        *self.previous.borrow_mut() = self.current.take();
        *self.current.borrow_mut() = self.scanner.borrow_mut().next();
    }

    fn consume(&self, name: TokenName, message: &str) -> Result<(), RegexpParseError> {
        let message = String::from(message);
        match self.read_current() {
            Some(tok) => {
                if tok.name != name {
                    let message = String::from(message);
                    let position = tok.position;
                    let post_message = String::new();
                    Err(RegexpParseError {
                        message,
                        position,
                        post_message,
                    })
                } else {
                    self.advance();
                    Ok(())
                }
            }
            None => {
                let message = String::from(message);
                let position = &self.scanner.borrow().chars.len() - 1;
                let post_message = String::new();
                Err(RegexpParseError {
                    message,
                    position,
                    post_message,
                })
            }
        }
    }

    fn match_current(&self, expected: TokenName) -> bool {
        if self.check(expected) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn check(&self, expected: TokenName) -> bool {
        match self.read_current() {
            Some(tok) => tok.name == expected,
            None => false,
        }
    }

    /*
    AFTER PARSING EXPRESSION (E), FIELD (current) MUST POINT TO THE FIRST
    TOKEN AFTER EXPRESSION (E)
     */
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
            Err(error) => {
                let pattern = self
                    .scanner
                    .borrow()
                    .chars
                    .iter()
                    .fold(String::new(), |a, b| format!("{a}{b}"));
                Err(error.format_error(&pattern))
            }
        }
    }

    // Expression => Union
    pub fn expression(&self) -> Result<Rc<Expression>, RegexpParseError> {
        let parsed_expression = self.union()?;
        if let Some(tok) = self.read_current() {
            if self.groupings.borrow().is_empty() // No active group
		&& tok.name == TokenName::RightParen
            {
                let message = String::from("Un-matched `)`");
                let position = tok.position;
                let post_message = String::new();
                return Err(RegexpParseError {
                    message,
                    position,
                    post_message,
                });
            }
        }
        Ok(parsed_expression)
    }

    // Union => Concat ( '|' Concat )? ( '|' Concat )*
    pub fn union(&self) -> Result<Rc<Expression>, RegexpParseError> {
        let mut concats = Vec::<Rc<Expression>>::new();
        loop {
            let concat_expr = self.concat()?;
            if concat_expr.base.borrow().is_some() {
                concats.push(Rc::clone(&concat_expr));
            } else {
                // Can't parse another expression, stop
                break;
            }

            if !self.match_current(TokenName::Pipe) {
                break;
            }
        }

        if concats.is_empty() {
            // Could not parse any expression.
            Ok(Rc::new(Expression::default()))
        } else if concats.len() == 1 {
            Ok(concats.pop().unwrap())
        } else {
            let union = ExpressionBase::Union {
                items: concats
                    .iter()
                    .map(|expr| Rc::clone(expr.base.borrow().as_ref().unwrap()))
                    .collect::<_>(),
            };
            let union = Rc::new(union);

            let returned_expression = {
                let parent = RefCell::new(None);
                let base = RefCell::new(Some(union));
                let children = RefCell::new(vec![]);

                Expression {
                    parent,
                    base,
                    children,
                }
            };
            let returned_expression = Rc::new(returned_expression);

            concats.iter().for_each(|concat_expr| {
                *concat_expr.parent.borrow_mut() = Some(Rc::downgrade(&returned_expression));
                returned_expression
                    .children
                    .borrow_mut()
                    .push(Rc::clone(concat_expr));
            });

            Ok(returned_expression)
        }
    }

    // Concat => Star Star*
    pub fn concat(&self) -> Result<Rc<Expression>, RegexpParseError> {
        let mut stars = Vec::<Rc<Expression>>::new();
        loop {
            let star_expr = self.star()?;
            if star_expr.base.borrow().is_some() {
                stars.push(Rc::clone(&star_expr));
            } else {
                // Could not parse another expression, stop
                break;
            }
        }

        if stars.is_empty() {
            // Could not parse any expression
            Ok(Rc::new(Expression::default()))
        } else if stars.len() <= 1 {
            Ok(stars.pop().unwrap())
        } else {
            let concat = ExpressionBase::Concat {
                items: stars
                    .iter()
                    .map(|expr| Rc::clone(expr.base.borrow().as_ref().unwrap()))
                    .collect::<_>(),
            };
            let concat = Rc::new(concat);

            let returned_expression = {
                let parent = RefCell::new(None);
                let base = RefCell::new(Some(concat));
                let children = RefCell::new(vec![]);

                Expression {
                    parent,
                    base,
                    children,
                }
            };
            let returned_expression = Rc::new(returned_expression);

            stars.iter().for_each(|star_expr| {
                *star_expr.parent.borrow_mut() = Some(Rc::downgrade(&returned_expression));
                returned_expression
                    .children
                    .borrow_mut()
                    .push(Rc::clone(star_expr));
            });

            Ok(returned_expression)
        }
    }

    // Star => Primary ( '*' )?
    pub fn star(&self) -> Result<Rc<Expression>, RegexpParseError> {
        let primary = self.primary()?;
        if self.match_current(TokenName::Star) {
            if primary.base.borrow().is_none() {
                /*
                Could not parse an expression
                Error: Using `*` preceeded by nothing
                 */
                let message = String::from("Expected expression before `*`");
                let position = self.read_previous().unwrap().position;
                let post_message = String::from("Use \\* to match a literal `*`");
                return Err(RegexpParseError {
                    message,
                    position,
                    post_message,
                });
            }
            let star = ExpressionBase::Star {
                inner_expr: Rc::clone(primary.base.borrow().as_ref().unwrap()),
            };
            let star = Rc::new(star);

            let returned_expression = {
                let parent = RefCell::new(None);
                let base = RefCell::new(Some(star));
                let children = RefCell::new(vec![]);

                Expression {
                    parent,
                    base,
                    children,
                }
            };
            let returned_expression = Rc::new(returned_expression);

            *primary.parent.borrow_mut() = Some(Rc::downgrade(&returned_expression));

            returned_expression.children.borrow_mut().push(primary);

            Ok(returned_expression)
        } else {
            Ok(primary)
        }
    }

    // Primary => EMPTY_STRING | SYMBOL | '(' Expression ')'
    pub fn primary(&self) -> Result<Rc<Expression>, RegexpParseError> {
        match self.read_current() {
            Some(peek) => {
                if peek.name == TokenName::LeftParen {
                    self.groupings.borrow_mut().push(GroupingType::Parentheses);
                    self.advance();
                    let parsed_expr = self.expression()?;
                    self.consume(TokenName::RightParen, "Expected `)` after expression.")?;
                    self.groupings.borrow_mut().pop();

                    let grouping = ExpressionBase::Grouping {
                        inner_expr: Rc::clone(parsed_expr.base.borrow().as_ref().unwrap()),
                    };
                    let grouping = Rc::new(grouping);

                    let returned_expression = {
                        let parent = RefCell::new(None);
                        let base = RefCell::new(Some(grouping));
                        let children = RefCell::new(vec![]);

                        Expression {
                            parent,
                            base,
                            children,
                        }
                    };
                    let returned_expression = Rc::new(returned_expression);

                    *parsed_expr.parent.borrow_mut() = Some(Rc::downgrade(&returned_expression));

                    returned_expression.children.borrow_mut().push(parsed_expr);

                    Ok(returned_expression)
                } else if peek.name == TokenName::Symbol || Self::is_escaped_token(&peek) {
                    // Single-symbol expression
                    self.advance();
                    let parent = RefCell::new(None);
                    let symbol = ExpressionBase::Symbol { value: peek.lexeme };
                    let symbol = Rc::new(symbol);
                    let base = RefCell::new(Some(symbol));
                    let children = RefCell::new(Vec::new());
                    Ok(Rc::new(Expression {
                        parent,
                        base,
                        children,
                    }))
                } else if peek.name == TokenName::EmptyString {
                    // Empty string expression
                    // Something like (), (|), ()|(), ...
                    self.advance();
                    let parent = RefCell::new(None);
                    let symbol = ExpressionBase::EmptyString;
                    let symbol = Rc::new(symbol);
                    let base = RefCell::new(Some(symbol));
                    let children = RefCell::new(Vec::new());
                    Ok(Rc::new(Expression {
                        parent,
                        base,
                        children,
                    }))
                } else {
                    /*
                    No expression found
                    When we have more functionality this else branch will change.
                     */
                    Ok(Rc::new(Expression::default()))
                }
            }
            None => {
                // End of input
                Ok(Rc::new(Expression::default()))
            }
        }
    }

    fn is_escaped_token(token: &Token) -> bool {
        match token.name {
            TokenName::EscapedSlash
            | TokenName::EscapedRightParen
            | TokenName::EscapedLeftParen
            | TokenName::EscapedStar
            | TokenName::EscapedPipe => true,
            _ => false,
        }
    }
}
