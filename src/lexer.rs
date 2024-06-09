use crate::error::*;

const RECOGNIZED_NON_ALPHANUM_SYMBOLS: [&str; 25] = [
    ";", ":", "->", "=", "::", ",", ".",
    "{", "}",
    "(", ")",
    "[", "]",
    "+", "-", "*", "/", "%",
    "!", "==", "!=", "<=", ">=", "<", ">",
];

const VALID_NON_ALPHANUM_IDENTIFIERS: [char; 1] = [
    '_',
];

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexemeKind {
    Import,
    Let,
    Proc,
    Func,
    If,
    Else,
    While,
    Do,
    Const,
    Return,
    True,
    False,
    
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,

    Semicolon,
    Colon,
    DoubleColon,
    Period,
    Comma,
    Arrow,

    Equals,
    Bang,
    DoubleEquals,
    NotEquals,
    LessEquals,
    GreaterEquals,
    Less,
    Greater,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,

    StringLiteral(String),
    NumberLiteral(String),
    Identifier(String),
}

impl LexemeKind {
    fn get_matching_bracket(&self) -> Option<Self> {
        match self {
            LexemeKind::RightParen => Some(LexemeKind::LeftParen),
            LexemeKind::RightBrace => Some(LexemeKind::LeftBrace),
            LexemeKind::RightBracket => Some(LexemeKind::LeftBracket),
            
            LexemeKind::LeftParen => Some(LexemeKind::RightParen),
            LexemeKind::LeftBrace => Some(LexemeKind::RightBrace),
            LexemeKind::LeftBracket => Some(LexemeKind::RightBracket),

            _ => None
        }
    }
}

impl ToString for LexemeKind {
    fn to_string(&self) -> String {
        match self {
            Self::Import => "import",
            Self::Let    => "let",
            Self::Proc   => "proc",
            Self::Func   => "func",
            Self::If     => "if",
            Self::Else   => "else",
            Self::While  => "while",
            Self::Do     => "do",
            Self::Const  => "const",
            Self::Return => "return",
            Self::True   => "true",
            Self::False  => "false",
            
            Self::LeftParen    => "(",
            Self::RightParen   => ")",
            Self::LeftBrace    => "{",
            Self::RightBrace   => "}",
            Self::LeftBracket  => "[",
            Self::RightBracket => "]",

            Self::Semicolon => ";",
            Self::Colon => ":",
            Self::DoubleColon => "::",
            Self::Period => ".",
            Self::Comma => ",",
            Self::Arrow => "->",
            
            Self::Equals => "=",
            Self::Bang => "!",
            Self::DoubleEquals => "==",
            Self::NotEquals => "!=",
            Self::LessEquals => "<=",
            Self::GreaterEquals => ">=",
            Self::Less => "<",
            Self::Greater => ">",
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Star => "*",
            Self::Slash => "/",
            Self::Percent => "%",
            
            Self::StringLiteral(value) | Self::NumberLiteral(value) | Self::Identifier(value) => {
                value.as_str()
            }
        }.to_owned()
    }
}

#[derive(Debug, Clone)]
pub struct Lexeme {
    pub kind: LexemeKind,
    pub line: usize
}

trait ToLexeme {
    fn to_lexeme(&self, line: usize) -> Result<Lexeme, Error>;
}

impl ToLexeme for String {
    // IMPORTANT!!!!! function does not handle StringLiterals
    fn to_lexeme(&self, line: usize) -> Result<Lexeme, Error> {
        match self.as_str() {
            "import" => Ok(Lexeme{kind: LexemeKind::Import,        line}),
            "let"    => Ok(Lexeme{kind: LexemeKind::Let,           line}),
            "proc"   => Ok(Lexeme{kind: LexemeKind::Proc,          line}),
            "func"   => Ok(Lexeme{kind: LexemeKind::Func,          line}),
            "if"     => Ok(Lexeme{kind: LexemeKind::If,            line}),
            "else"   => Ok(Lexeme{kind: LexemeKind::Else,          line}),
            "while"  => Ok(Lexeme{kind: LexemeKind::While,         line}),
            "do"     => Ok(Lexeme{kind: LexemeKind::Do,            line}),
            "const"  => Ok(Lexeme{kind: LexemeKind::Const,         line}),
            "return" => Ok(Lexeme{kind: LexemeKind::Return,        line}),
            "true"   => Ok(Lexeme{kind: LexemeKind::True,          line}),
            "false"  => Ok(Lexeme{kind: LexemeKind::False,         line}),
            
            "("      => Ok(Lexeme{kind: LexemeKind::LeftParen,     line}),
            ")"      => Ok(Lexeme{kind: LexemeKind::RightParen,    line}),
            "{"      => Ok(Lexeme{kind: LexemeKind::LeftBrace,     line}),
            "}"      => Ok(Lexeme{kind: LexemeKind::RightBrace,    line}),
            "["      => Ok(Lexeme{kind: LexemeKind::LeftBracket,   line}),
            "]"      => Ok(Lexeme{kind: LexemeKind::RightBracket,  line}),
            
            ";"      => Ok(Lexeme{kind: LexemeKind::Semicolon,     line}),
            "::"     => Ok(Lexeme{kind: LexemeKind::DoubleColon,   line}),
            ":"      => Ok(Lexeme{kind: LexemeKind::Colon,         line}),
            "."      => Ok(Lexeme{kind: LexemeKind::Period,        line}),
            ","      => Ok(Lexeme{kind: LexemeKind::Comma,         line}),
            "->"     => Ok(Lexeme{kind: LexemeKind::Arrow,         line}),
            
            "="      => Ok(Lexeme{kind: LexemeKind::Equals,        line}),
            "!"      => Ok(Lexeme{kind: LexemeKind::Bang,          line}),
            "=="     => Ok(Lexeme{kind: LexemeKind::DoubleEquals,  line}),
            "!="     => Ok(Lexeme{kind: LexemeKind::NotEquals,     line}),
            "<="     => Ok(Lexeme{kind: LexemeKind::LessEquals,    line}),
            ">="     => Ok(Lexeme{kind: LexemeKind::GreaterEquals, line}),
            "<"      => Ok(Lexeme{kind: LexemeKind::Less,          line}),
            ">"      => Ok(Lexeme{kind: LexemeKind::Greater,       line}),

            "+"      => Ok(Lexeme{kind: LexemeKind::Plus,          line}),
            "-"      => Ok(Lexeme{kind: LexemeKind::Minus,         line}),
            "*"      => Ok(Lexeme{kind: LexemeKind::Star,          line}),
            "/"      => Ok(Lexeme{kind: LexemeKind::Slash,         line}),
            "%"      => Ok(Lexeme{kind: LexemeKind::Percent,       line}),
            
            _ => {
                // is symbol int?
                if self.parse::<u64>().is_ok() {
                    Ok(Lexeme{
                        kind: LexemeKind::NumberLiteral(self.to_owned()),
                        line
                    })
                    
                } else {
                    // else, it is probably identifier                                        
                    // check if all chars are valid identifier chars
                    for i in self.chars() {
                        if i.is_alphanumeric() ||  VALID_NON_ALPHANUM_IDENTIFIERS.contains(&i) {
                            continue;
                        }
                        return Err(Error{
                            message: format!("invalid symbol `{}`", self.to_owned()),
                            lines: vec![line],
                        });
                    }
                    
                    Ok(Lexeme{
                        kind:LexemeKind::Identifier(self.to_owned()),
                        line
                    })
                }
            }
        }
    }
}

pub trait Lexer {
    fn lex(&self) -> Result<Vec<Lexeme>, Error>;
}

impl Lexer for String {
    fn lex(&self) -> Result<Vec<Lexeme>, Error> {
        // println!("in lexer");
        
        let mut lexemes = Vec::<Lexeme>::new();

        let mut buffer = String::new();

        let mut line = 1usize;
        let mut line_string_start = 0usize;
        
        let complete_token = |b: &mut String, l: &mut Vec<Lexeme>, line: usize| -> Result<(), Error>{
            if !b.is_empty() {            
                let lexeme = b.to_lexeme(line)?;
                l.push(lexeme);
                b.clear();
            }

            Ok(())
        };

        let mut should_skip = false;
        let mut in_string = false;
        let self_length = self.chars().count();
        for (i, c) in self[..self_length - 1].chars().enumerate() {
            // dbg!(c);
            
            if c == '\n' {
                line += 1;
            }
        
            // finding string literals
            if in_string {
                // if the end of the string is found
                if c == '"' {
                    in_string = false;
                    let lexeme = Lexeme{
                        kind: LexemeKind::StringLiteral(buffer.clone()),
                        line
                    };
                    lexemes.push(lexeme);
                    buffer.clear();
                } else {
                    buffer.push(c);
                }

                continue;
            }

            if should_skip {
                should_skip = false;
                continue;
            }

            if c.is_whitespace() {
                complete_token(&mut buffer, &mut lexemes, line)?;
                continue;
            }

            let peeked = &self[i..i+2];
            if RECOGNIZED_NON_ALPHANUM_SYMBOLS.contains(&peeked) {
                complete_token(&mut buffer, &mut lexemes, line)?;
                let lexeme = peeked.to_string().to_lexeme(line)?;
                lexemes.push(lexeme);
                buffer.clear();
                should_skip = true;
                continue;
            }

            if c == '"' {
                if !buffer.is_empty() {
                    complete_token(&mut buffer, &mut lexemes, line)?;    
                }
                in_string = true;
                line_string_start = line;
                continue;
            }
        
            if RECOGNIZED_NON_ALPHANUM_SYMBOLS.contains(&c.to_string().as_str()) {
                complete_token(&mut buffer, &mut lexemes, line)?;
                let lexeme = c.to_string().to_lexeme(line)?;
                lexemes.push(lexeme);
                continue;
            }
        
            buffer.push(c);
        }

        // if by the end, we are still inside a string literal, thats an error
        if in_string {
            return Err(Error{
                message: String::from("unclosed string literal"),
                lines: vec![line_string_start],
            })
        }
        
        // find all instances of [NumberLiteral, Period, NumberLiteral] and turn them to
	    // NumberLiteral with fractional component

        // vec that holds the indexes to replace
        let mut to_replace = Vec::<usize>::new();

        // vec that holds new number literals
        let mut number_literals = Vec::<Lexeme>::new();

        // find the things to replace by index
        if lexemes.len() >= 2 {
            for (i, _) in lexemes[..lexemes.len()-2].iter().enumerate() {
                let lexeme_kinds = &lexemes[i..i+3]
                    .iter()
                    .map(|x| x.kind.clone())
                    .collect::<Vec<LexemeKind>>();
                let line_number = lexemes[i].line;
                        
                if let [LexemeKind::NumberLiteral(integral), LexemeKind::Period, LexemeKind::NumberLiteral(fractional)] = lexeme_kinds.as_slice() {
                    let new_number_literal = Lexeme{
                        kind: LexemeKind::NumberLiteral(format!("{}.{}", integral, fractional)),
                        line: line_number
                    };
                    to_replace.push(i);
                    number_literals.push(new_number_literal);
                }
            }

            // replace them
            to_replace.reverse();
            number_literals.reverse();
            for (i, num) in to_replace.iter().enumerate() {
                lexemes.drain(num..&(num+3));        
                lexemes.insert(*num, number_literals[i].clone());
            }    
        }
        
        // check if brackets are mismatched
        // let mut bracket_counter = 0;
        let mut left_bracket_stack = Vec::<Lexeme>::new();
        for lexeme in lexemes.iter() {
            match lexeme.kind {
                LexemeKind::LeftParen | LexemeKind::LeftBrace | LexemeKind::LeftBracket => {
                    left_bracket_stack.push(lexeme.clone());
                    // println!("found {:?}", lexeme.kind);
                },
                
                LexemeKind::RightParen | LexemeKind::RightBrace | LexemeKind::RightBracket => {
                    let top = left_bracket_stack
                        .last()
                        .ok_or(Error {
                            message: String::from("mismatched brackets"),
                            lines: vec![lexeme.line],
                        })?.clone();
                    
                    if top.kind == lexeme.kind.get_matching_bracket().unwrap() {
                        left_bracket_stack.pop();
                    } else {
                        let mut lines = vec![top.line, lexeme.line];
                        lines.sort();
                        lines.dedup();
                        
                        return Err(Error {
                            message: String::from("mismatched brackets"),
                            lines
                        })
                    }
                },
                
                _ => ()
            }

            
        }

        if !left_bracket_stack.is_empty() {
            let mut lines = left_bracket_stack.iter().map(|x| x.line).collect::<Vec<usize>>();
            lines.sort();
            lines.dedup();
            
            return Err(Error{
                message: String::from("mismatched brackets"),
                lines,
            })            
        }

        lexemes.insert(0, Lexeme {
            kind: LexemeKind::LeftBrace,
            line: 0
        });
        
        lexemes.push(Lexeme {
            kind: LexemeKind::RightBrace,
            line: 0
        });
        
        Ok(lexemes)
    }
}
