use crate::parser::*;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Error<T> {
    pub kind: T,
    pub lines: Vec<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum LexerError {
    UnclosedString,
    MismatchedBrackets,
    
    InvalidSymbol {
        symbol_name: String
    },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ParserError {
    SyntaxError {
        expected: String,
        found: String,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum SemanticError {
    UndeclaredSymbol {
        symbol_name: String,
    },

    RedeclaredSymbol {
        symbol_name: String,
    },

    TypeMismatch {
        expected: String,
        found: String
    },

    InvalidOperation {
        operation: BinaryOperation,
        data_types: [Type; 2]
    },

    NotFunction {
        symbol_name: String
    },

    NotProcedure {
        symbol_name: String
    },

    InvalidArgumentCount {
        expected: usize,
        found: usize,
    },

    ConstantReassignment {
        symbol_name: String
    }
}
