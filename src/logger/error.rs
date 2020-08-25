use super::Color;
use crate::helpers::Pos;

#[derive(Debug, Clone, Copy, PartialEq)]
/// An error type, i.e `Syntax` error or `UnexpectedToken` error
pub enum ErrorType {
    Syntax,
    UnexpectedToken,
    UnterminatedString,
    UnknownCharacter,
    UndefinedSyntax,
    SyntaxTypeError,
    UndefinedTypeError,
    TypeMismatch,
    UndefinedSymbol,
    TypeCastError,
    PossibleUninitVal,
    ScopeError,
    InferError,
    VisibilityError,
    ImportError,
}

impl ErrorType {
    pub fn as_str(&self) -> &str {
        match *self {
            ErrorType::Syntax => "syntax_error",
            ErrorType::UnexpectedToken => "unexpected_token",
            ErrorType::UnterminatedString => "unterminated_string",
            ErrorType::UnknownCharacter => "unknown_character",
            ErrorType::UndefinedSyntax => "undefined_syntax",
            ErrorType::SyntaxTypeError => "syntax_type",
            ErrorType::UndefinedTypeError => "undefined_type",
            ErrorType::TypeMismatch => "type_mismatch",
            ErrorType::UndefinedSymbol => "undefined_symbol",
            ErrorType::TypeCastError => "type_case",
            ErrorType::PossibleUninitVal => "possible_initialized",
            ErrorType::VisibilityError => "visibility_error",
            ErrorType::ImportError => "import_error",
            ErrorType::InferError => "infer_error",
            ErrorType::ScopeError => "scope_error",
        }
    }
}

#[derive(Debug, Clone, Copy)]
/// Error display mode
pub enum ErrorDisplayType {
    /// "Error" mode, make underline red and text red
    Error,
    /// "Warning" mode, make underline yellow and text yellow
    Warning,
    /// "Info" mode, make underline Blue and text blue
    Info,
}

impl ErrorDisplayType {
    pub fn plural(&self) -> &str {
        match self {
            ErrorDisplayType::Error => "Errors",
            ErrorDisplayType::Warning => "Warnings",
            _ => "Errors",
        }
    }

    pub fn singular(&self) -> &str {
        match self {
            ErrorDisplayType::Error => "Error",
            ErrorDisplayType::Warning => "Warning",
            _ => "Error",
        }
    }

    pub fn get_underline(&self) -> &str {
        match self {
            ErrorDisplayType::Error => "^",
            ErrorDisplayType::Warning => "~",
            ErrorDisplayType::Info => "-",
        }
    }

    pub fn get_color(self) -> &'static str {
        match self {
            ErrorDisplayType::Error => Color::Red,
            ErrorDisplayType::Warning => Color::Yellow,
            ErrorDisplayType::Info => Color::Blue,
        }
        .as_str()
    }

    pub fn get_color_class(self) -> Color {
        match self {
            ErrorDisplayType::Error => Color::Red,
            ErrorDisplayType::Warning => Color::Yellow,
            ErrorDisplayType::Info => Color::Blue,
        }
    }
}

#[derive(Debug, Clone, Copy)]
/// For incremental error reporting so we don't have weird unnecessary errors caused by another error.
/// I.e. so we don't' have a undefined variable error because of a type error in the declaration.
pub enum ErrorLevel {
    NonExistentVar = 0,
    NonExistentFunc = 1,
    NonExistentType = 2,
    TypeError = 3,
    CoreError = 4,
}

#[derive(Debug, Clone)]
pub enum ErrorOrVec {
    Error(ErrorValue, ErrorLevel),
    ErrorVec(Vec<(ErrorValue, ErrorLevel)>),
}

impl ErrorOrVec {
    pub fn unwrap_error(self) -> (ErrorValue, ErrorLevel) {
        match self {
            ErrorOrVec::Error(e, level) => (e, level),
            ErrorOrVec::ErrorVec(_) => panic!("Tried to unwrap ErrorVec value"),
        }
    }

    pub fn unwrap_vec(self) -> Vec<(ErrorValue, ErrorLevel)> {
        match self {
            ErrorOrVec::Error(_, _) => panic!("Tried to unwrap Error value"),
            ErrorOrVec::ErrorVec(e) => e,
        }
    }

    pub fn as_vec(self) -> Vec<(ErrorValue, ErrorLevel)> {
        match self {
            ErrorOrVec::Error(e, level) => vec![(e, level)],
            ErrorOrVec::ErrorVec(e) => e,
        }
    }
}

#[derive(Debug, Clone)]
/// Underlines and such
pub struct ErrorAnnotation {
    /// Error message
    pub message: Option<String>,
    /// Error position
    pub position: Pos,
    /// Error display mode
    pub mode: ErrorDisplayType,
    /// Position
    pub position_rel: ((usize, usize), (usize, usize)),
}

impl ErrorAnnotation {
    /// Returns an error annotation
    ///
    /// Arguments
    ///
    /// * `message`: error message
    /// * `position`: position of error
    /// * `mode`: mode of error report
    /// * `filename`: filename of annotation
    pub fn new(message: Option<String>, position: Pos, mode: ErrorDisplayType) -> ErrorAnnotation {
        ErrorAnnotation {
            message,
            position,
            mode,
            position_rel: ((0, 0), (0, 0)),
        }
    }

    pub fn has_label(&self) -> bool {
        self.message.is_some()
    }
}

#[derive(Debug, Clone)]
/// An full on error containing useful info.
pub struct ErrorValue {
    /// Error message
    pub message: String,
    /// Error type
    pub error: ErrorType,
    /// Error position
    pub position: Pos,
    /// Error display mode
    pub mode: ErrorDisplayType,
    /// Annotations
    pub annotations: Vec<ErrorAnnotation>,
    /// Urgent error: raise even if another function parses further
    pub urgent: bool,
}

impl ErrorValue {
    /// Returns an error object
    ///
    /// # Arguments
    ///
    /// * `message`: error message
    /// * `error`: error type
    /// * `position`: position of error
    /// * `token`: optional token associated with error
    /// * `mode`: mode of error report
    pub fn new<'a, 'b>(
        message: String,
        error: ErrorType,
        position: Pos,
        mode: ErrorDisplayType,
        annotations: Vec<ErrorAnnotation>,
        urgent: bool,
    ) -> ErrorValue {
        ErrorValue {
            message,
            error,
            position,
            mode,
            annotations,
            urgent,
        }
    }

    pub fn is_priority(&self) -> bool {
        self.urgent
    }
}

