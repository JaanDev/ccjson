use std::{
    collections::HashMap, error::Error, fmt, fs, iter::Peekable, path::Path, slice::Windows,
    sync::LazyLock,
};

use regex::Regex;

use test_each_file::test_each_path;

#[derive(Debug, PartialEq)]
pub enum JsonValue {
    String(String),
    Integer(i64),
    Float(f64),
    Object(HashMap<String, JsonValue>),
    Array(Vec<JsonValue>),
    Boolean(bool),
    Null,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
enum TokenType {
    CurvedBraceOpen,
    CurvedBraceClose,
    SquareBraceOpen,
    SquareBraceClose,
    String,
    Colon,
    Integer,
    Float,
    Bool,
    Comma,
    Null,
    EOF,
}

// if there are >= MAX_DEPTH nested objects/arrays, a JsonError will be returned
const MAX_DEPTH: u8 = 20;

#[derive(Debug)]
struct Token<'a> {
    token_type: TokenType,
    data: &'a str,
}

impl<'a> TryFrom<&Token<'a>> for String {
    type Error = Box<dyn Error>;

    fn try_from(value: &Token<'a>) -> Result<Self, Self::Error> {
        match value.token_type {
            TokenType::String => {
                let data = &value.data[1..value.data.len() - 1];
                let mut escape_next = false;

                let mut res = String::new();

                let mut iter = data.chars();

                while let Some(c) = iter.next() {
                    let cp = c as u32;
                    if let 0x00..=0x1F = cp {
                        return Err(Box::new(JsonError::UnescapedChar { c: cp as u8 }));
                    }

                    if escape_next {
                        match c {
                            '"' | '\\' | '/' => {
                                res.push(c);
                            }
                            'b' => res.push(0x08 as char),
                            'f' => res.push(0x0C as char),
                            'n' => res.push(0x0A as char),
                            'r' => res.push(0x0D as char),
                            't' => res.push(0x09 as char),
                            'u' => {
                                let mut charcode = String::new();

                                for _ in 0..4 {
                                    charcode.push(
                                        iter.next()
                                            .ok_or(Box::new(JsonError::InvalidEscapeChar { c }))?,
                                    );
                                }

                                let cp = u32::from_str_radix(&charcode, 16)?;
                                let encoded_char = char::from_u32(cp).ok_or(Box::new(
                                    JsonError::InvalidEscapeCodepoint { cp: charcode },
                                ))?;
                                res.push(encoded_char);
                            }
                            _ => return Err(Box::new(JsonError::InvalidEscapeChar { c })),
                        };
                        escape_next = false;
                    } else {
                        match c {
                            '\\' => escape_next = true,
                            _ => {
                                res.push(c);
                            }
                        }
                    }
                }

                Ok(res)
            }
            _ => Err(Box::new(JsonError::WrongToken {
                got: value.token_type.clone(),
            })),
        }
    }
}

impl<'a> TryFrom<&Token<'a>> for i64 {
    type Error = Box<dyn Error>;

    fn try_from(value: &Token<'a>) -> Result<Self, Self::Error> {
        match value.token_type {
            TokenType::Integer => Ok(value.data.parse()?),
            _ => Err(Box::new(JsonError::WrongToken {
                got: value.token_type.clone(),
            })),
        }
    }
}

impl<'a> TryFrom<&Token<'a>> for f64 {
    type Error = Box<dyn Error>;

    fn try_from(value: &Token<'a>) -> Result<Self, Self::Error> {
        match value.token_type {
            TokenType::Float => Ok(value.data.parse()?),
            _ => Err(Box::new(JsonError::WrongToken {
                got: value.token_type.clone(),
            })),
        }
    }
}

impl<'a> TryFrom<&Token<'a>> for bool {
    type Error = Box<dyn Error>;

    fn try_from(value: &Token<'a>) -> Result<Self, Self::Error> {
        match value.token_type {
            TokenType::Bool => match value.data {
                "true" => Ok(true),
                "false" => Ok(false),
                _ => unreachable!(),
            },
            _ => Err(Box::new(JsonError::WrongToken {
                got: value.token_type.clone(),
            })),
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd)]
enum ParserState {
    Key,
    Value,
}

#[derive(Debug)]
enum JsonError {
    WrongToken { got: TokenType },
    ParsingError { line: usize, col: usize },
    TokenParsingError { cause: &'static str, data: String },
    MultipleKeys(String),
    WrongKeyword(String),
    InvalidEscapeChar { c: char },
    InvalidEscapeCodepoint { cp: String },
    UnescapedChar { c: u8 },
    MaxDepthExceeded,
    EmptyFile,
}

impl fmt::Display for JsonError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            JsonError::WrongToken { got } => {
                write!(f, "Wrong token: got {got:?}")
            }
            JsonError::ParsingError { line, col } => {
                write!(f, "Parsing error at {line}:{col}")
            }
            JsonError::TokenParsingError { cause, data } => {
                write!(f, "Token parsing error: {cause} on: {data}")
            }
            JsonError::MultipleKeys(key) => {
                write!(f, "Cannot have multiple keys: \"{key}\"")
            }
            JsonError::WrongKeyword(keyword) => {
                write!(f, "Invalid keyword: \"{keyword}\"")
            }
            JsonError::InvalidEscapeChar { c } => {
                write!(f, "Invalid escape char: \"\\{c}\"")
            }
            JsonError::InvalidEscapeCodepoint { cp } => {
                write!(f, "Invalid escape codepoint: 0x{cp}")
            }
            JsonError::MaxDepthExceeded => {
                write!(f, "Max depth of {MAX_DEPTH} nested objects/arrays exceeded")
            }
            JsonError::UnescapedChar { c } => {
                write!(f, "Unescaped char 0x{c:X}")
            }
            JsonError::EmptyFile => write!(f, "Empty file!"),
        }
    }
}

impl Error for JsonError {}

type TokensIter<'a> = Peekable<Windows<'a, Token<'a>>>;

impl JsonValue {
    fn process_value(s: &str) -> Result<TokenType, Box<dyn Error>> {
        static INTEGER_REGEX: LazyLock<Regex> =
            LazyLock::new(|| Regex::new(r"^-?([1-9]\d*|0)$").unwrap());
        static FLOATING_POINT_REGEX: LazyLock<Regex> =
            LazyLock::new(|| Regex::new(r"^-?([1-9]\d*|0)\.\d+([eE][\-+]?\d+)?$").unwrap());
        static FLOATING_POINT_REGEX_2: LazyLock<Regex> =
            LazyLock::new(|| Regex::new(r"^-?([1-9]\d*|0)[eE][\-+]?\d+$").unwrap());

        match s {
            "true" | "false" => Ok(TokenType::Bool),
            "null" => Ok(TokenType::Null),
            _ => {
                if INTEGER_REGEX.is_match(s) {
                    Ok(TokenType::Integer)
                } else if FLOATING_POINT_REGEX.is_match(s) || FLOATING_POINT_REGEX_2.is_match(s) {
                    Ok(TokenType::Float)
                } else {
                    Err(Box::new(JsonError::WrongKeyword(s.to_string())))
                }
            }
        }
    }

    fn parse_tokens(str: &str) -> Result<Vec<Token<'_>>, Box<dyn Error>> {
        let mut tokens: Vec<Token> = Vec::new();

        let mut cur_type = TokenType::CurvedBraceOpen;
        let mut cur_start = 0;
        let mut escape_next = false;
        let mut cur_keyword: Option<usize> = None;

        for (i, c) in str.char_indices() {
            if c.is_whitespace() {
                continue;
            }

            match c {
                '{' if cur_type != TokenType::String => {
                    tokens.push(Token {
                        token_type: TokenType::CurvedBraceOpen,
                        data: &str[i..=i],
                    });
                }
                '}' if cur_type != TokenType::String => {
                    if let Some(cur_keyword_start) = cur_keyword {
                        let keyword_slice = &str[cur_keyword_start..i].trim_end();
                        tokens.push(Token {
                            token_type: Self::process_value(keyword_slice)?,
                            data: keyword_slice,
                        });
                        cur_keyword = None;
                    }
                    if cur_type == TokenType::String {
                        continue;
                    }
                    tokens.push(Token {
                        token_type: TokenType::CurvedBraceClose,
                        data: &str[i..=i],
                    });
                }
                '[' if cur_type != TokenType::String => {
                    tokens.push(Token {
                        token_type: TokenType::SquareBraceOpen,
                        data: &str[i..=i],
                    });
                }
                ']' if cur_type != TokenType::String => {
                    if let Some(cur_keyword_start) = cur_keyword {
                        let keyword_slice = &str[cur_keyword_start..i].trim_end();
                        tokens.push(Token {
                            token_type: Self::process_value(keyword_slice)?,
                            data: keyword_slice,
                        });
                        cur_keyword = None;
                    }
                    tokens.push(Token {
                        token_type: TokenType::SquareBraceClose,
                        data: &str[i..=i],
                    });
                }
                '"' => {
                    if escape_next {
                        escape_next = false;
                        continue;
                    }
                    match cur_type {
                        TokenType::String => {
                            tokens.push(Token {
                                token_type: TokenType::String,
                                data: &str[cur_start..=i],
                            });
                            cur_type = TokenType::CurvedBraceOpen;
                        }
                        _ => {
                            cur_type = TokenType::String;
                            cur_start = i;
                        }
                    }
                }
                '\\' => {
                    escape_next = !escape_next;
                }
                ',' if cur_type != TokenType::String => {
                    if let Some(cur_keyword_start) = cur_keyword {
                        let keyword_slice = &str[cur_keyword_start..i].trim_end();
                        tokens.push(Token {
                            token_type: Self::process_value(keyword_slice)?,
                            data: keyword_slice,
                        });
                        cur_keyword = None;
                    }
                    tokens.push(Token {
                        token_type: TokenType::Comma,
                        data: &str[i..=i],
                    });
                }
                ':' if cur_type != TokenType::String => {
                    tokens.push(Token {
                        token_type: TokenType::Colon,
                        data: &str[i..=i],
                    });
                }
                _ => {
                    escape_next = false;
                    if cur_type != TokenType::String && cur_keyword.is_none() {
                        cur_keyword = Some(i);
                    }
                }
            }
        }

        tokens.push(Token {
            token_type: TokenType::EOF,
            data: "",
        });

        Ok(tokens)
    }

    fn process_object(tokens: &mut TokensIter, depth: u8) -> Result<JsonValue, Box<dyn Error>> {
        if depth >= MAX_DEPTH {
            return Err(Box::new(JsonError::MaxDepthExceeded));
        }
        let mut items = HashMap::new();
        let mut state = ParserState::Key;
        let mut cur_key = String::new();

        // println!("enter obj");

        while let Some(token_pair) = tokens.next() {
            let (cur_token, next_token) = match token_pair {
                [cur_token, next_token] => (cur_token, next_token),
                _ => unreachable!(),
            };

            // println!("obj {cur_token:?} {next_token:?}");

            let next_type = &next_token.token_type;

            let is_pair_ok = match cur_token.token_type {
                TokenType::CurvedBraceOpen => {
                    next_type == &TokenType::CurvedBraceClose || next_type == &TokenType::String
                }
                TokenType::CurvedBraceClose => match next_type {
                    TokenType::SquareBraceClose
                    | TokenType::Comma
                    | TokenType::CurvedBraceClose
                    | TokenType::EOF => break,
                    _ => false,
                },
                TokenType::String => {
                    let string_value = String::try_from(cur_token)?;
                    match state {
                        ParserState::Key => {
                            cur_key = string_value;
                            state = ParserState::Value;
                            next_type == &TokenType::Colon
                        }
                        ParserState::Value => {
                            if items
                                .insert(cur_key.clone(), JsonValue::String(string_value))
                                .is_some()
                            {
                                return Err(Box::new(JsonError::MultipleKeys(cur_key)));
                            }
                            state = ParserState::Key;
                            next_type == &TokenType::Comma
                                || next_type == &TokenType::CurvedBraceClose
                        }
                    }
                }
                TokenType::Colon => match state {
                    ParserState::Key => false,
                    ParserState::Value => match next_type {
                        TokenType::CurvedBraceOpen => {
                            let new_object = Self::process_object(tokens, depth + 1)?;
                            if items.insert(cur_key.clone(), new_object).is_some() {
                                return Err(Box::new(JsonError::MultipleKeys(cur_key)));
                            }

                            state = ParserState::Key;
                            true
                        }
                        TokenType::SquareBraceOpen => {
                            let new_array = Self::process_array(tokens, depth + 1)?;
                            if items.insert(cur_key.clone(), new_array).is_some() {
                                return Err(Box::new(JsonError::MultipleKeys(cur_key)));
                            }

                            state = ParserState::Key;
                            true
                        }
                        TokenType::SquareBraceClose
                        | TokenType::CurvedBraceClose
                        | TokenType::Comma
                        | TokenType::Colon => false,
                        _ => true,
                    },
                },
                TokenType::Comma => state == ParserState::Key && next_type == &TokenType::String,
                TokenType::Bool => {
                    let new_value = JsonValue::Boolean(bool::try_from(cur_token)?);
                    match state {
                        ParserState::Key => false,
                        ParserState::Value => {
                            if items.insert(cur_key.clone(), new_value).is_some() {
                                return Err(Box::new(JsonError::MultipleKeys(cur_key)));
                            }
                            state = ParserState::Key;
                            next_type == &TokenType::Comma
                                || next_type == &TokenType::CurvedBraceClose
                        }
                    }
                }
                TokenType::Null => {
                    let new_value = JsonValue::Null;
                    match state {
                        ParserState::Key => false,
                        ParserState::Value => {
                            if items.insert(cur_key.clone(), new_value).is_some() {
                                return Err(Box::new(JsonError::MultipleKeys(cur_key)));
                            }
                            state = ParserState::Key;
                            next_type == &TokenType::Comma
                                || next_type == &TokenType::CurvedBraceClose
                        }
                    }
                }
                TokenType::Integer => {
                    let new_value = JsonValue::Integer(i64::try_from(cur_token)?);
                    match state {
                        ParserState::Key => false,
                        ParserState::Value => {
                            if items.insert(cur_key.clone(), new_value).is_some() {
                                return Err(Box::new(JsonError::MultipleKeys(cur_key)));
                            }
                            state = ParserState::Key;
                            next_type == &TokenType::Comma
                                || next_type == &TokenType::CurvedBraceClose
                        }
                    }
                }
                TokenType::Float => {
                    let new_value = JsonValue::Float(f64::try_from(cur_token)?);
                    match state {
                        ParserState::Key => false,
                        ParserState::Value => {
                            if items.insert(cur_key.clone(), new_value).is_some() {
                                return Err(Box::new(JsonError::MultipleKeys(cur_key)));
                            }
                            state = ParserState::Key;
                            next_type == &TokenType::Comma
                                || next_type == &TokenType::CurvedBraceClose
                        }
                    }
                }
                _ => todo!("{:?}", cur_token.token_type),
            };

            if !is_pair_ok {
                return Err(Box::new(JsonError::WrongToken {
                    got: cur_token.token_type.clone(),
                }));
            }
        }

        // println!("obj exit");

        Ok(JsonValue::Object(items))
    }

    fn process_array(tokens: &mut TokensIter, depth: u8) -> Result<JsonValue, Box<dyn Error>> {
        if depth >= MAX_DEPTH {
            return Err(Box::new(JsonError::MaxDepthExceeded));
        }
        let mut items = Vec::new();

        while let Some(token_pair) = tokens.next() {
            let (cur_token, next_token) = match token_pair {
                [cur_token, next_token] => (cur_token, next_token),
                _ => unreachable!(),
            };

            let next_type = &next_token.token_type;

            let is_pair_ok = match cur_token.token_type {
                TokenType::SquareBraceOpen => match next_type {
                    TokenType::SquareBraceClose
                    | TokenType::String
                    | TokenType::Integer
                    | TokenType::Float
                    | TokenType::Bool
                    | TokenType::Null => true,
                    TokenType::CurvedBraceOpen => {
                        let new_obj = Self::process_object(tokens, depth + 1)?;
                        items.push(new_obj);
                        true
                    }
                    TokenType::SquareBraceOpen => {
                        let new_arr = Self::process_array(tokens, depth + 1)?;
                        items.push(new_arr);
                        true
                    }
                    _ => false,
                },
                TokenType::SquareBraceClose => match next_type {
                    TokenType::SquareBraceClose
                    | TokenType::Comma
                    | TokenType::CurvedBraceClose
                    | TokenType::EOF => break,
                    _ => false,
                },
                TokenType::String => {
                    let new_value = JsonValue::String(String::try_from(cur_token)?);
                    items.push(new_value);
                    next_type == &TokenType::Comma || next_type == &TokenType::SquareBraceClose
                }
                TokenType::Comma => match next_type {
                    TokenType::String
                    | TokenType::Integer
                    | TokenType::Float
                    | TokenType::Bool
                    | TokenType::Null => true,
                    TokenType::CurvedBraceOpen => {
                        let new_obj = Self::process_object(tokens, depth + 1)?;
                        items.push(new_obj);
                        true
                    }
                    TokenType::SquareBraceOpen => {
                        let new_arr = Self::process_array(tokens, depth + 1)?;
                        items.push(new_arr);
                        true
                    }
                    _ => false,
                },
                TokenType::Bool => {
                    let new_value = JsonValue::Boolean(bool::try_from(cur_token)?);
                    items.push(new_value);
                    next_type == &TokenType::Comma || next_type == &TokenType::SquareBraceClose
                }
                TokenType::Null => {
                    let new_value = JsonValue::Null;
                    items.push(new_value);
                    next_type == &TokenType::Comma || next_type == &TokenType::SquareBraceClose
                }
                TokenType::Integer => {
                    let new_value = JsonValue::Integer(i64::try_from(cur_token)?);
                    items.push(new_value);
                    next_type == &TokenType::Comma || next_type == &TokenType::SquareBraceClose
                }
                TokenType::Float => {
                    let new_value = JsonValue::Float(f64::try_from(cur_token)?);
                    items.push(new_value);
                    next_type == &TokenType::Comma || next_type == &TokenType::SquareBraceClose
                }
                _ => todo!("{:?}", cur_token.token_type),
            };

            if !is_pair_ok {
                return Err(Box::new(JsonError::WrongToken {
                    got: cur_token.token_type.clone(),
                }));
            }
        }

        Ok(JsonValue::Array(items))
    }

    pub fn build_from_string(str: &str) -> Result<Self, Box<dyn Error>> {
        let tokens = Self::parse_tokens(str)?;

        let mut tokens = tokens.windows(2).peekable();

        if tokens.len() == 0 {
            return Err(Box::new(JsonError::EmptyFile)); // todo
        }

        // first pair (cur,next) in tokens should have cur contain start brace of element.

        let node = match tokens.peek().unwrap() {
            [first_token, _] => match &first_token.token_type {
                TokenType::SquareBraceOpen => Self::process_array(&mut tokens, 1)?,
                TokenType::CurvedBraceOpen => Self::process_object(&mut tokens, 1)?,
                other => return Err(Box::new(JsonError::WrongToken { got: other.clone() })),
            },
            _ => unreachable!(),
        };

        if let Some(token_pair) = tokens.next() {
            return Err(Box::new(JsonError::WrongToken {
                got: token_pair[0].token_type.clone(),
            }));
        }

        Ok(node)
    }

    pub fn build_from_file(filename: &Path) -> Result<Self, Box<dyn Error>> {
        let file_data = fs::read_to_string(filename)?;
        Self::build_from_string(&file_data)
    }
}

#[cfg(test)]
pub mod tests {
    use crate::*;

    #[test]
    pub fn test_step1_invalid() {
        println!(
            "{}",
            JsonValue::build_from_file(Path::new("tests_data/step1/invalid.json")).unwrap_err()
        );
    }

    #[test]
    pub fn test_step1_valid() {
        assert_eq!(
            JsonValue::build_from_file(Path::new("tests_data/step1/valid.json")).unwrap(),
            JsonValue::Object(HashMap::new())
        );
    }

    #[test]
    pub fn test_step2_invalid() {
        println!(
            "{}",
            JsonValue::build_from_file(Path::new("tests_data/step2/invalid.json")).unwrap_err()
        );
    }

    #[test]
    pub fn test_step2_invalid2() {
        println!(
            "{}",
            JsonValue::build_from_file(Path::new("tests_data/step2/invalid2.json")).unwrap_err()
        );
    }

    #[test]
    pub fn test_step2_valid() {
        assert_eq!(
            JsonValue::build_from_file(Path::new("tests_data/step2/valid.json")).unwrap(),
            JsonValue::Object(HashMap::from([(
                "key".to_string(),
                JsonValue::String("value".to_string())
            )]))
        );
    }

    #[test]
    pub fn test_step2_valid2() {
        assert_eq!(
            JsonValue::build_from_file(Path::new("tests_data/step2/valid2.json")).unwrap(),
            JsonValue::Object(HashMap::from([
                ("key".to_string(), JsonValue::String("value".to_string())),
                ("key2".to_string(), JsonValue::String("value".to_string()))
            ]))
        );
    }

    #[test]
    pub fn test_step3_invalid() {
        println!(
            "{}",
            JsonValue::build_from_file(Path::new("tests_data/step3/invalid.json")).unwrap_err()
        );
    }

    #[test]
    pub fn test_step3_valid() {
        assert_eq!(
            JsonValue::build_from_file(Path::new("tests_data/step3/valid.json")).unwrap(),
            JsonValue::Object(HashMap::from([
                ("key1".to_string(), JsonValue::Boolean(true)),
                ("key2".to_string(), JsonValue::Boolean(false)),
                ("key3".to_string(), JsonValue::Null),
                ("key4".to_string(), JsonValue::String("value".to_string())),
                ("key5".to_string(), JsonValue::Integer(101)),
            ]))
        );
    }

    #[test]
    pub fn test_float_parsing() {
        for s in vec![
            "0.0",
            "-123.123",
            "-0.123",
            "-10e10",
            "15.0123e000",
            "15.0213e+12",
            "12.3231e-0004",
        ] {
            let t = Token {
                token_type: TokenType::Float,
                data: s,
            };

            let res: f64 = (&t).try_into().unwrap();

            println!("Passed: {s} -> {res}");
        }
    }

    #[test]
    pub fn test_step4_invalid() {
        println!(
            "{}",
            JsonValue::build_from_file(Path::new("tests_data/step4/invalid.json")).unwrap_err()
        );
    }

    #[test]
    pub fn test_step4_valid() {
        assert_eq!(
            JsonValue::build_from_file(Path::new("tests_data/step4/valid.json")).unwrap(),
            JsonValue::Object(HashMap::from([
                ("key".to_string(), JsonValue::String("value".to_string())),
                ("key-n".to_string(), JsonValue::Integer(101)),
                ("key-o".to_string(), JsonValue::Object(HashMap::new())),
                ("key-l".to_string(), JsonValue::Array(Vec::new())),
            ]))
        );
    }

    #[test]
    pub fn test_step4_valid2() {
        assert_eq!(
            JsonValue::build_from_file(Path::new("tests_data/step4/valid2.json")).unwrap(),
            JsonValue::Object(HashMap::from([
                ("key".to_string(), JsonValue::String("value".to_string())),
                ("key-n".to_string(), JsonValue::Integer(101)),
                (
                    "key-o".to_string(),
                    JsonValue::Object(HashMap::from([((
                        "inner key".to_string(),
                        JsonValue::String("inner value".to_string())
                    ))]))
                ),
                (
                    "key-l".to_string(),
                    JsonValue::Array(vec![JsonValue::String("list value".to_string())])
                ),
            ]))
        );
    }

    test_each_path! { in "tests_data/test5/test/fails" as test5 => test5  }
    fn test5(filename: &Path) {
        println!("{}", JsonValue::build_from_file(filename).unwrap_err());
    }

    #[test]
    pub fn test_step5_pass1() {
        use JsonValue::*;

        let value = Array(vec![
            String("JSON Test Pattern pass1".to_string()),
            Object(HashMap::from([
                (
                    "object with 1 member".to_string(),
                    Array(vec![
                        String("array with 1 element".to_string()),
                    ]),
                ),
            ])),
            Object(HashMap::new()),
            Array(vec![]),
            Integer(-42),
            Boolean(true),
            Boolean(false),
            Null,
            Object(HashMap::from([
                ("integer".to_string(), Integer(1234567890)),
                ("real".to_string(), Float(-9876.543210)),
                ("e".to_string(), Float(0.123456789e-12)),
                ("E".to_string(), Float(1.234567890E+34)),
                ("".to_string(), Float(23456789012E66)),
                ("zero".to_string(), Integer(0)),
                ("one".to_string(), Integer(1)),
                ("space".to_string(), String(" ".to_string())),
                ("quote".to_string(), String("\"".to_string())),
                ("backslash".to_string(), String("\\".to_string())),
                ("controls".to_string(), String("\u{0008}\u{000C}\n\r\t".to_string())),
                ("slash".to_string(), String("/ & /".to_string())),
                ("alpha".to_string(), String("abcdefghijklmnopqrstuvwyz".to_string())),
                ("ALPHA".to_string(), String("ABCDEFGHIJKLMNOPQRSTUVWYZ".to_string())),
                ("digit".to_string(), String("0123456789".to_string())),
                ("0123456789".to_string(), String("digit".to_string())),
                ("special".to_string(), String("`1~!@#$%^&*()_+-={':[,]}|;.</>?".to_string())),
                ("hex".to_string(), String("\u{0123}\u{4567}\u{89AB}\u{CDEF}\u{ABCD}\u{EF4A}".to_string())),
                ("true".to_string(), Boolean(true)),
                ("false".to_string(), Boolean(false)),
                ("null".to_string(), Null),
                ("array".to_string(), Array(vec![])),
                ("object".to_string(), Object(HashMap::new())),
                ("address".to_string(), String("50 St. James Street".to_string())),
                ("url".to_string(), String("http://www.JSON.org/".to_string())),
                ("comment".to_string(), String("// /* <!-- --".to_string())),
                ("# -- --> */".to_string(), String(" ".to_string())),
                (
                    " s p a c e d ".to_string(),
                    Array(vec![
                        Integer(1),
                        Integer(2),
                        Integer(3),
                        Integer(4),
                        Integer(5),
                        Integer(6),
                        Integer(7),
                    ]),
                ),
                (
                    "compact".to_string(),
                    Array(vec![
                        Integer(1),
                        Integer(2),
                        Integer(3),
                        Integer(4),
                        Integer(5),
                        Integer(6),
                        Integer(7),
                    ]),
                ),
                (
                    "jsontext".to_string(),
                    String("{\"object with 1 member\":[\"array with 1 element\"]}".to_string()),
                ),
                (
                    "quotes".to_string(),
                    String("&#34; \" %22 0x22 034 &#x22;".to_string()),
                ),
                (
                    "/\\\"\u{CAFE}\u{BABE}\u{AB98}\u{FCDE}\u{BCDA}\u{EF4A}\u{0008}\u{000C}\n\r\t`1~!@#$%^&*()_+-=[]{}|;:',./<>?"
                        .to_string(),
                    String("A key can be any string".to_string()),
                ),
            ])),
            Float(0.5),
            Float(98.6),
            Float(99.44),
            Integer(1066),
            Float(1e1),
            Float(0.1e1),
            Float(1e-1),
            Float(1e00),
            Float(2e+00),
            Float(2e-00),
            String("rosebud".to_string()),
        ]);

        assert_eq!(
            JsonValue::build_from_file(Path::new("tests_data/test5/test/pass1.json")).unwrap(),
            value
        );
    }

    #[test]
    fn test_step5_pass2() {
        use JsonValue::*;

        let value = Array(vec![Array(vec![Array(vec![Array(vec![Array(vec![
            Array(vec![Array(vec![Array(vec![Array(vec![Array(vec![
                Array(vec![Array(vec![Array(vec![Array(vec![Array(vec![
                    Array(vec![Array(vec![Array(vec![Array(vec![String(
                        "Not too deep".to_string(),
                    )])])])]),
                ])])])])]),
            ])])])])]),
        ])])])])]);

        assert_eq!(
            JsonValue::build_from_file(Path::new("tests_data/test5/test/pass2.json")).unwrap(),
            value
        );
    }

    #[test]
    fn test_step5_pass3() {
        use JsonValue::*;

        let value = Object(HashMap::from([(
            "JSON Test Pattern pass3".to_string(),
            Object(HashMap::from([
                (
                    "The outermost value".to_string(),
                    String("must be an object or array.".to_string()),
                ),
                (
                    "In this test".to_string(),
                    String("It is an object.".to_string()),
                ),
            ])),
        )]));

        assert_eq!(
            JsonValue::build_from_file(Path::new("tests_data/test5/test/pass3.json")).unwrap(),
            value
        );
    }
}
