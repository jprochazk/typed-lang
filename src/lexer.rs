use ast2str::AstToStr;
use derive_more::Constructor;
use logos::Logos;
use std::borrow::Cow;

pub use logos::Span;

#[derive(Clone, Debug, PartialEq, Eq, AstToStr, Constructor)]
pub struct Token<'source> {
  #[forward]
  pub lexeme: Cow<'source, str>,
  pub span: Span,
  pub kind: TokenKind, /* <'source> */
}

pub trait NextTokenExt<'source> {
  fn next_token(&mut self) -> Option<Token<'source>>;
}
impl<'source> NextTokenExt<'source> for Lexer<'source> {
  fn next_token(&mut self) -> Option<Token<'source>> {
    self
      .next()
      .map(|token| Token::new(Cow::from(self.slice()), self.span(), token))
  }
}

pub type Lexer<'source> = logos::Lexer<'source, TokenKind /* <'source> */>;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Logos)]
pub enum TokenKind /* <'source> */ {
  #[token("fn")]
  Fn,
  #[token("throw")]
  Throw,
  #[token("try")]
  Try,
  #[token("catch")]
  Catch,
  #[token("finally")]
  Finally,
  #[token("pass")]
  Pass,
  #[token("continue")]
  Continue,
  #[token("break")]
  Break,
  #[token("return")]
  Return,
  #[token("struct")]
  Struct,
  #[token("impl")]
  Impl,
  #[token("trait")]
  Trait,
  #[token("enum")]
  Enum,
  #[token("type")]
  Type,
  #[token("is")]
  Is,
  #[token("none")]
  None,
  #[token("true")]
  True,
  #[token("false")]
  False,
  #[token("+")]
  Plus,
  #[token("-")]
  Minus,
  #[token("*")]
  Star,
  #[token("/")]
  Slash,
  #[token("%")]
  Percent,
  #[token("**")]
  Power,
  #[token("!")]
  #[token("not")]
  Bang,
  #[token("~")]
  Tilde,
  #[token("<")]
  Less,
  #[token(">")]
  More,
  #[token("<<")]
  ShiftLeft,
  #[token(">>")]
  ShiftRight,
  #[token("&")]
  BitAnd,
  #[token("|")]
  BitOr,
  #[token("^")]
  BitXor,
  #[token(":=")]
  Walrus,
  #[token("=")]
  Equal,
  #[token("&&")]
  #[token("and")]
  And,
  #[token("||")]
  #[token("or")]
  Or,
  #[token("&&=")]
  AndEqual,
  #[token("||=")]
  OrEqual,
  #[token("+=")]
  PlusEqual,
  #[token("-=")]
  MinusEqual,
  #[token("*=")]
  StarEqual,
  #[token("/=")]
  SlashEqual,
  #[token("%=")]
  PercentEqual,
  #[token("**=")]
  PowerEqual,
  #[token("==")]
  EqualEqual,
  #[token("!=")]
  BangEqual,
  #[token("<=")]
  LessEqual,
  #[token(">=")]
  MoreEqual,
  #[token("<<=")]
  ShiftLeftEqual,
  #[token(">>=")]
  ShiftRightEqual,
  #[token("&=")]
  BitAndEqual,
  #[token("|=")]
  BitOrEqual,
  #[token("^=")]
  BitXorEqual,
  #[token("->")]
  Arrow,
  #[token("(")]
  LeftParen,
  #[token(")")]
  RightParen,
  #[token("{")]
  LeftBrace,
  #[token("}")]
  RightBrace,
  #[token("[")]
  LeftBracket,
  #[token("]")]
  RightBracket,
  #[token(":")]
  Colon,
  #[token(",")]
  Comma,
  #[token(";")]
  Semi,
  #[token(".")]
  Dot,
  #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
  Identifier,
  #[regex("\"([^\"\\\\]|\\\\.)*\"")]
  #[regex("'([^'\\\\]|\\\\.)*'")]
  String,
  #[regex("[0-9]+[Ee][+-]?[0-9]+|([0-9]+\\.[0-9]+([Ee][+-]?[0-9]+)?)", priority = 2)]
  #[token("NaN")]
  #[token("inf")]
  Float,
  #[regex("[0-9]([0-9_]*[0-9])?")]
  Integer,
  #[regex("(\n[ \t]+)+")]
  Indent,
  #[regex("#[^\n]*", logos::skip)]
  Comment,
  #[token(" ", logos::skip)]
  #[token("\t", logos::skip)]
  #[token("\n", logos::skip)]
  Whitespace,
  #[error]
  Invalid,
}

#[rustfmt::skip]
#[cfg(test)]
mod tests {
  use super::*;
  use pretty_assertions::assert_eq;

  macro_rules! text_lexemes {
    ($name:ident { $a:literal, [] }) => {
      #[test]
      fn $name() {
        let a = {
          let mut lex = TokenKind::lexer(indoc::indoc!($a).trim());
          let mut out = Vec::<&'static str>::new();
          while let Some(_) = lex.next() {
            out.push(lex.slice());
          }
          out
        };
        let b = Vec::<&'static str>::new();
        assert_eq!(a, b);
      }
    };
    ($name:ident { $a:literal, [$($lexeme:literal),*] }) => {
      #[test]
      fn $name() {
        let a = {
          let mut lex = TokenKind::lexer(indoc::indoc!($a).trim());
          let mut out = Vec::<&'static str>::new();
          while let Some(_) = lex.next() {
            out.push(lex.slice());
          }
          out
        };
        let b = vec![$($lexeme),*];
        assert_eq!(a, b);
      }
    };
    ($name:ident [ $({ $a:literal, [$($lexeme:literal),*] })* ]) => {
      #[test]
      fn $name() {
        $(
          {
            let a = {
              let mut lex = TokenKind::lexer(indoc::indoc!($a).trim());
              let mut out = Vec::new();
              while let Some(token) = lex.next() {
                assert!(token != TokenKind::Invalid);
                out.push(lex.slice());
              }
              out
            };
            let b = vec![$($lexeme),*];
            assert_eq!(a, b);
          }
        )*
      }
    };
  }

  #[derive(Debug)]
  struct TestToken<'source>(TokenKind, Option<&'source str>);
  impl<'source> PartialEq<TestToken<'source>> for TestToken<'source> {
    fn eq(&self, other: &TestToken<'source>) -> bool {
      if self.0 != other.0 {
        false
        // both lexemes must exist for them to be compared
      } else if self.1.is_some() && other.1.is_some() && self.1 != other.1 {
        false
      } else {
        true
      }
    }
  }

  fn tokenize<'source>(src: &'source str) -> Vec<TestToken<'source>> {
    let mut lexer = TokenKind::lexer(src);
    let mut out = vec![];
    while let Some(token) = lexer.next() {
      out.push(TestToken(token, Some(lexer.slice())));
    }
    out
  }

  macro_rules! t {
    ($kind:ident) => {
      TestToken(TokenKind::$kind, None)
    };
    ($kind:ident, $lexeme:literal) => {
      TestToken(TokenKind::$kind, Some($lexeme))
    };
    ($kind:ident, $lexeme:literal, $value:expr) => {
      TestToken(TokenKind::$kind($value), Some($lexeme))
    };
  }

  macro_rules! test_tokens {
    ($name:ident { $source:literal, [$($token:expr),*] }) => {
      #[test]
      fn $name() {
        assert_eq!(
          tokenize(indoc::indoc!($source).trim()),
          vec![$($token),*]
        )
      }
    };
  }

  text_lexemes!(arithmetic [
    { "a + b", ["a",  "+", "b"] }
    { "a - b", ["a",  "-", "b"] }
    { "a * b", ["a",  "*", "b"] }
    { "a / b", ["a",  "/", "b"] }
    { "a % b", ["a",  "%", "b"] }
    { "a ** b", ["a", "**", "b"] }
  ]);

  text_lexemes!(unary [
    { "!a", ["!", "a"] }
    { "not a", ["not", "a"] }
    { "~a", ["~", "a"] }
  ]);

  text_lexemes!(bitwise [
    { "a & b", ["a", "&", "b"] }
    { "a | b", ["a", "|", "b"] }
    { "a ^ b", ["a", "^", "b"] }
    { "a << b", ["a", "<<", "b"] }
    { "a >> b", ["a", ">>", "b"] }
  ]);

  text_lexemes!(logical [
    { "a && b", ["a",  "&&", "b"] }
    { "a || b", ["a",  "||", "b"] }
    { "a and b", ["a", "and", "b"] }
    { "a or b", ["a",  "or", "b"] }
  ]);

  text_lexemes!(pairs { "{} () <> []", ["{", "}", "(", ")", "<", ">", "[", "]"] });

  text_lexemes!(comparison [
    { "a == b",  ["a", "==", "b"] }
    { "a != b",  ["a", "!=", "b"] }
    { "a <= b",  ["a", "<=", "b"] }
    { "a >= b",  ["a", ">=", "b"] }
  ]);

  text_lexemes!(assignment [
    { "a = b", ["a", "=", "b"] }
    { "a := b", ["a", ":=", "b"] }
    { "a &&= b", ["a", "&&=", "b"] }
    { "a ||= b", ["a", "||=", "b"] }
    { "a += b",  ["a", "+=", "b"] }
    { "a -= b",  ["a", "-=", "b"] }
    { "a *= b",  ["a", "*=", "b"] }
    { "a /= b",  ["a", "/=", "b"] }
    { "a %= b",  ["a", "%=", "b"] }
    { "a **= b", ["a", "**=", "b"] }
    { "a &= b", ["a", "&=", "b"] }
    { "a |= b", ["a", "|=", "b"] }
    { "a ^= b", ["a", "^=", "b"] }
    { "a <<= b", ["a", "<<=", "b"] }
    { "a >>= b", ["a", ">>=", "b"] }
  ]);

  text_lexemes!(misc { ": , ; -> .", [":", ",", ";", "->", "."] });

  text_lexemes!(identifiers [
    { "asdf", ["asdf"] }
    { "a0s0d0f0", ["a0s0d0f0"] }
    { "asdf_asdf", ["asdf_asdf"] }
    { "a0s0d0f0_a0s0d0f0", ["a0s0d0f0_a0s0d0f0"] }
  ]);

  text_lexemes!(strings [
    { r#""asdf""#, [r#""asdf""#] }
    { r#"'asdf'"#, [r#"'asdf'"#] }
  ]);

  text_lexemes!(numbers [
    { "1.0e+5", ["1.0e+5"] }
    { "NaN", ["NaN"] }
    { "inf", ["inf"] }
    { "100000", ["100000"] }
    { "100_000", ["100_000"] }
  ]);

  text_lexemes!(keywords {
    "import as from if elif else for in while loop fn throw try catch finally pass continue break return struct impl trait enum type and or not is none true false",
    ["import", "as", "from", "if", "elif", "else", "for", "in", "while", "loop", "fn", "throw", "try", "catch", "finally", "pass", "continue", "break", "return", "struct", "impl", "trait", "enum", "type", "and", "or", "not", "is", "none", "true", "false"]
  });

  text_lexemes!(comment {
    "# test",
    []
  });

  test_tokens!(indent_simple {
    "
    fn test():
      \tpass
    ",
    [
      t!(Fn),
      t!(Identifier, "test"),
      t!(LeftParen),
      t!(RightParen),
      t!(Colon),
      t!(Indent, "\n  \t"),
      t!(Pass)
    ]
  });

  test_tokens!(indent_multi {
    "
    fn test():
      
      pass
    ",
    [
      t!(Fn),
      t!(Identifier, "test"),
      t!(LeftParen),
      t!(RightParen),
      t!(Colon),
      t!(Indent, "\n  \n  "),
      t!(Pass)
    ]
  });

  test_tokens!(indent_multi_empty {
    "
    fn test():
    
      pass
    ",
    [
      t!(Fn),
      t!(Identifier, "test"),
      t!(LeftParen),
      t!(RightParen),
      t!(Colon),
      t!(Indent, "\n  "),
      t!(Pass)
    ]
  });

  test_tokens!(comments {
    "
    # test
    fn test(): # test
      pass
    ",
    [
      t!(Fn),
      t!(Identifier, "test"),
      t!(LeftParen),
      t!(RightParen),
      t!(Colon),
      t!(Indent, "\n  "),
      t!(Pass)
    ]
  });
}
