use crate::ast::*;
use std::borrow::Cow;

macro_rules! op {
  ($a:ident, $kind:ident) => {
    expr!(
      $a.span.start..$a.span.end,
      UnaryExpr = {
        UnaryOp::$kind,
        box $a
      }
    )
  };
  ($a:ident, $b:ident, $kind:ident) => {{
    expr!(
      $a.span.start..$b.span.end,
      BinaryExpr = {
        BinaryOp::$kind,
        box $a,
        box $b
      }
    )
  }};
}

#[inline]
fn unescape(s: &str) -> String {
  let mut out = String::from(s);
  unescape_in_place(&mut out);
  out
}

// Adapted from https://docs.rs/snailquote/0.3.0/x86_64-pc-windows-msvc/src/snailquote/lib.rs.html.
/// Unescapes the given string in-place. Returns `None` if the string contains an invalid escape sequence.
fn unescape_in_place(s: &mut String) -> Option<()> {
  let mut out = String::with_capacity(s.len());
  let mut chars = s.chars();
  while let Some(ch) = chars.next() {
    if ch == '\\' {
      if let Some(next) = chars.next() {
        let escape = match next {
          'a' => Some('\u{07}'),
          'b' => Some('\u{08}'),
          'v' => Some('\u{0B}'),
          'f' => Some('\u{0C}'),
          'n' => Some('\n'),
          'r' => Some('\r'),
          't' => Some('\t'),
          '\'' => Some('\''),
          '"' => Some('"'),
          'e' | 'E' => Some('\u{1B}'),
          'u' => Some(parse_unicode(&mut chars)?),
          _ => None,
        };
        match escape {
          Some(esc) => {
            out.push(esc);
          }
          None => {
            out.push(ch);
            out.push(next);
          }
        }
      }
    } else {
      out.push(ch);
    }
  }
  *s = out;
  Some(())
}

// Adapted from https://docs.rs/snailquote/0.3.0/x86_64-pc-windows-msvc/src/snailquote/lib.rs.html.
fn parse_unicode<I>(chars: &mut I) -> Option<char>
where
  I: Iterator<Item = char>,
{
  match chars.next() {
    Some('{') => {}
    _ => {
      return None;
    }
  }
  let unicode_seq: String = chars.take_while(|&c| c != '}').collect();
  u32::from_str_radix(&unicode_seq, 16).ok().and_then(char::from_u32)
}

#[cfg(test)]
mod tests {
  use super::*;
  use ast2str::AstToStr;
  use pretty_assertions::assert_eq;

  pub trait AsDisplay: std::fmt::Display {
    fn as_display(&self) -> DisplayAsDebugWrapper<&'_ Self> {
      DisplayAsDebugWrapper(self)
    }
  }
  impl AsDisplay for str {}

  #[derive(Clone, PartialEq)]
  pub struct DisplayAsDebugWrapper<T>(T);

  impl<T> std::fmt::Debug for DisplayAsDebugWrapper<T>
  where
    T: std::fmt::Display,
  {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      write!(f, "{}", self.0)
    }
  }

  impl<T> std::ops::Deref for DisplayAsDebugWrapper<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
      &self.0
    }
  }

  macro_rules! test_eq {
    ($input:expr, $expected:expr) => {{
      assert_eq!(
        entry($input)
          .unwrap()
          .ast_to_str()
          .replace(|c| ['├', '─', '│', '╰', '✕', '↓'].contains(&c), " ")
          .trim()
          .as_display(),
        $expected.trim().as_display()
      )
    }};
  }
}
