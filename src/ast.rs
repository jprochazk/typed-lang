use std::borrow::Cow;

use ast2str::AstToStr;
use derive_more::{Constructor, From};

pub type Span = std::ops::Range<usize>;

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct Ast<'source> {
  pub statements: Vec<Stmt<'source>>,
}

#[derive(Debug, AstToStr, Constructor)]
pub struct Stmt<'source> {
  #[forward]
  pub kind: StmtKind<'source>,
  pub span: Span,
}

impl<'source> PartialEq<Stmt<'source>> for Stmt<'source> {
  fn eq(&self, other: &Stmt<'source>) -> bool {
    self.kind == other.kind
  }
}

#[derive(Debug, PartialEq, AstToStr)]
pub enum StmtKind<'source> {
  PrintStmt(#[forward] PrintStmt<'source>),
  VarStmt(#[forward] VarStmt<'source>),
  ExprStmt(#[forward] ExprStmt<'source>),
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct PrintStmt<'source> {
  pub value: Expr<'source>,
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct VarStmt<'source> {
  pub name: Cow<'source, str>,
  pub ty: Option<Cow<'source, str>>,
  pub init: Option<Expr<'source>>,
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct ExprStmt<'source> {
  pub inner: Expr<'source>,
}

#[derive(Debug, AstToStr, Constructor)]
pub struct Expr<'source> {
  #[forward]
  pub kind: ExprKind<'source>,
  pub span: Span,
}

impl<'source> PartialEq<Expr<'source>> for Expr<'source> {
  fn eq(&self, other: &Expr<'source>) -> bool {
    // spans are ignored
    self.kind == other.kind
  }
}

#[derive(Debug, PartialEq, AstToStr)]
pub enum ExprKind<'source> {
  BinaryExpr(#[forward] BinaryExpr<'source>),
  UnaryExpr(#[forward] UnaryExpr<'source>),
  LiteralExpr(#[forward] LiteralExpr<'source>),
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct BinaryExpr<'source> {
  #[debug]
  pub op: BinaryOp,
  pub left: Box<Expr<'source>>,
  pub right: Box<Expr<'source>>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinaryOp {
  // Arithmetic
  Add,
  Sub,
  Mul,
  Div,
  Rem,
  Pow,
  // Comparison
  Eq,
  Neq,
  Lt,
  Le,
  Gt,
  Ge,
  // Logical
  And,
  Or,
}

#[derive(Debug, PartialEq, AstToStr, Constructor)]
pub struct UnaryExpr<'source> {
  #[debug]
  pub op: UnaryOp,
  pub expr: Box<Expr<'source>>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOp {
  Not,
  Neg,
}

#[derive(Debug, PartialEq, AstToStr, From)]
pub enum LiteralExpr<'source> {
  Number(#[rename = "value"] f64),
  String(#[rename = "value"] Cow<'source, str>),
  Bool(#[rename = "value"] bool),
  Nil,
}

#[macro_export]
macro_rules! ast {
  ($($stmt:expr),*) => {
    Ast::new(vec![
      $(
        $stmt
      ),*
    ])
  }
}

#[macro_export]
macro_rules! stmt {
  ($span:expr, $kind:ident, $($value:tt)*) => {{
    let _span = $span;
    crate::ast::Stmt::new(
      crate::ast::StmtKind::$kind(
        crate::ast::$kind::new($($value)*)
      ),
      _span
    )
  }};
}

#[macro_export]
macro_rules! expr {
  ($span:expr, $kind:ident = { $($value:expr),* }) => {{
    let _span = $span;
    crate::ast::Expr::new(
      crate::ast::ExprKind::$kind(
        crate::ast::$kind::new($($value),*)
      ),
      _span
    )
  }};
  ($span:expr, Nil) => {{
    let _span = $span;
    crate::ast::Expr::new(
      crate::ast::ExprKind::LiteralExpr(
        crate::ast::LiteralExpr::Nil
      ),
      _span
    )
  }};
  ($span:expr, Number, $value:expr) => {{
    let _span = $span;
    crate::ast::Expr::new(
      crate::ast::ExprKind::LiteralExpr(
        crate::ast::LiteralExpr::Number($value)
      ),
      _span
    )
  }};
  ($span:expr, String, $value:expr) => {{
    let _span = $span;
    crate::ast::Expr::new(
      crate::ast::ExprKind::LiteralExpr(
        crate::ast::LiteralExpr::String($value)
      ),
      _span
    )
  }};
  ($span:expr, Bool, $value:expr) => {{
    let _span = $span;
    crate::ast::Expr::new(
      crate::ast::ExprKind::LiteralExpr(
        crate::ast::LiteralExpr::Bool($value)
      ),
      _span
    )
  }};
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_ast() {
    ast![stmt!(
      0..0,
      ExprStmt,
      expr!(
        0..0,
        BinaryExpr = {
          BinaryOp::Add,
          box expr!(0..0, Number, 0f64),
          box expr!(0..0, Number, 1f64)
        }
      )
    )];
  }
}
