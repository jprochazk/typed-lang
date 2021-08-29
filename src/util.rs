use ast2str::AstToStr;

#[derive(Debug, PartialEq)]
pub enum Either<L, R> {
  Left(L),
  Right(R),
}

impl<L: AstToStr, R: AstToStr> AstToStr for Either<L, R> {
  fn ast_to_str(&self) -> String {
    match self {
      Either::Left(left) => left.ast_to_str(),
      Either::Right(right) => right.ast_to_str(),
    }
  }
}
