package xyz.minond.talk.pti

object Statement {
  trait Statement

  case class Error(
      message: String,
      prev: Option[Error] = None,
      stmt: Option[Statement] = None)

  case class SExprStmt(values: List[Statement]) extends Statement
  case class QuoteStmt(value: Statement) extends Statement
  case class IdentifierStmt(value: String) extends Statement
  case class IntegerNumberStmt(value: Int) extends Statement
  case class StringStmt(value: String) extends Statement
  case class RealNumberStmt(value: Double) extends Statement
  case class BooleanStmt(value: Boolean) extends Statement
}
