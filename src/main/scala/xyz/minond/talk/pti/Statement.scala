package xyz.minond.talk.pti

object Statement {
  trait Stmt

  case class SExprStmt(head: Stmt, tail: List[Stmt]) extends Stmt
  case class QuoteStmt(value: Stmt) extends Stmt
  case class IdentifierStmt(value: String) extends Stmt
  case class IntegerNumberStmt(value: Int) extends Stmt
  case class RealNumberStmt(value: Double) extends Stmt
  case class BooleanStmt(value: Boolean) extends Stmt
}
