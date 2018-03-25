package xyz.minond.talk.pti

abstract class Expression {
  override final def toString =
    this match {
      case BooleanExpr(value) => if (value) "#t" else "#f"
      case IdentifierExpr(value) => value
      case IntNumberExpr(value) => value.toString
      case QuoteExpr(value) => s"'$value"
      case RealNumberExpr(value) => value.toString
      case SExpr(values) => s"(${values.map(_.toString).mkString(" ")})"
      case StringExpr(value) => value
      case ErrorExpr(message, _) => s"""(error "$message")"""
      case LambdaExpr(_, _, _) => "#<procedure>"
      case builtin: BuiltinExpr =>
        val name = Interpreter.builtin
          .filter({ case (_, b) => b == builtin })
          .keys
          .headOption
          .getOrElse("???")

        s"#<procedure:$name>"
    }
}

case class BooleanExpr(value: Boolean) extends Expression
case class IdentifierExpr(value: String) extends Expression
case class IntNumberExpr(value: Int) extends Expression
case class QuoteExpr(value: Expression) extends Expression
case class RealNumberExpr(value: Double) extends Expression
case class SExpr(values: List[Expression]) extends Expression
case class StringExpr(value: String) extends Expression

case class BuiltinExpr(fn: (List[Expression], Environment) => Expression)
    extends Expression

case class ErrorExpr(message: String, prev: Option[ErrorExpr] = None) extends Expression {
  def stringify(prefix: String = ""): String = {
    val next = prev match {
      case Some(err) => "\n" + err.stringify(prefix + "  ")
      case None => ""
    }

    s"; ${prefix}- ${message}${next}"
  }
}

case class LambdaExpr(args: Set[String], body: Expression, env: Environment)
    extends Expression {
  def scope(
      vals: List[Expression],
      local: Environment,
      global: Environment): Environment =
    // XXX Add support for varargs
    args.zip(vals).foldLeft[Environment](local.pushBack(global)) {
      case (env: Environment, (name: String, expr: Expression)) =>
        env.define(name, expr)

      case _ => local
    }

  def validArity(count: Int): Boolean =
    // XXX Add support for varargs
    count == args.size
}

case class Environment(
    vars: Map[String, Expression],
    parent: Option[Environment] = None) {
  def define(name: String, expr: Expression) =
    Environment(vars ++ Map(name -> expr), parent)

  def pushBack(env: Environment): Environment = {
    parent match {
      case Some(par) => Environment(vars, Some(par.pushBack(env)))
      case None => Environment(vars, Some(env))
    }
  }

  def lookup(name: String): Expression = {
    (vars.get(name), parent) match {
      case (Some(expr), _) => expr
      case (None, Some(env)) => env.lookup(name)
      case (None, None) => ErrorExpr(Interpreter.Message.ERR_UNDEFINED_LOOKUP(name))
    }
  }

  override def toString = {
    val text = vars.keys.toList ++ (parent match {
      case Some(env) => List(env.toString)
      case _ => List.empty
    })

    s"Environment{${text.mkString(", ")}}"
  }
}
