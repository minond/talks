package xyz.minond.pti

trait Loader {
  def load(env: Environment): Environment
}

sealed abstract class Expression {
  override final def toString =
    this match {
      case True => "#t"
      case False => "#f"
      case Error(message, _) => s"""(error "$message")"""
      case Identifier(value) => value
      case Integer(value) => value.toString
      case Pair(a, b) => s"($a . $b)"
      case Quote(value, _) => s"'$value"
      case Real(value) => value.toString
      case SExpr(values) => s"(${values.map(_.toString).mkString(" ")})"
      case Str(value) => value
      case Procedure(_) => "#<procedure>"
      case Proc(_, _, _, delayed) =>
        if (delayed) "#<procedure...>"
        else "#<procedure>"
    }

  def quote: Expression =
    this match {
      case expr: Quote => expr
      case expr => Quote(expr)
    }

  def unQuote: Expression =
    this match {
      case Quote(expr, _) => expr
      case expr => expr
    }
}

case class Identifier(value: String) extends Expression
case class Integer(value: Int) extends Expression
case class Pair(a: Expression, b: Expression) extends Expression
case class Real(value: Double) extends Expression
case class SExpr(values: List[Expression]) extends Expression
case class Str(value: String) extends Expression

sealed trait QuoteInfo
case object UserSpace extends QuoteInfo
case object Internal extends QuoteInfo
case object PrintfNl extends QuoteInfo
case class Quote(value: Expression, info: QuoteInfo = UserSpace) extends Expression

object Bool {
  def apply(value: Boolean): Bool =
    if (value) True
    else False
}

sealed trait Bool extends Expression
case object True extends Bool
case object False extends Bool

case class Error(message: String, prev: Option[Error] = None) extends Expression {
  def stringify(prefix: String = ""): String = {
    val next = prev match {
      case Some(err) => "\n" + err.stringify(prefix + "  ")
      case None => ""
    }

    s"; ${prefix}- ${message}${next}"
  }
}

case class Procedure(fn: (List[Expression], Environment) => Expression) extends Expression

case class Proc(
    args: List[String],
    body: Expression,
    env: Environment,
    delayed: Boolean = false)
    extends Expression {
  def scope(
      vals: List[Expression],
      local: Environment,
      global: Environment): Environment =
    zipArgVals(vals).foldLeft[Environment](local.pushBack(global)) {
      case (env: Environment, (name: String, expr: Expression)) =>
        env.define(name, expr)

      case _ => local
    }

  def zipArgVals(vals: List[Expression]): List[(String, Expression)] = {
    if (!isVariadic)
      args.zip(vals)
    else
      args.filter(_ != ".").zip(vals) ++ List(
        (
          args(args.indexOf(".") + 1),
          SExpr(vals.drop(args.indexOf(".")))
        ))
  }

  def validArity(count: Int): Boolean =
    if (!isVariadic) count == args.size
    else count >= args.indexOf(".") - 1

  def isVariadic =
    args.nonEmpty && args.contains(".") && args.last != "."
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
      case (None, None) => Error(Interpreter.Message.ERR_UNDEFINED_LOOKUP(name))
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
