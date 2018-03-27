package xyz.minond.talk.pti

abstract class Expression {
  override final def toString =
    this match {
      case True => "#t"
      case False => "#f"
      case Error(message, _) => s"""(error "$message")"""
      case Identifier(value) => value
      case Integer(value) => value.toString
      case Pair(a, b) => s"($a . $b)"
      case Quote(value) => s"'$value"
      case Real(value) => value.toString
      case SExpr(values) => s"(${values.map(_.toString).mkString(" ")})"
      case Str(value) => value

      case Lambda(_, _, _, delayed) =>
        if (delayed) "#<procedure...>"
        else "#<procedure>"

      case Builtin(fn) =>
        val name = Interpreter.builtin
          .filter({ case (_, b) => b == Builtin(fn) })
          .keys
          .headOption
          .getOrElse("???")

        s"#<procedure:$name>"
    }

  def unQuote: Expression =
    this match {
      case Quote(expr) => expr
      case expr => expr
    }
}

case class Identifier(value: String) extends Expression
case class Integer(value: Int) extends Expression
case class Pair(a: Expression, b: Expression) extends Expression
case class Quote(value: Expression) extends Expression
case class Real(value: Double) extends Expression
case class SExpr(values: List[Expression]) extends Expression
case class Str(value: String) extends Expression

object BooleanExpr {
  def apply(value: Boolean): BooleanExpr =
    if (value) True
    else False
}

trait BooleanExpr extends Expression
case object True extends BooleanExpr
case object False extends BooleanExpr

case class Builtin(fn: (List[Expression], Environment) => Expression) extends Expression

case class Error(message: String, prev: Option[Error] = None) extends Expression {
  def stringify(prefix: String = ""): String = {
    val next = prev match {
      case Some(err) => "\n" + err.stringify(prefix + "  ")
      case None => ""
    }

    s"; ${prefix}- ${message}${next}"
  }
}

case class Lambda(
    args: Set[String],
    body: Expression,
    env: Environment,
    delayed: Boolean = false)
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
