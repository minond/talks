sealed trait Token
case class Number(value: Double) extends Token
case class Str(value: String) extends Token
case object OpenParen extends Token
case object CloseParen extends Token
case object True extends Token
case object False extends Token
case class Identifier(value: String) extends Token

object Main {
  def main(args: Array[String]): Unit = {
    println("S")
  }
}
