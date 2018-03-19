package xyz.minond.talk.pti

object Main {
  def main(args: Array[String]): Unit = {
    val scanner = new Scanner("(define x 123)")
    scanner foreach println
  }
}
