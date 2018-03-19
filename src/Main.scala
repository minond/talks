package xyz.minond.talk.pti

object Main {
  def main(args: Array[String]): Unit = {
    new Scanner("""(define x 123)""") foreach println
    new Scanner("""(define y "hi here")""") foreach println
    new Scanner("""     (define x 123)   """) foreach println
    new Scanner(""""one two three"four 1 2 3 123""") foreach println
    new Scanner("""one->two->three""") foreach println
  }
}
