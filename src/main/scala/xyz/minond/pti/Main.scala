package xyz.minond.pti

import java.io.{BufferedReader, InputStreamReader}

object Main {
  def main(args: Array[String]): Unit =
    Repl.run(new BufferedReader(new InputStreamReader(System.in)))
}
