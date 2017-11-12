package test

import lang.{Lexeme, Scanner, TokenEnd}

import scala.util.{Failure, Success, Try}

object ScannerTest extends App {

  val source =
    """
       | print "Hello\nworld!"
    """.stripMargin
  val scanner = new Scanner(source, "inline")
  printTokens(scanner)

  def printTokens(scanner: Scanner): Unit = {
    while (true) {
      Try(scanner.scan()) match {
        case Success(Lexeme(TokenEnd, _)) =>
          println("Reached the end!")
          return
        case Success(lexeme) =>
          println(lexeme.toString())
        case Failure(error) =>
          println(error.getMessage())
          return
      }
    }
  }
}

