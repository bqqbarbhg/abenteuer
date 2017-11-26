package test

import lang.{Token, Scanner, TokenEnd}

import scala.util.{Failure, Success, Try}

object ScannerTest extends App {

  val source =
    """
       | print "Hello\nworld!"
       | ! test 123
       | () { } -> { }
    """.stripMargin
  val scanner = new Scanner(source, "inline")
  printTokens(scanner)

  def printTokens(scanner: Scanner): Unit = {
    while (true) {
      Try(scanner.scan()) match {
        case Success(TokenEnd()) =>
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

