package lang

import util.EscapeUtil._
import scala.util.matching.Regex
import scala.util.matching.Regex.Match
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.HashMap
import scala.annotation.tailrec
import lang.Scanner._

case class SourceLocation(file: String, line: Int, column: Int, data: String) {
  override def toString: String = s"[$file:$line:$column: ${data.escapeControl} ]"
}

abstract class Token

case class TokenIdentifier(id: String) extends Token
case class TokenNumber(value: Int) extends Token
case class TokenString(value: String) extends Token {
  override def toString: String = s"""TokenString("${value.escape}")"""
}
case object TokenEnd extends Token
case object TokenNewline extends Token
case object TokenOpenBlock extends Token
case object TokenCloseBlock extends Token

case object KeywordTable extends Token

case class Lexeme(token: Token, location: SourceLocation)

object Scanner {

  val keywordMap = HashMap[String, Token]("table" -> KeywordTable)
  val operatorMap = HashMap[String, Token]("{" -> TokenOpenBlock, "}" -> TokenCloseBlock)

  val tok_identifier = (raw"""[A-Za-z]([A-Za-z0-9\-]*[A-Za-z0-9])?""".r,
    (m: Match) => keywordMap.getOrElse(m.group(0), new TokenIdentifier(m.group(0))))

  val tok_number = (raw"""[0-9]+""".r,
    (m: Match) => new TokenNumber(m.group(0).toInt))

  val tok_string = (raw"""("((\\[\\"nrt]|[^"\\])*)")""".r,
    (m: Match) => new TokenString(m.group(2).unescape))

  val tok_newline = (raw"""\n""".r,
    (m: Match) => TokenNewline)

  val tok_operator = (s"[${operatorMap.keys.mkString("")}]".r,
    (m: Match) => operatorMap(m.group(0)))

  val re_whitespace = raw"""[ \r\t]+|#[^\n]*""".r

  val tokens = Array(tok_identifier, tok_number, tok_string, tok_newline, tok_operator)

}

class Scanner(val source: String, val filename: String) {

  var charOffset = 0
  var lineNumber = 1
  var previousLinebreakOffset = 0

  var hasReportedEndOnce = false

  def sourceAtPosition = source.substring(charOffset)

  @tailrec private def skipWhitespace(): Unit = {
    re_whitespace.findPrefixMatchOf(sourceAtPosition) match {
      case Some(mat) =>
        charOffset += mat.end
        skipWhitespace()
      case None =>
    }
  }

  /**
    * Scan next lexeme from the source.
    * Throws an error on fail!
    * @return A lexeme if one is succesfully parsed
    */
  def scan(): Lexeme = {
    skipWhitespace()

    val pos = sourceAtPosition
    for ((re, tokenFunc) <- tokens) {
      re.findPrefixMatchOf(pos) match {
        case Some(mat) =>
          val data = pos.substring(0, mat.end)
          val col = charOffset - previousLinebreakOffset + 1
          val loc = new SourceLocation(filename, lineNumber, col, data)
          val token = tokenFunc(mat)

          charOffset += mat.end
          if (token == TokenNewline) {
            lineNumber += 1
            previousLinebreakOffset = charOffset
          }

          return new Lexeme(token, loc)
        case None =>
      }
    }

    if (pos.length == 0) {
      if (hasReportedEndOnce) {
        throw new RuntimeException("Scanned past the end!")
      }
      hasReportedEndOnce = true

      val col = charOffset - previousLinebreakOffset + 1
      val loc = new SourceLocation(filename, lineNumber, col, "")
      return new Lexeme(TokenEnd, loc)
    }

    // TODO: A custom exception class
    val col = charOffset - previousLinebreakOffset + 1
    throw new RuntimeException(s"Scanner fail at line $lineNumber:$col!")
  }

}
