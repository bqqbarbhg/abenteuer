package lang

import util.EscapeUtil._
import scala.util.matching.Regex
import scala.util.matching.Regex.Match
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.HashMap
import scala.annotation.tailrec
import lang.Scanner._

case class SourceLocation(file: String, line: Int, column: Int, data: String) {
  def prefix: String = s"$file:$line:$column"
  override def toString: String = s"[$prefix: ${data.escapeControl} ]"
}

abstract class Token {
  var locationImpl: SourceLocation = null
  def location: SourceLocation = locationImpl
}

case class TokenId(id: String) extends Token
case class TokenNumber(value: Int) extends Token
case class TokenString(value: String) extends Token {
  override def toString: String = s"""TokenString("${value.escape}")"""
}
case class TokenEnd() extends Token
case class TokenNewline() extends Token
case class TokenOpenBlock() extends Token
case class TokenCloseBlock() extends Token
case class TokenOpenParen() extends Token
case class TokenCloseParen() extends Token
case class TokenComma() extends Token
case class TokenDot() extends Token
case class TokenArrow() extends Token
case class TokenNot() extends Token

case class KeywordTable() extends Token
case class KeywordEntity() extends Token
case class KeywordRule() extends Token

object Scanner {

  val tok_identifier = (raw"""[A-Za-z]([A-Za-z0-9\-]*[A-Za-z0-9])?""".r,
    (m: Match) => m.group(0) match {
      case "table" => new KeywordTable
      case "entity" => new KeywordEntity
      case identifier => new TokenId(identifier)
    })

  val tok_number = (raw"""[+\-]?[0-9]+""".r,
    (m: Match) => new TokenNumber(m.group(0).toInt))

  val tok_string = (raw"""("((\\[\\"nrt]|[^"\\])*)")""".r,
    (m: Match) => new TokenString(m.group(2).unescape))

  val tok_newline = (raw"""\n""".r,
    (m: Match) => new TokenNewline)

  val tok_operator = (s"[{}(),.!]|->".r,
    (m: Match) => m.group(0) match {
      case "{" => new TokenOpenBlock()
      case "}" => new TokenCloseBlock()
      case "(" => new TokenOpenParen()
      case ")" => new TokenCloseParen()
      case "," => new TokenComma()
      case "." => new TokenDot()
      case "!" => new TokenNot()
      case "->" => new TokenArrow()
    })

  val re_whitespace = raw"""[ \r\t]+|#[^\n]*|\\\r?\n""".r

  val tokens = Array(tok_identifier, tok_number, tok_string, tok_newline, tok_operator)

}

class Scanner(val source: String, val filename: String) {

  var charOffset = 0
  var lineNumber = 1
  var previousLinebreakOffset = 0

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
  def scan(): Token = {
    skipWhitespace()

    val pos = sourceAtPosition
    for ((re, tokenFunc) <- tokens) {
      re.findPrefixMatchOf(pos) match {
        case Some(mat) =>
          val data = pos.substring(0, mat.end)
          val col = charOffset - previousLinebreakOffset + 1
          val token = tokenFunc(mat)
          token.locationImpl = new SourceLocation(filename, lineNumber, col, data)

          charOffset += mat.end
          token match {
            case TokenNewline() =>
              lineNumber += 1
              previousLinebreakOffset = charOffset
            case _ =>
          }

          return token
        case None =>
      }
    }

    if (pos.length == 0) {
      val col = charOffset - previousLinebreakOffset + 1
      val token = new TokenEnd
      token.locationImpl = new SourceLocation(filename, lineNumber, col, "")
      token
    } else {
      val col = charOffset - previousLinebreakOffset + 1
      val loc = new SourceLocation(filename, lineNumber, col, "")
      throw new CompileError(loc, s"Unexpected character '${pos.substring(0, 1).escapeControl}'")
    }
  }

}
