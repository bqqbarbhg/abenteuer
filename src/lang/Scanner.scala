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
  def name: String
}

case class TokenId(id: String) extends Token { def name: String = "identifier" }
case class TokenNumber(value: Int) extends Token { def name: String = "number" }
case class TokenString(value: String) extends Token {
  override def toString: String = s"""TokenString("${value.escape}")"""
  def name: String = "string litral"
}
case class TokenEnd() extends Token { def name: String = "end of file" }
case class TokenNewline(real: Boolean) extends Token { def name: String = if (real) "newline" else "statement terminator" }
case class TokenOpenBlock() extends Token { def name: String = "block open" }
case class TokenCloseBlock() extends Token { def name: String = "block close" }
case class TokenOpenParen() extends Token { def name: String = "opening paren" }
case class TokenCloseParen() extends Token { def name: String = "closing paren" }
case class TokenComma() extends Token { def name: String = "comma" }
case class TokenDot() extends Token { def name: String = "dot" }
case class TokenArrow() extends Token { def name: String = "arrow" }
case class TokenNot() extends Token { def name: String = "not" }
case class TokenValuePrefix() extends Token { def name: String = "value prefix" }
case class TokenColon() extends Token { def name: String = "colon" }
case class TokenIndirectPrefix() extends Token { def name: String = "indirect prefix" }
case class TokenWildcard() extends Token { def name: String = "wildcard" }

case class KeywordTable() extends Token { def name: String = "keyword 'table'" }
case class KeywordEntity() extends Token { def name: String = "keyword 'entity'" }
case class KeywordDefine() extends Token { def name: String = "keyword 'define'" }
case class KeywordRule() extends Token { def name: String = "keyword 'rule'" }
case class KeywordExternal() extends Token { def name: String = "keyword 'external'" }

object Scanner {

  val tok_identifier = (raw"""[A-Za-z]([A-Za-z0-9\-]*[A-Za-z0-9])?""".r,
    (m: Match) => m.group(0) match {
      case "table" => new KeywordTable
      case "entity" => new KeywordEntity
      case "define" => new KeywordDefine
      case "external" => new KeywordExternal
      case identifier => new TokenId(identifier)
    })

  val tok_number = (raw"""[+\-]?[0-9]+""".r,
    (m: Match) => new TokenNumber(m.group(0).toInt))

  val tok_string = (raw"""("((\\[\\"nrt]|[^"\\])*)")""".r,
    (m: Match) => new TokenString(m.group(2).unescape))

  val tok_newline = (raw"""[\n;]""".r,
    (m: Match) => new TokenNewline(m.group(0) == "\n"))

  val tok_operator = (s"[{}(),.!*:&_]|->".r,
    (m: Match) => m.group(0) match {
      case "{" => new TokenOpenBlock
      case "}" => new TokenCloseBlock
      case "(" => new TokenOpenParen
      case ")" => new TokenCloseParen
      case "," => new TokenComma
      case "." => new TokenDot
      case "!" => new TokenNot
      case "->" => new TokenArrow
      case "*" => new TokenValuePrefix
      case ":" => new TokenColon
      case "&" => new TokenIndirectPrefix
      case "_" => new TokenWildcard
    })

  val re_whitespace = raw"""[ \r\t]+|#[^\n]*|\\\r?\n""".r

  val tokens = Array(tok_identifier, tok_number, tok_string, tok_newline, tok_operator)

  def errorLine(source: String, loc: lang.SourceLocation): String = {
    val lines = source.split('\n')
    val line = lines(loc.line - 1).replace("\t", " ")
    var arrow = ""
    for ((c, i) <- line.zipWithIndex) {
      arrow += {
        if (i < loc.column - 1) ' '
        else if (i == loc.column - 1) '^'
        else if (i < loc.column + loc.data.length - 2) '~'
        else if (i == loc.column + loc.data.length - 2) '^'
        else ' '
      }
    }

    line.stripLineEnd + "\n" + arrow
  }
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
            case TokenNewline(real) =>
              if (real) {
                lineNumber += 1
                previousLinebreakOffset = charOffset
              }
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
