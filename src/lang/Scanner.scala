package lang

import scala.util.matching.Regex

case class SourceLocation(file: String, line: Int, column: Int)

abstract class Token

case class Identifier(id: String) extends Token
case class Number(value: String) extends Token

case class Lexeme(token: Token, location: SourceLocation)

class Scanner(val source: String, val filename: String) {

}
