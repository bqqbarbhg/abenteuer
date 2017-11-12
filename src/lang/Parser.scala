package lang

import scala.collection.immutable.Vector
import scala.collection.mutable.ArrayBuffer

abstract class AstNode

case class AstTable(val name: TokenIdentifier, val columns: Vector[TokenIdentifier], val constraints: AstBlock) extends AstNode
case class AstBlock(val statements: Vector[AstNode]) extends AstNode

case class CompileError(val location: SourceLocation,
                        val message: String) extends Exception(s"${location.file}:${location.line}:${location.column}: $message")
{

  def this(token: Token, message: String) = this(token.location, message)

}

object Parser {
  def parse(source: String, filename: String): AstNode = {
    val scanner = new Scanner(source, filename)
    val parser = new Parser(scanner)
    parser.parse()
  }
}

class Parser(val scanner: Scanner) {

  var token: Token = scanner.scan()

  def accept[T](fn: PartialFunction[Token, T]): Option[T] = {
    val tok = token
    if (fn.isDefinedAt(tok)) {
      token = scanner.scan()
      Some(fn(tok))
    } else {
      None
    }
  }

  def acceptMany[T](fn: PartialFunction[Token, T]): ArrayBuffer[T] = {
    val buffer = new ArrayBuffer[T]()
    while (fn.isDefinedAt(token)) {
      buffer += fn(token)
      token = scanner.scan()
    }
    buffer
  }

  def require[T](expected: String)(fn: PartialFunction[Token, T]): T = {
    accept(fn) match {
      case Some(t) => t
      case None => unexpected(expected)
    }
  }

  def unexpected(expected: String): Nothing = {
   throw new CompileError(token, s"Unexpected token ${token.toString}, expected $expected")
  }

  def acceptEnd() = accept { case TokenEnd() => } isDefined

  def parse(): AstBlock = {
    val block = new ArrayBuffer[AstNode]()

    while (!acceptEnd()) {
      accept {
        case TokenNewline() => // Nop
      } getOrElse {
        block += parseTopLevel()
      }
    }

    new AstBlock(block.toVector)
  }

  def parseTopLevel(): AstNode = require[AstNode]("top-level statement") {
    case KeywordTable() =>
      val name = require("table name") { case t: TokenIdentifier => t }
      val cols = acceptMany { case t: TokenIdentifier => t }
      val block = acceptMaybeBlock() getOrElse new AstBlock(Vector[AstNode]())
      new AstTable(name, cols.toVector, block)
  }

  def acceptMaybeBlock(): Option[AstBlock] = accept[AstBlock] {
    case TokenOpenBlock() =>
      require("block closing '}'") { case TokenCloseBlock() => }
      new AstBlock(Vector[AstNode]())
  }

}

