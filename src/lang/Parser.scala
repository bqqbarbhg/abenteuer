package lang

import scala.collection.immutable.Vector
import scala.collection.mutable.ArrayBuffer

abstract class AstNode

case class AstTable(val name: TokenId, val columns: Vector[TokenId], val constraints: AstBlock) extends AstNode
case class AstBlock(val statements: Vector[AstNode]) extends AstNode
case class AstQueryStmt(val operator: TokenId, val values: Vector[TokenId]) extends AstNode

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

/*

  program = top-level*
          | <eof>
          ;

  top-level = "table" id id* table-body?
            | <newline>
            ;

  query-stmt = id query-tuple ("," query-tuple)* ;
  query-tuple = id+ ;

  table-body = "{" ( query-stmt* | <newline> ) "}"

 */
class Parser(scanner: Scanner) extends ParserBase(scanner) {

  def parse(): AstBlock = {
    val block = untilAccept("", { case TokenEnd() => true }) { parseTopLevel() }.flatten.toVector
    new AstBlock(block)
  }

  def parseTopLevel(): Option[AstNode] = require("top-level statement") {
    case table: KeywordTable => Some(finishTable())
    case TokenNewline() => None
  }

  def finishTable(): AstTable = {
    val name = require("table name") { case id: TokenId => id }
    val cols = acceptMany { case id: TokenId => id }.toVector
    if (cols.isEmpty) errorHere(s"Expected at least one column for table ${name.id}")
    val body = tryParseTableBody().getOrElse(new AstBlock(Vector[AstNode]()))
    new AstTable(name, cols, body)
  }

  def tryParseTableBody(): Option[AstBlock] = accept {
    case TokenOpenBlock() =>
      val stmt = untilAccept("table body", { case TokenCloseBlock() => true }) {
        val ast: Option[Vector[AstQueryStmt]] = require("query statement") {
          case TokenNewline() => None
          case id: TokenId => Some(finishQueryStmt(id))
        }
        ast
      }.flatten.flatten.toVector
      new AstBlock(stmt)
  }

  def finishQueryStmt(name: TokenId): Vector[AstQueryStmt] = {
    val first = parseQueryTuple()
    val args = Vector[Vector[TokenId]](first) ++ acceptMany { case TokenComma() => parseQueryTuple() }
    args.map(a => new AstQueryStmt(name, a))
  }

  def parseQueryTuple(): Vector[TokenId] = {
    val result = acceptMany { case id: TokenId => id }.toVector
    if (result.isEmpty) errorHere("Expected at least one query argument")
    result
  }

}

