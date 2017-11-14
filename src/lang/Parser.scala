package lang

import scala.collection.immutable.Vector
import scala.collection.mutable.ArrayBuffer

abstract class AstNode

case class AstTable(val name: TokenId, val columns: Vector[TokenId], val constraints: AstBlock) extends AstNode
case class AstEntity(val name: TokenId, val statements: AstBlock) extends AstNode
case class AstBlock(val statements: Vector[AstNode]) extends AstNode
case class AstQueryStmt(val operator: TokenId, val values: Vector[AstEx]) extends AstNode
case class AstNamespace(val name: TokenId, val block: AstBlock) extends AstNode
case class AstFreeQuery(val queries: Vector[AstQueryStmt]) extends AstNode

abstract class AstEx extends AstNode
case class AstExId(val token: TokenId) extends AstEx
case class AstExString(val token: TokenString) extends AstEx
case class AstExNumber(val token: TokenNumber) extends AstEx

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
          | <eof> ;

  top-level = "table" ID ID* query-body?
            | "entity" ID query-body?
            | ID "{" top-level* "}"
            | query-stmt
            | <newline> ;

  query-stmt = ID query-tuple ("," query-tuple)* ;
  query-tuple = expr+ ;

  expr = ID | STRING | NUMBER ;

  query-body = "{" ( query-stmt* | <newline> ) "}"

 */
class Parser(scanner: Scanner) extends ParserBase(scanner) {

  def parse(): AstBlock = {
    val block = untilAccept("", { case TokenEnd() => true }) { parseTopLevel() }.flatten.toVector
    new AstBlock(block)
  }

  def parseTopLevel(): Option[AstNode] = require("top-level statement") {
    case KeywordTable() => Some(finishTable())
    case KeywordEntity() => Some(finishEntity())
    case id: TokenId =>
      accept {
        case TokenOpenBlock() => Some(finishNamespace(id))
      }.getOrElse {
        val queries = finishQueryStmt(id)
        Some(new AstFreeQuery(queries))
      }
    case TokenNewline() => None
  }

  def finishTable(): AstTable = {
    val name = require("table name") { case id: TokenId => id }
    val cols = acceptMany { case id: TokenId => id }.toVector
    if (cols.isEmpty) errorHere(s"Expected at least one column for table ${name.id}")
    val body = tryParseQueryBody("table body").getOrElse(new AstBlock(Vector[AstNode]()))
    new AstTable(name, cols, body)
  }

  def finishEntity(): AstEntity = {
    val name = require("entity name") { case id: TokenId => id }
    val body = tryParseQueryBody("entiy body").getOrElse(new AstBlock(Vector[AstNode]()))
    new AstEntity(name, body)
  }

  def finishNamespace(name: TokenId): AstNamespace = {
    val block = untilAccept("", { case TokenCloseBlock() => true }) { parseTopLevel() }.flatten.toVector
    new AstNamespace(name, new AstBlock(block))
  }

  def tryParseQueryBody(context: String): Option[AstBlock] = accept {
    case TokenOpenBlock() =>
      val stmt = untilAccept(context, { case TokenCloseBlock() => true }) {
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
    val args = Vector[Vector[AstEx]](first) ++ acceptMany { case TokenComma() => parseQueryTuple() }
    args.map(a => new AstQueryStmt(name, a))
  }

  def parseQueryTuple(): Vector[AstEx] = {
    val result = acceptMany(parseQueryExpr).toVector
    if (result.isEmpty) errorHere("Expected at least one query argument")
    result
  }

  def parseQueryExpr: PartialFunction[Token, AstEx] = {
    case t: TokenId => new AstExId(t)
    case t: TokenString => new AstExString(t)
    case t: TokenNumber => new AstExNumber(t)
  }

}

