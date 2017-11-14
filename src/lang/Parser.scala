package lang

import scala.collection.immutable.Vector
import scala.collection.mutable.ArrayBuffer

abstract class AstNode

case class AstTable(name: TokenId, columns: Vector[TokenId], constraints: AstBlock) extends AstNode
case class AstEntity(name: TokenId, statements: AstBlock) extends AstNode
case class AstBlock(statements: Vector[AstNode]) extends AstNode
case class AstNamespace(name: TokenId, block: AstBlock) extends AstNode
case class AstFreeQuery(queries: Vector[AstQueryStmt]) extends AstNode

abstract class AstStmt extends AstNode
case class AstQueryStmt(operator: TokenId, values: Vector[AstEx]) extends AstStmt
case class AstNotStmt(stmt: AstStmt) extends AstStmt

abstract class AstEx extends AstNode
case class AstExId(token: TokenId) extends AstEx
case class AstExString(token: TokenString) extends AstEx
case class AstExNumber(token: TokenNumber) extends AstEx
case class AstLambda(arguments: Vector[TokenId], pre: AstBlock, post: AstBlock) extends AstEx

case class CompileError(location: SourceLocation,
                        message: String) extends Exception(s"${location.file}:${location.line}:${location.column}: $message")
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
  query-tuple = expr* ;
  query-body = "{" ( "!"? query-stmt* | <newline> ) "}"

  expr = ID | STRING | NUMBER | lambda ;

  lambda = "(" ID* ")" query-body? ("->" query-body)?

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
        val ast: Option[Vector[AstStmt]] = accept[Option[Vector[AstStmt]]] {
          case TokenNewline() => None
        }.getOrElse(Some(parseStmt()))
        ast
      }.flatten.flatten.toVector
      new AstBlock(stmt)
  }

  def parseStmt(): Vector[AstStmt] = require("query statement") {
    case TokenNot() => parseStmt().map(new AstNotStmt(_))
    case id: TokenId => finishQueryStmt(id)
  }

  def finishQueryStmt(name: TokenId): Vector[AstQueryStmt] = {
    val first = parseQueryTuple()
    val args = Vector[Vector[AstEx]](first) ++ acceptMany { case TokenComma() => parseQueryTuple() }
    args.map(a => new AstQueryStmt(name, a))
  }

  def parseQueryTuple(): Vector[AstEx] = {
    val result = acceptMany(parseQueryExpr).toVector
    result
  }

  def parseQueryExpr: PartialFunction[Token, AstEx] = {
    case t: TokenId => new AstExId(t)
    case t: TokenString => new AstExString(t)
    case t: TokenNumber => new AstExNumber(t)
    case TokenOpenParen() => finishLambda()
  }

  def finishLambda(): AstLambda = {
    val args = acceptMany { case t: TokenId => t }.toVector
    require("closing ')'") { case TokenCloseParen() => }
    val pre = tryParseQueryBody("lambda pre-body").getOrElse(new AstBlock(Vector[AstNode]()))
    val post = accept { case TokenArrow() =>
        tryParseQueryBody("lambda post-body").getOrElse { errorHere("Expected lambda post-body after '=>'") }
    } getOrElse(new AstBlock(Vector[AstNode]()))

    new AstLambda(args, pre, post)
  }

}

