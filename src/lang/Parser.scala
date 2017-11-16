package lang

import scala.collection.immutable.Vector
import scala.collection.mutable.ArrayBuffer

abstract class AstNode(val representingToken: Token) {
  def loc = representingToken.location.prefix
}

case class AstTable(name: TokenId, columns: Vector[TokenId], constraints: AstStmtBlock) extends AstNode(name)
case class AstEntity(name: TokenId, statements: AstStmtBlock) extends AstNode(name)
case class AstBlock(statements: Vector[AstNode], open: Token) extends AstNode(open)
case class AstStmtBlock(statements: Vector[AstStmt], open: Token) extends AstNode(open)
case class AstNamespace(name: TokenId, block: AstBlock) extends AstNode(name)
case class AstFreeQuery(queries: Vector[AstQueryStmt]) extends AstNode(queries(0).operator.path(0))

abstract class AstStmt(representingToken: Token) extends AstNode(representingToken)
case class AstQueryStmt(operator: AstExName, values: Vector[AstEx]) extends AstStmt(operator.path(0))
case class AstNotStmt(stmt: AstStmt, not: TokenNot) extends AstStmt(not)

abstract class AstEx(representingToken: Token) extends AstNode(representingToken)
case class AstExName(path: Vector[TokenId]) extends AstEx(path(0))
case class AstExString(token: TokenString) extends AstEx(token)
case class AstExNumber(token: TokenNumber) extends AstEx(token)
case class AstLambda(arguments: Vector[TokenId], pre: AstStmtBlock, post: AstStmtBlock, firstToken: Token) extends AstEx(firstToken)

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

  expr = STRING | NUMBER | ex-name | ex-lambda ;
  ex-name = ID ( "." ID )*
  ex-lambda = "(" ID* ")" query-body? ("->" query-body)?

 */
class Parser(scanner: Scanner) extends ParserBase(scanner) {

  val firstToken = token

  def parse(): AstBlock = {
    val block = untilAccept("", { case TokenEnd() => true }) { parseTopLevel() }.flatten.toVector
    new AstBlock(block, firstToken)
  }

  def parseTopLevel(): Option[AstNode] = require("top-level statement") {
    case KeywordTable() => Some(finishTable())
    case KeywordEntity() => Some(finishEntity())
    case id: TokenId =>
      accept {
        case t: TokenOpenBlock => Some(finishNamespace(id, t))
      }.getOrElse {
        val queries = finishQueryStmt(finishName(id))
        Some(new AstFreeQuery(queries))
      }
    case TokenNewline() => None
  }

  def finishTable(): AstTable = {
    val name = require("table name") { case id: TokenId => id }
    val cols = acceptMany { case id: TokenId => id }.toVector
    if (cols.isEmpty) errorHere(s"Expected at least one column for table ${name.id}")
    val body = tryParseQueryBody("table body").getOrElse(new AstStmtBlock(Vector[AstStmt](), firstToken))
    new AstTable(name, cols, body)
  }

  def finishEntity(): AstEntity = {
    val name = require("entity name") { case id: TokenId => id }
    val body = tryParseQueryBody("entiy body").getOrElse(new AstStmtBlock(Vector[AstStmt](), firstToken))
    new AstEntity(name, body)
  }

  def finishNamespace(name: TokenId, open: TokenOpenBlock): AstNamespace = {
    val block = untilAccept("", { case TokenCloseBlock() => true }) { parseTopLevel() }.flatten.toVector
    new AstNamespace(name, new AstBlock(block, open))
  }

  def tryParseQueryBody(context: String): Option[AstStmtBlock] = accept {
    case open: TokenOpenBlock =>
      val stmt = untilAccept(context, { case TokenCloseBlock() => true }) {
        val ast: Option[Vector[AstStmt]] = accept[Option[Vector[AstStmt]]] {
          case TokenNewline() => None
        }.getOrElse(Some(parseStmt()))
        ast
      }.flatten.flatten.toVector
      new AstStmtBlock(stmt, open)
  }

  def parseStmt(): Vector[AstStmt] = require("query statement") {
    case not: TokenNot => parseStmt().map(a => new AstNotStmt(a, not))
    case id: TokenId => finishQueryStmt(finishName(id))
  }

  def finishQueryStmt(name: AstExName): Vector[AstQueryStmt] = {
    val first = parseQueryTuple()
    val args = Vector[Vector[AstEx]](first) ++ acceptMany { case TokenComma() => parseQueryTuple() }
    args.map(a => new AstQueryStmt(name, a))
  }

  def parseQueryTuple(): Vector[AstEx] = {
    val result = acceptMany(parseQueryExpr).toVector
    result
  }

  def parseQueryExpr: PartialFunction[Token, AstEx] = {
    case t: TokenId => finishName(t)
    case t: TokenString => new AstExString(t)
    case t: TokenNumber => new AstExNumber(t)
    case t: TokenOpenParen => finishLambda(t)
  }

  def finishName(first: TokenId): AstExName = {
    val rest = acceptMany {
      case TokenDot() =>
        require("name after namespace") { case t: TokenId => t }
    }
    AstExName(Vector(first) ++ rest)
  }

  def finishLambda(openParen: Token): AstLambda = {
    val args = acceptMany { case t: TokenId => t }.toVector
    require("closing ')'") { case TokenCloseParen() => }
    val pre = tryParseQueryBody("lambda pre-body").getOrElse(new AstStmtBlock(Vector[AstStmt](), firstToken))
    val post = accept { case TokenArrow() =>
        tryParseQueryBody("lambda post-body").getOrElse { errorHere("Expected lambda post-body after '=>'") }
    } getOrElse(new AstStmtBlock(Vector[AstStmt](), firstToken))

    new AstLambda(args, pre, post, openParen)
  }

}

