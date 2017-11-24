package lang

import scala.collection.immutable.Vector
import scala.collection.mutable.ArrayBuffer

abstract class AstNode(val representingToken: Token) {
  def loc = representingToken.location.prefix
}

case class AstTable(name: AstExName, columns: Vector[TokenId], constraints: AstStmtBlock) extends AstNode(name.path(0))
case class AstEntity(name: AstExName, statements: AstStmtBlock) extends AstNode(name.path(0))
case class AstDefine(name: AstExName, value: AstEx) extends AstNode(name.path(0))
case class AstExternal(name: AstExName, foreign: TokenString) extends AstNode(name.path(0))
case class AstBlock(statements: Vector[AstNode], open: Token) extends AstNode(open)
case class AstStmtBlock(statements: Vector[AstStmt], open: Token) extends AstNode(open)
case class AstNamespace(name: AstExName, block: AstBlock) extends AstNode(name.path(0))
case class AstFreeQuery(queries: Vector[AstQueryStmt]) extends AstNode(queries(0).operator.path(0))

abstract class AstStmt(representingToken: Token) extends AstNode(representingToken)
case class AstQueryStmt(operator: AstExName, values: Vector[AstEx]) extends AstStmt(operator.path(0))
case class AstNotStmt(stmt: AstStmt, not: TokenNot) extends AstStmt(not)
case class AstActionStmt(query: AstQueryStmt) extends AstStmt(query.representingToken)

abstract class AstEx(representingToken: Token) extends AstNode(representingToken)
case class AstExName(path: Vector[TokenId]) extends AstEx(path(0))
case class AstExString(token: TokenString) extends AstEx(token)
case class AstExNumber(token: TokenNumber) extends AstEx(token)
case class AstExLambda(arguments: Vector[TokenId], pre: AstStmtBlock, post: AstStmtBlock, firstToken: Token) extends AstEx(firstToken)
case class AstExValueName(name: AstExName, firstToken: Token) extends AstEx(firstToken)

case class CompileError(location: SourceLocation,
                        message: String, auxLoc: Vector[SourceLocation] = Vector[SourceLocation]()) extends Exception(s"${location.file}:${location.line}:${location.column}: $message")
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

  top-level = "table" ex-name ID* query-body?
            | "entity" ex-name query-body?
            | "define" ex-name query-tuple
            | "external" ex-name STRING
            | ex-name "{" top-level* "}"
            | query-stmt
            | <newline> ;

  query-stmt = ex-name ":"? query-tuple ("," query-tuple)* ;
  query-tuple = expr* ;
  query-body = "{" ( "!"? query-stmt* | <newline> ) "}"

  expr = STRING | NUMBER | ex-name | ex-lambda | ex-value ;
  ex-name = ID ( "." ID )*
  ex-lambda = "(" ID* ")" query-body? ("->" query-body)?
  ex-value = "*" ex-name

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
    case KeywordDefine() => Some(finishDefine())
    case KeywordExternal() => Some(finishExternal())
    case id: TokenId =>
      val name = finishName(id)
      accept {
        case t: TokenOpenBlock => Some(finishNamespace(name, t))
      }.getOrElse {
        val queries = finishQueryStmt(name)
        Some(new AstFreeQuery(queries))
      }
    case TokenNewline() => None
  }

  def finishTable(): AstTable = {
    val nameBegin = require("table name") { case id: TokenId => id }
    val name = finishName(nameBegin)
    val cols = acceptMany { case id: TokenId => id }.toVector
    if (cols.isEmpty) errorHere(s"Expected at least one column for table ${name.path.last.id}")
    val body = tryParseQueryBody("table body").getOrElse(new AstStmtBlock(Vector[AstStmt](), firstToken))
    new AstTable(name, cols, body)
  }

  def finishEntity(): AstEntity = {
    val nameBegin = require("entity name") { case id: TokenId => id }
    val name = finishName(nameBegin)
    val body = tryParseQueryBody("entiy body").getOrElse(new AstStmtBlock(Vector[AstStmt](), firstToken))
    new AstEntity(name, body)
  }

  def finishDefine(): AstDefine = {
    val nameBegin = require("define name") { case id: TokenId => id }
    val name = finishName(nameBegin)
    val value = require("define value")(parseQueryExpr)
    new AstDefine(name, value)
  }

  def finishExternal(): AstExternal = {
    val nameBegin = require("external local name") { case id: TokenId => id }
    val name = finishName(nameBegin)
    val foreign = require("external foreign name") { case t: TokenString => t }
    new AstExternal(name, foreign)
  }

  def finishNamespace(name: AstExName, open: TokenOpenBlock): AstNamespace = {
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
    case id: TokenId =>
      val name = finishName(id)
      accept[Vector[AstStmt]] {
        case t: TokenColon => finishQueryStmt(name).map(new AstActionStmt(_))
      }.getOrElse(finishQueryStmt(name))
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
    case t: TokenValuePrefix =>
      val nameBegin = require("value name") { case id: TokenId => id }
      val name = finishName(nameBegin)
      new AstExValueName(name, t)
  }

  def finishName(first: TokenId): AstExName = {
    val rest = acceptMany {
      case TokenDot() =>
        require("name after namespace") { case t: TokenId => t }
    }
    AstExName(Vector(first) ++ rest)
  }

  def finishLambda(openParen: Token): AstExLambda = {
    val args = acceptMany { case t: TokenId => t }.toVector
    require("closing ')'") { case TokenCloseParen() => }
    val pre = tryParseQueryBody("lambda pre-body").getOrElse(new AstStmtBlock(Vector[AstStmt](), firstToken))
    val post = accept { case TokenArrow() =>
        tryParseQueryBody("lambda post-body").getOrElse { errorHere("Expected lambda post-body after '=>'") }
    } getOrElse(new AstStmtBlock(Vector[AstStmt](), firstToken))

    new AstExLambda(args, pre, post, openParen)
  }

}

