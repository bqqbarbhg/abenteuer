package lang

import scala.collection.immutable.Vector
import scala.collection.mutable.HashMap
import scala.collection.mutable.Buffer
import vm.TableConstraint
import lang.AstPrinter.AstWithPrettyPrint

import scala.util.{Failure, Success, Try}
import util.EscapeUtil.EscapedString

abstract class NamedValue {
  var representingToken: Token = null
  def loc = representingToken.location.prefix
  def what: String
}

case class NamedTable(val table: vm.Table) extends NamedValue { override def what: String = "table" }
case class NamedEntity(val entity: vm.Entity) extends NamedValue { override def what: String = "entity" }
case class NamedNamespace(val namespace: Namespace) extends NamedValue { override def what: String = "namespace" }
case class NamedExternal(val external: vm.External) extends NamedValue { override def what: String = "external" }
case class NamedDefine(val value: AstEx, val ns: Namespace) extends NamedValue {
  private var state = 0
  private var evaluatedValue: Option[Any] = None

  def eval(cg: Codegen): Option[Any] = {
    state match {
      case 0 =>
        state = 1
        evaluatedValue = Some(cg.evalLiteralConstant(value, ns))
        state = 2
        evaluatedValue
      case 2 => evaluatedValue
      case _ => None
    }
  }

  override def what: String = "define"
}

class Namespace(val parent: Option[Namespace], val name: String) {
  val fullName: String = parent.map(n => n.namespaced(name)).getOrElse("")

  def namespaced(name: String) = if (fullName.nonEmpty) fullName + "." + name else name

  private val names: HashMap[String, NamedValue] = new HashMap[String, NamedValue]()

  def set(name: TokenId, value: NamedValue): Unit = {
    set(AstExName(Vector(name)), value)
  }

  def create(name: AstExName, value: NamedValue, index: Int = 0): Unit = {
    value.representingToken = name.path.last

    if (index + 1 == name.path.size) {
      val nameStr = name.path(index).id
      names.get(nameStr) match {
        case Some(prev) => throw new CompileError(name.representingToken.location, s"'$nameStr' is already defined as a ${prev.what} at '${prev.loc}'")
        case None => names(nameStr) = value
      }
    } else {
      val ns = name.path(index).id
      names.get(ns) match {
        case Some(NamedNamespace(child)) => child.create(name, value, index + 1)
        case None =>
          val child = new Namespace(Some(this), ns)
          child.create(name, value, index + 1)
          val nn = NamedNamespace(child)
          nn.representingToken = name.path(index)
          names(ns) = nn
        case Some(other) => throw new CompileError(name.representingToken.location, s"Could not create namespace '$ns', already defined as a ${other.what} at ${other.loc}.")
      }
    }
  }

  def set(name: AstExName, value: NamedValue, index: Int = 0): Unit = {
    value.representingToken = name.path.last

    if (index + 1 == name.path.size) {
      val nameStr = name.path(index).id
      names.get(nameStr) match {
        case Some(prev) => throw new CompileError(name.representingToken.location, s"'$nameStr' is already defined as a ${prev.what} at '${prev.loc}'")
        case None => names(nameStr) = value
      }
    } else {
      val ns = name.path(index).id
      names.get(ns) match {
        case Some(NamedNamespace(child)) => child.set(name, value, index + 1)
        case _ => throw new CompileError(name.representingToken.location, s"${name.loc}: Namespace '$ns' not found!")
      }
    }
  }

  def get(name: AstExName, backtrace: Boolean = true, index: Int = 0): Option[NamedValue] = {
    val value = if (index + 1 == name.path.size) {
      names.get(name.path(index).id)
    } else {
      val ns = name.path(index).id
      names.get(ns) match {
        case Some(NamedNamespace(child)) => child.get(name, false, index + 1)
        case _ => None
      }
    }

    if (backtrace && value.isEmpty && parent.isDefined) {
      parent.get.get(name, true, 0)
    } else {
      value
    }
  }
}

class BindCollection(val args: Vector[TokenId]) {
  val binds: Buffer[String] = args.map(_.id).toBuffer
  val constants = Buffer[Any]()

  def evalArg(cg: Codegen, ex: AstEx, ns: Namespace, allowCreate: Boolean): Int = ex match {
    case t: AstExName =>
      if (t.path.length > 1) cg.error(t, "Namespaces are not supported in local names!")
      val name = t.path.head.id
      binds.indexOf(name) match {
        case -1 =>
          if (!allowCreate) cg.error(t, s"A new bind '${name}' cannot be defined here!")
          binds += name
          binds.length - 1
        case valid => valid
      }
    case w: AstExWildcard =>
      Int.MinValue
    case ex =>
      val value = cg.evalLiteralConstant(ex, ns)
      val ix = constants.indexOf(value) match {
        case -1 =>
          constants += value
          constants.length - 1
        case valid => valid
      }

      // Constants have purely negative indices, so first constant is -1
      -ix - 1
  }
}

object Codegen {
  def codegen(context: vm.Context, roots: Seq[AstNode]): Unit = {
    val cg = new Codegen(context)
    cg.doCodegen(roots)
  }
}

class Codegen(val context: vm.Context) {

  val rootNamespace = new Namespace(None, "")

  def error(ast: AstNode, message: String, auxLoc: Any*): Nothing = {
    throw new CompileError(ast.representingToken.location, message, auxLoc.map {
      case t: Token => t.location
      case a: AstNode => a.representingToken.location
      case l: SourceLocation => l
      case n: NamedValue => n.representingToken.location
    }.toVector)
  }

  def error(token: Token, message: String): Nothing = {
    throw new CompileError(token.location, message)
  }

  def evalLiteralConstant(ast: AstEx, ns: Namespace): Any = {
    ast match {
      case t: AstExName => ns.get(t) match {
        case Some(e: NamedDefine) => e.eval(this).getOrElse { error(t, s"Cyclical dependency in define '${t.prettyPrint}', defined at ${e.loc}", e) }
        case Some(NamedEntity(e)) => e
        case Some(NamedExternal(e)) => e
        case Some(NamedTable(e)) => e
        case Some(something) => error(ast, s"'${t.prettyPrint}' is a ${something.what} instead of a literal value, see definition at ${something.loc}", something)
        case None => error(ast, s"Could not find literal value '${t.prettyPrint}'")
      }
      case t: AstExNumber => t.token.value
      case t: AstExString => t.token.value
      case t: AstExLambda => evalLambda(t, ns)
      case t: AstExValueName => evalLiteralConstant(t.name, ns)
      case _ => error(ast, "Not a valid literal expression")
    }
  }

  private def getTable(name: AstExName, numArgs: Int, ns: Namespace): vm.Table = {
    val table = ns.get(name) match {
      case Some(NamedTable(table)) => table
      // case Some(e: NamedDefine) => e.eval(this).getOrElse { error(t, s"Cyclical dependency in define '${t.prettyPrint}', defined at ${e.loc}", e) }
      case Some(other) => error(name, s"Expected a table, '${name.prettyPrint}' is ${other.what}, see ${other.loc}", other)
      case None => error(name, s"Undefined table '${name.prettyPrint}'")
    }

    if (numArgs != table.maxColumns)
      error(name, s"Table '${table.name}' has ${table.maxColumns} columns but pattern has ${numArgs}")

    table
  }

  private def evalRuleBlock(block: AstStmtBlock, binds: BindCollection, ns: Namespace): Vector[vm.Condition] = {
    def evalStmt(stmt: AstStmt): vm.Condition = stmt match {
      case t: AstQueryStmt =>
        val args = t.values.map(ex => binds.evalArg(this, ex, ns, true))
        evalLiteralConstant(t.operator, ns) match {
          case table: vm.Table =>
            new vm.QueryCondition(table.name, args)
          case rule: vm.Rule =>
            new vm.RuleCondition(rule, args)
          case something =>
            error(t.operator, s"Not a valid queryable object: $something!")
        }

      case t: AstIndirectQueryStmt =>
        val args = t.query.values.map(ex => binds.evalArg(this, ex, ns, true))
        val bind = binds.evalArg(this, t.query.operator, ns, false)
        new vm.IndirectCondition(bind, args)

      case t: AstNotStmt => new vm.NegationCondition(evalStmt(t.stmt))

      case other => error(other, "Unexpected statement in query block")
    }

    block.statements.map(evalStmt).toVector
  }

  private def evalActionBlock(block: AstStmtBlock, binds: BindCollection, ns: Namespace): Vector[vm.Action] = {
    def evalStmt(stmt: AstStmt): vm.Action = stmt match {
      case t: AstQueryStmt =>
        val table = getTable(t.operator, t.values.length, ns)
        val args = t.values.map(ex => binds.evalArg(this, ex, ns, false))
        new vm.TableUpdateAction(table, args)

      case t: AstIndirectQueryStmt =>
        val bind = binds.evalArg(this, t.query.operator, ns, false)
        val args = t.query.values.map(ex => binds.evalArg(this, ex, ns, false))
        new vm.IndirectAction(bind, args)

      case t: AstNotStmt =>
        val query = t.stmt match {
          case q: AstQueryStmt => q
          case other => error(t.stmt, "Only negation of query statements is allowed in action blocks")
        }
        val table = getTable(query.operator, query.values.length, ns)
        val args = query.values.map(ex => binds.evalArg(this, ex, ns, false))
        new vm.TableDeleteAction(table, args)

      case t: AstActionStmt =>
        val args = t.query.values.map(ex => binds.evalArg(this, ex, ns, false))
        evalLiteralConstant(t.query.operator, ns) match {
          case func: vm.ExternalAction => new vm.ExternalFuncAction(func.func, args)
          case rule: vm.Rule => new vm.RuleAction(rule, args)
          case other => error(t.query.operator, s"Invalid action '${t.query.operator.prettyPrint}'")
        }

      case other => error(other, "Unexpected statement in action block")
    }

    block.statements.map(evalStmt).toVector
  }

  private def evalLambda(lambda: AstExLambda, ns: Namespace): vm.Rule = {
    val binds = new BindCollection(lambda.arguments)
    val conditions = evalRuleBlock(lambda.pre, binds, ns)
    val actions = evalActionBlock(lambda.post, binds, ns)
    new vm.Rule(context, binds.binds.toVector, lambda.arguments.map(_.id), conditions, actions, binds.constants.toVector)
  }

  private def doConstraint(stmt: AstStmt, ns: Namespace): vm.TableConstraint = {
    val query = stmt match {
      case q: AstQueryStmt => q
      case other => error(other, "Invalid table constraint, expected a simple query statement")
    }

    query.operator.path.map(_.id).mkString(".") match {
      case "unique" =>
        query.values match {
          case Vector(AstExName(Vector(tok))) => new TableConstraint.Unique(tok.id)
          case _ => error(query, "Expected single column name argument for 'unique'")
        }
      case "unordered" =>
        query.values match {
          case Vector(AstExName(Vector(tok0)), AstExName(Vector(tok1))) => new TableConstraint.Unordered(tok0.id, tok1.id)
          case _ => error(query, "Expected two column name arguments for 'unordered'")
        }
      case "default" =>
        query.values match {
          case Vector(AstExName(Vector(tok)), ex) =>
            val value = evalLiteralConstant(ex, ns)
            new TableConstraint.Default(tok.id, value)
          case _ => error(query, "Expected a column name and a value for 'default'")
        }
      case "shared" =>
        query.values match {
          case Vector() => TableConstraint.Shared
          case _ => error(query, "No arguments expected for 'shared'")
        }
      case other => error(query, s"Unknown table constraint '$other'")
    }
  }

  private def doTable(table: AstTable, ns: Namespace): Unit = {
    val constraints = table.constraints.statements.map(a => doConstraint(a, ns))
    val name = ns.namespaced(table.name.path.map(_.id).mkString("."))

    val isShared = constraints.collect { case TableConstraint.Shared => }.nonEmpty
    val maybeShared = if (isShared) {
      context.findSharedTable(name)
    } else {
      None
    }

    val tab = maybeShared.getOrElse(Try(new vm.Table(context, name, table.columns.map(_.id), constraints)) match {
      case Success(tab) => tab
      case Failure(err) => error(table, s"Failed to create table '${table.name.prettyPrint}': ${err.getMessage}")
    })

    if (isShared && maybeShared.isEmpty) {
      context.defineSharedTable(tab)
    }

    ns.create(table.name, NamedTable(tab))
    context.queryables(tab.name) = tab
  }

  private def doStatements(self: Option[vm.Entity], stmts: Vector[AstStmt], ns: Namespace): Unit = {
    for (stmt <- stmts) stmt match {
      case t: AstQueryStmt =>
        val table = ns.get(t.operator) match {
          case Some(NamedTable(tab)) => tab
          case Some(other) => error(t.operator, s"'${t.operator.prettyPrint}' is defined as a ${other.what}, not a table, see definition at ${other.loc}", other)
          case None => error(t.operator, s"Table not found: '${t.operator.prettyPrint}'")
        }

        val values = (self ++ t.values.map(a => evalLiteralConstant(a, ns))).toArray

        if (values.length > table.maxColumns) {
          if (self.isDefined) {
            error(t.values(table.maxColumns - 1), s"Too many values provided for '${table.name}' (${table.maxColumns} expected at most, ${values.length} given including implicit self)")
          } else {
            error(t.values(table.maxColumns), s"Too many values provided for '${table.name}' (${table.maxColumns} expected at most, ${values.length} given)")
          }
        } else if (values.length < table.minColumns) {
          if (self.isDefined) {
            error(t.values.lastOption.getOrElse(stmt), s"Not enough values provided for '${table.name}' (${table.minColumns} expected at least, ${values.length} given including implicit self)")
          } else {
            error(t.values.lastOption.getOrElse(stmt), s"Not enough values provided for '${table.name}' (${table.minColumns} expected at least, ${values.length} given)")
          }
        }

        table.insert(values)

      case t: AstNotStmt => error(t, "Inverted statements are not supported in definitions")
    }
  }

  private def doEntity(astEntity: AstEntity, ns: Namespace): Unit = {
    val entity = ns.get(astEntity.name) match {
      case Some(NamedEntity(e)) => e
      case Some(other) => error(astEntity.name, s"Internal error: Entity clobbered by ${other.what} at ${other.loc} after namespace pass", other)
      case None => error(astEntity.name, s"Internal error: Entity not defined after namespace pass")
    }

    doStatements(Some(entity), astEntity.statements.statements, ns)
  }

  private def doNamespaces(node: AstNode, ns: Namespace): Unit = node match {
    case block: AstBlock => block.statements.foreach(a => doNamespaces(a, ns))
    case namespace: AstNamespace =>
      val ns2 = ns.get(namespace.name, false) match {
        case Some(NamedNamespace(ns2)) => ns2
        case _ =>
          val ns2 = new Namespace(Some(ns), namespace.name.path.last.id)
          ns.create(namespace.name, NamedNamespace(ns2))
          ns2
      }
      doNamespaces(namespace.block, ns2)
    case astEntity: AstEntity =>
      val entity = context.createEntity(astEntity.name.path.last.id)
      ns.create(astEntity.name, NamedEntity(entity))
    case astDefine: AstDefine =>
      val define = NamedDefine(astDefine.value, ns)
      ns.create(astDefine.name, define)
    case astExternal: AstExternal =>
      val value = context.externals.get(astExternal.foreign.value).getOrElse {
        error(astExternal.foreign, s"External name '${astExternal.foreign.value.escape}' is not defined")
      }
      val external = NamedExternal(value)
      ns.create(astExternal.name, external)
    case _ => // Nop
  }

  private def doTables(node: AstNode, ns: Namespace): Unit = node match {
    case block: AstBlock => block.statements.foreach(a => doTables(a, ns))
    case namespace: AstNamespace => ns.get(namespace.name) match {
      case Some(NamedNamespace(ns2)) => doTables(namespace.block, ns2)
      case Some(other) => error(namespace, s"Internal error: Namespace clobbered by ${other.what} at ${other.loc} after namespace pass", other)
      case None => error(namespace, s"Internal error: Namespace not defined after namespace pass")
    }
    case astTable: AstTable => doTable(astTable, ns)
    case astDefine: AstDefine => ns.get(astDefine.name) match {
      case Some(nd: NamedDefine) => nd.eval(this)
      case Some(other) => error(astDefine, s"Internal error: Define clobbered by ${other.what} at ${other.loc} after namespace pass", other)
      case None => error(astDefine, s"Internal error: Define not defined after namespace pass")
    }
    case _ => // Nop
  }

  private def doEntities(node: AstNode, ns: Namespace): Unit = node match {
    case block: AstBlock => block.statements.foreach(a => doEntities(a, ns))
    case namespace: AstNamespace => ns.get(namespace.name) match {
      case Some(NamedNamespace(ns2)) => doEntities(namespace.block, ns2)
      case Some(other) => error(namespace, s"Internal error: Namespace clobbered by ${other.what} at ${other.loc} after namespace pass", other)
      case None => error(namespace, s"Internal error: Namespace not defined after namespace pass")
    }
    case astEntity: AstEntity => doEntity(astEntity, ns)
    case astFreeQuery: AstFreeQuery => doStatements(None, astFreeQuery.queries, ns)
    case _ => // Nop
  }

  def doCodegen(roots: Seq[AstNode]): Unit = {
    roots.foreach(a => doNamespaces(a, rootNamespace))
    roots.foreach(a => doTables(a, rootNamespace))
    roots.foreach(a => doEntities(a, rootNamespace))
  }

}
