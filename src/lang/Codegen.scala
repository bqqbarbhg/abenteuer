package lang

import scala.collection.immutable.Vector
import scala.collection.mutable.HashMap
import vm.TableConstraint

abstract class NamedValue {
  var representingToken: Token = null
  def loc = representingToken.location.prefix
}

case class NamedTable(val tableName: String) extends NamedValue
case class NamedEntity(val entity: vm.Entity) extends NamedValue
case class NamedNamespace(val namespace: Namespace) extends NamedValue

class Namespace(val parent: Option[Namespace]) {

  private def names: HashMap[String, NamedValue] = new HashMap[String, NamedValue]()

  def set(name: TokenId, value: NamedValue): Unit = {
    set(AstExName(Vector(name)), value)
  }

  def set(name: AstExName, value: NamedValue, index: Int = 0): Unit = {
    if (index + 1 == name.path.size) {
      val nameStr = name.path(index).id
      names.get(nameStr) match {
        case Some(prev) => throw new RuntimeException(s"${name.loc}: '$nameStr' is already defined at '${prev.loc}'")
        case None => names(nameStr) = value
      }
    } else {
      val ns = name.path(index).id
      names.get(ns) match {
        case Some(NamedNamespace(child)) => child.set(name, value, index + 1)
        case _ => throw new RuntimeException(s"${name.loc}: Namespace '$ns' not found!")
      }
    }
  }

  def get(name: AstExName, index: Int = 0): Option[NamedValue] = {
    val value = if (index + 1 == name.path.size) {
      names.get(name.path(index).id)
    } else {
      val ns = name.path(index).id
      names.get(ns) match {
        case Some(NamedNamespace(child)) => child.get(name, index + 1)
        case _ => None
      }
    }

    if (index == 0 && value.isEmpty && parent.isDefined) {
      parent.get.get(name, 0)
    } else {
      value
    }
  }
}

class Codegen(val context: vm.Context) {

  def error(ast: AstNode, message: String): Nothing = {
    throw new RuntimeException(s"${ast.loc}: $message")
  }

  private def evalLiteralConstant(ast: AstEx): Any = {
    ast match {
      case t: AstExNumber => t.token.value
      case t: AstExString => t.token.value
      case _ =>
    }
  }

  private def doConstraint(stmt: AstStmt): vm.TableConstraint = {
    val query = stmt match {
      case q: AstQueryStmt => q
      case other => error(other, "Invalid table constraint, expected a simple query statement")
    }

    query.operator.path.mkString(".") match {
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
      case "default" => ???
      case other => error(query, s"Unknown table constraint '$other'")
    }
  }

  private def doTable(table: AstTable): vm.Table = {
    val constraints = table.constraints.statements.map(doConstraint(_))
    new vm.Table(context, table.name.id, table.columns.map(_.id), constraints)
  }

  def doNamespaces(node: AstNode, ns: Namespace): Unit = node match {
    case block: AstBlock => block.statements.foreach(a => doNamespaces(a, ns))
    case namespace: AstNamespace =>
      val ns2 = new Namespace(Some(ns))
      ns.set(namespace.name, NamedNamespace(ns2))
      doNamespaces(namespace.block, ns2)
    case _ => // Nop
  }

}
