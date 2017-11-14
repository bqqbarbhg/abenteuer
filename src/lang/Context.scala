package lang

import scala.collection.mutable.HashMap

class Context {

  val queryables: HashMap[String, db.Queryable] = new HashMap[String, db.Queryable]()

  def query(table: String, pattern: db.Pattern): Iterator[db.Row] = {
    queryables.get(table) match {
      case Some(queryable) => queryable.query(pattern)
      case None => new util.NoneIterator[db.Row]()
    }
  }

  def createTable(astTab: AstTable): Unit = {
    val table = new db.Table(astTab.columns.size)
  }

  def executeTables(root: AstNode): Unit = root match {
    case AstBlock(stmt) => stmt.foreach(executeTables)
    case AstNamespace(ns, block) => executeTables(block)
    case table: AstTable => createTable(table)
    case _ => // Nop
  }

  def executeAst(root: AstNode): Unit = {
    executeTables(root)
  }

}
