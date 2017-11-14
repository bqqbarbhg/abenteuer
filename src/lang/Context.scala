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

  def executeTables(root: AstNode): Unit = {
  }

  def executeAst(root: AstNode): Unit = {
    executeTables(root)
  }

}
