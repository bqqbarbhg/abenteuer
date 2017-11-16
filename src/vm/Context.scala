package vm

import scala.collection.mutable.HashMap

class Context {

  val queryables: HashMap[String, db.Queryable] = new HashMap[String, db.Queryable]()
  val entityNameCounter = new HashMap[String, Int]().withDefaultValue(0)

  def createEntity(debugName: String): Entity = {
    val count = entityNameCounter(debugName) + 1
    entityNameCounter(debugName) = count
    new Entity(s"$debugName.$count")
  }

  def query(table: String, pattern: db.Pattern): Iterator[db.Row] = {
    queryables.get(table) match {
      case Some(queryable) => queryable.query(pattern)
      case None => new util.NoneIterator[db.Row]()
    }
  }

}

