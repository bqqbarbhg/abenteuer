package vm

import scala.collection.mutable.HashMap

abstract class External
case class ExternalAction(func: Action.ExternalFunc) extends External

class Context {

  val queryables: HashMap[String, db.Queryable] = new HashMap[String, db.Queryable]()
  val namedEntities: HashMap[String, Entity] = new HashMap[String, Entity]()
  val entityNameCounter = new HashMap[String, Int]().withDefaultValue(0)
  val externals = new HashMap[String, External]()

  def createEntity(debugName: String): Entity = {
    val count = entityNameCounter(debugName) + 1
    entityNameCounter(debugName) = count
    val name = s"$debugName.$count"
    val entity = new Entity(name)
    namedEntities(name) = entity
    entity
  }

  def query(table: String, pattern: db.Pattern): Iterator[db.Row] = {
    queryables.get(table) match {
      case Some(queryable) => queryable.query(pattern)
      case None => new util.NoneIterator[db.Row]()
    }
  }

}

