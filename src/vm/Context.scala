package vm

import scala.collection.mutable.HashMap
import scala.reflect.ClassTag

abstract class External
case class ExternalAction(func: Action.ExternalFunc) extends External

class SharedContext {
  val tables = new HashMap[String, vm.Table]
}

class Context(val shared: SharedContext = new SharedContext()) {

  val queryables: HashMap[String, db.Queryable] = new HashMap[String, db.Queryable]()
  val namedEntities: HashMap[String, Entity] = new HashMap[String, Entity]()
  val entityNameCounter = new HashMap[String, Int]().withDefaultValue(0)
  val externals = new HashMap[String, External]()

  def findSharedTable(name: String): Option[vm.Table] = shared.tables.get(name)
  def defineSharedTable(table: vm.Table): Unit = shared.tables.update(table.name, table)

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

  def query[T1](table: String)(t1: Option[T1]): Iterator[T1] = {
    query(table, db.Pattern(t1)).map(row => (row(0).asInstanceOf[T1]))
  }

  def query[T1, T2](table: String)(t1: Option[T1], t2: Option[T2]): Iterator[(T1, T2)] = {
    query(table, db.Pattern(t1, t2)).map(row => (row(0).asInstanceOf[T1], row(1).asInstanceOf[T2]))
  }

  def query[T1, T2, T3](table: String)(t1: Option[T1], t2: Option[T2], t3: Option[T3]): Iterator[(T1, T2, T3)] = {
    query(table, db.Pattern(t1, t2, t3)).map(row => (row(0).asInstanceOf[T1], row(1).asInstanceOf[T2], row(2).asInstanceOf[T3]))
  }

}

