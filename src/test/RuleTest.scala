package test

import scala.collection.mutable.Buffer
import scala.collection.immutable.Vector

object RuleTest extends App {

  val ctx = new lang.Context()

  val tableHas = new db.Table(2)
  val tableName = new db.Table(2)
  val tableNoInventory = new db.Table(1)

  ctx.queryables("has") = tableHas
  ctx.queryables("name") = tableName
  ctx.queryables("noInventory") = tableNoInventory

  tableHas.insert(db.Row("player", "key"))
  tableHas.insert(db.Row("player", "thing"))

  tableName.insert(db.Row("player", "Player"))
  tableName.insert(db.Row("key", "Some Key"))
  tableName.insert(db.Row("thing", "Some Thing"))

  tableNoInventory.insert(db.Row("thing"))

  val binds = Vector("holder", "item", "name")
  val conditions = Vector[lang.Condition](
    new lang.QueryCondition("has", Vector(0, 1)),
    new lang.NegationCondition(new lang.QueryCondition("noInventory", Vector(1))),
    new lang.QueryCondition("name", Vector(1, 2))
  )

  val rule = new lang.Rule(binds, conditions)
  for (res <- rule.query(ctx)) {
    println(res.mkString(", "))
  }
}

