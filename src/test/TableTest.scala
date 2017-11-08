package test

object TableTest extends App {

  // name, place, age
  val table = new db.Table(3)
  table.insert(db.Row("A", "Espoo", 200))
  table.insert(db.Row("B", "Espoo", 200))
  table.insert(db.Row("C", "Espoo", 300))
  table.insert(db.Row("D", "Helsinki", 200))
  table.insert(db.Row("E", "Helsinki", 100))
  table.insert(db.Row("F", "Helsinki", 200))

  for (row <- table.query(db.Pattern(None, None, None))) {
    println(row.mkString(", "))
  }

  table.remove(db.Pattern(Some("C"), None, None))

  for (row <- table.query(db.Pattern(None, None, None))) {
    println(row.mkString(", "))
  }

  for (row <- table.query(db.Pattern(None, Some("Espoo"), None))) {
    println(row.mkString(", "))
  }

  val ut = new db.UnorderedTableSimple(2, 0, 1)
  ut.insert(db.Row(1, 5))
  val ret = ut.query(db.Pattern(Some(5), None)).map(_.mkString(", ")).mkString("\n")
  println(ret)
}
