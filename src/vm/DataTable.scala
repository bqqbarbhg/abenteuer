package vm

import vm.TableConstraint._

abstract class TableConstraint

object TableConstraint {
  /** Constraint that ensures that all values of column `col` are unique */
  case class Unique(col: String) extends TableConstraint
  /** Not really a constraint, but makes two columns unordered in respect to each other */
  case class Unordered(col0: String, col1: String) extends TableConstraint
  /** Spcecify a default value `value` for a column `col` */
  case class Default(col: String, value: Any) extends TableConstraint
}

class Table(val context: Context, val name: String, val columns: Vector[String], val constraints: Vector[TableConstraint]) extends db.Queryable {

  override def toString: String = s"Table($name ${columns.mkString(" ")})"

  val defaultValues = constraints.collect { case c: Default => c }.sortBy(c => columns.indexOf(c.col)).toVector
  for ((dv, i) <- defaultValues.zipWithIndex) {
    assert(Some(dv.col) == columns.lift(columns.size - defaultValues.size + i), "Default values must be in the last columns")
  }

  def maxColumns: Int = columns.length
  def minColumns: Int = columns.length - defaultValues.length

  def query(pattern: db.Pattern): Iterator[db.Row] = ???
}
