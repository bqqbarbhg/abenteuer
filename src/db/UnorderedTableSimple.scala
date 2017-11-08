package db

import scala.collection.mutable.ArrayBuffer
import db.UnorderedTableSimple._
import util.SimpleIterator

object UnorderedTableSimple {

  def lessThan(a: Any, b: Any): Boolean = {
    // Thanks again, JVM
    val ac = a.asInstanceOf[java.lang.Comparable[Any]]
    val bc = b.asInstanceOf[java.lang.Comparable[Any]]
    ac.compareTo(bc) < 0
  }

  def normalizeRow(row: Row, colA: Int, colB: Int): Row = {
    // Swap the two values if they're the wrong way
    val va = row(colA)
    val vb = row(colB)
    if (lessThan(vb, va)) {
      val copy = row.clone()
      copy(colA) = vb
      copy(colB) = va
      copy
    } else {
      row
    }
  }

  /** Iterator that combines two row iterators and returns their union */
  class DualIndexIterator(private val itA: Iterator[Table.RowIndex], private val itB: Iterator[Table.RowIndex]
                         ) extends SimpleIterator[Table.RowIndex] {

    def advance(it: Iterator[Table.RowIndex]) = if (it.hasNext) Some(it.next()) else None

    private var valA: Option[Table.RowIndex] = advance(itA)
    private var valB: Option[Table.RowIndex] = advance(itB)

    override def nextOption(): Option[Table.RowIndex] = {
      if (valA.isDefined && valB.isDefined) {
        val a = valA.get
        val b = valB.get

        // Advance the iterator which has the lower row index
        if (a < b) {
          valA = advance(itA)
          Some(a)
        } else if (a > b) {
          valB = advance(itB)
          Some(b)
        } else {
          // Both iterators have the row, return and skip in sync
          valA = advance(itA)
          valB = advance(itB)
          Some(a)
        }

      } else if (valA.isDefined) {
        val ret = valA
        valA = advance(itA)
        ret
      } else if (valB.isDefined) {
        val ret = valB
        valB = advance(itB)
        ret
      } else {
        None
      }
    }
  }

  /** Wraps an itearator of row indices and translates the results to data rows.
    * Swizzles unordered values to right places within the groups.
    */
  class UnorderedRowDataIteratorSimple(private val rowData: ArrayBuffer[Row],
                                       private val indexIterator: Iterator[Table.RowIndex], private val colA: Int,
                                       private val colB: Int, private val pattern: Pattern) extends Iterator[Row] {
    def hasNext: Boolean = indexIterator.hasNext
    def next(): Row = {
      val row = rowData(indexIterator.next())
      // If the pattern defines a value and they don't align swap them
      if ((pattern(colA).isDefined && pattern(colA).get != row(colA)) ||
          (pattern(colB).isDefined && pattern(colB).get != row(colB))) {
        val newRow = row.clone()
        newRow(colA) = row(colB)
        newRow(colB) = row(colA)
        newRow
      } else {
        row
      }
    }
  }
}

/** Special case of UnorderedTable where only two columns can have an unordered relation
  * Offers much higher performance and simplicity.
  */
class UnorderedTableSimple(numColumns: Int, val colA: Int, val colB: Int) extends Table(numColumns) {

  override def queryRows(pattern: Pattern): Iterator[Table.RowIndex] = {
    if (pattern(colA).isEmpty && pattern(colB).isEmpty) {
      // Both unordered columns are None, no special handling
      super.queryRows(pattern)
    } else if (pattern(colA).isDefined && pattern(colB).isDefined) {
      // Both unordered columns are defined, fix order and keep going
      val newPat = pattern.clone()
      newPat(colB) = pattern(colA)
      newPat(colA) = pattern(colB)
      super.queryRows(newPat)
    } else {
      // Only one of the unordered columns are defined, need to do two queries
      // and combine the results
      val newPat = pattern.clone()
      newPat(colB) = pattern(colA)
      newPat(colA) = pattern(colB)
      val itA = super.queryRows(pattern)
      val itB = super.queryRows(newPat)
      new DualIndexIterator(itA, itB)
    }
  }

  override def query(pattern: Pattern): Iterator[Row] = {
    assert(pattern.length == numColumns)
    val indexIterator = queryRows(pattern)
    new UnorderedRowDataIteratorSimple(rowData, indexIterator, colA, colB, pattern)
  }

  /** Insert a new row into the table */
  override def insert(row: Row): Unit = {
    super.insert(normalizeRow(row, colA, colB))
  }
}


