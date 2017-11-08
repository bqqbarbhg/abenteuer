package db

import db.UnorderedTable._
import scala.collection.mutable.TreeSet
import scala.collection.mutable.ArrayBuffer

object UnorderedTable {

  type ColumnGroups = Array[Array[Int]]

  /** Normalize the unordered groups in the row to the canonical sorting, for example
    * with the ordering group ((0, 1)) the rows ("A", "B", 5) and ("B", "A", 5) both
    * normalize to ("A", "B", 5)
    */
  def normalizeRow(row: Row, grouping: ColumnGroups): Row = {
    val newRow = row.clone()
    for (group <- grouping) {
      // Thank JVM for letting us do this mess
      val values = group.map(row(_))
      val order = values.map(_.asInstanceOf[java.lang.Comparable[Any]]).sortWith(_.compareTo(_) < 0)
      for ((index, value) <- group.zip(order)) {
        newRow(index) = value
      }
    }
    newRow
  }

  /** Normalizing a pattern is not as straightforward as normalizing rows, consider
    * the following case: The table contains both row ("A", "B"), ("B", "C") and
    * the pattern is partially specified, such as (Some("B"), None). If the pattern
    * would be fully specified the simple ordering would give the correct results,
    * but without the knowledge of the second column we don't know which column
    * the constraint "B" should be normalized to!
    * To solve this we need to iterate through all the permutations of unknown and
    * bounded values in the pattern. Note that the internal oredering between bounded
    * values remains, so the normalized patterns of (Some("A"), Some("B"), None) are
    * (None, "A", "B"), ("A", None, "B"), ("A", "B", None), but not for example ("B",
    * "A", None), since such a pattern would never be stored in the table.
    */
  def normalizePattern(pattern: Pattern, grouping: ColumnGroups): Vector[Pattern] = {

    // Gather all the ordered permutations of values for each ordering group
    val groupOrderings = grouping.map(group => {
      val values = group.flatMap(pattern(_))
      val order = values.map(_.asInstanceOf[java.lang.Comparable[Any]]).sortWith(_.compareTo(_) < 0)
      val mask = group.map(pattern(_).isDefined)
      mask.permutations.map(permutation => {
        var valueIndex = 0
        permutation.map(present => {
          if (present) {
            val ix = valueIndex
            valueIndex += 1
            Some(order(ix))
          } else {
            None
          }
        }).toArray
      }).toArray
    }).toArray


    // Recursively apply the ordering to obtain the cartesian product of all possible
    // orderings between all different oredering groups
    def applyOrdering(pat: Pattern, groupIndex: Int): Vector[Pattern] = {
      if (groupIndex >= grouping.length) return Vector(pat)

      groupOrderings(groupIndex).foldLeft(Vector[Pattern]())((patterns, permutation) => {
        val newPat = pat.clone()
        for ((index, value) <- grouping(groupIndex).zip(permutation)) {
          newPat(index) = value
        }
        patterns ++ applyOrdering(newPat, groupIndex + 1)
      })
    }
    applyOrdering(pattern, 0)
  }

  /** Wraps an itearator of row indices and translates the results to data rows.
    * Swizzles unordered values to right places within the groups.
    */
  class UnorderedRowDataIterator(private val rowData: ArrayBuffer[Row],
                                 private val indexIterator: Iterator[Table.RowIndex], private val grouping: ColumnGroups,
                                 private val pattern: Pattern) extends Iterator[Row] {
    def hasNext: Boolean = indexIterator.hasNext
    def next(): Row = {
      val row = rowData(indexIterator.next())
      val swizzled = row.clone()
      // O(n^2) but small n (2 to 3 or so)
      for (group <- grouping) {
        val values = group.map(row(_)).toBuffer

        // First, place the pattern matches to their places
        for (g <- group) {
          if (pattern(g).isDefined) {
            val value = pattern(g).get
            values -= value
            swizzled(g) = value
          }
        }

        // Place the other values to the empty slots
        for (g <- group) {
          if (pattern(g).isEmpty) {
            val value = values.last
            values.remove(values.length - 1)
            swizzled(g) = value
          }
        }
      }

      swizzled
    }
  }

}

/** Table which supports unordered groups within the columns.
  *
  * @param numColumns Number of columns in the database.
  * @param grouping Array of column arrays that belong to the same group.
  *                 Columns that belong to the same ordering group don't
  *                 have any internal ordering and behave as sets.
  */
class UnorderedTable(numColumns: Int, val grouping: ColumnGroups) extends Table(numColumns) {

  override def queryRows(pattern: Pattern): Iterator[Table.RowIndex] = {
    val canonicalPats = normalizePattern(pattern, grouping)

    // Optimized early-out for single pattern case
    if (canonicalPats.length == 1) return super.queryRows(canonicalPats.head)

    // Collect all the results into an ordered collection while removing duplicates
    val results = new TreeSet[Table.RowIndex]()
    for (pat <- canonicalPats) {
      results ++= super.queryRows(pat)
    }
    results.toIterator
  }

  override def query(pattern: Pattern): Iterator[Row] = {
    assert(pattern.length == numColumns)
    val indexIterator = queryRows(pattern)
    new UnorderedRowDataIterator(rowData, indexIterator, grouping, pattern)
  }

  /** Insert a new row into the table */
  override def insert(row: Row): Unit = {
    super.insert(normalizeRow(row, grouping))
  }
}
