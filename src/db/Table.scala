package db

import scala.collection.mutable.HashMap
import scala.collection.mutable.TreeSet
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ArrayStack
import util.SimpleIterator
import util.NoneIterator
import db.Table._

object Table {

  type RowIndex = Int

  /**
    * A sorted set of rows, used for indexing the database.
    * Operations should be scalable.
    */
  class RowSet {
    private val set = new TreeSet[RowIndex]()

    /** Insert a new row into the set. The value _must_ not exist before in the set.
      * Complexity: O(log n)
      */
    def insert(row: RowIndex): Unit = {
      if (!set.add(row)) {
        throw new RuntimeException("Tried to re-add key to RowSet!")
      }
    }

    /** Remove a key from the set. The value _must_ exist in the set.
      * Complexity: O(log n)
      */
    def remove(row: RowIndex): Unit = {
      if (!set.remove(row)) {
        throw new RuntimeException("Tried to remove non-existing key from RowSet!")
      }
    }

    /**
      * Find first row that is >= than `bound`.
      * Complexity: O(log n)
      */
    def lowerBound(bound: RowIndex): Option[RowIndex] = {
      set.iteratorFrom(bound).toStream.headOption
    }

    /** Returns an iterator over all the rows in the set.
      */
    def toIterator: Iterator[RowIndex] = {
      set.toIterator
    }

    /** Is the set empty? */
    def isEmpty: Boolean = set.isEmpty
  }

  /** Index in the database sense, data structure for accelerating queries. Every
    * column in the database has one of these.
    * Map from column value -> set of rows where the column has the value.
    */
  type DbIndex = HashMap[Any, RowSet]

  /** Iterator that tracks multiple row index sets yielding rows that are shared
    * between all sets. This is the heart of the complicated query algorithm
    * being able to answer queries with multiple constraints.
    *
    * Performance is O(m log n) where m is the number of rows in the _smallest_
    * row set in the query and n is the size of the _largest_.
    */
  class MultiIndexIterator(private val rowSets: Array[RowSet]) extends SimpleIterator[RowIndex] {
    private var currentRow: RowIndex = -1

    def nextOption(): Option[RowIndex] = {
      // The iterator is always sitting on a valid row index, or in the initial
      // case row -1. Advance the row by one and sync the indices to find the
      // next row. Start by skipping the current row.
      currentRow += 1

      // This loop is guaranteed to be exited in finite time since every iteration
      // either advances tha `currentRow` until a match is found or there is a
      // column with no rows >= `currentRow`.
      while (true) {
        var numInSync = 0
        for (rowSet <- rowSets) {
          val someCurrentRow = Some(currentRow)
          rowSet.lowerBound(currentRow) match {
            // Column has been exhausted => there can be no more matches
            case None => return None
            // `currentRow` present in column => potential match
            case `someCurrentRow` => numInSync += 1
            // Any other row => no matches below `row`, keep looking
            case Some(row) => currentRow = row
          }
        }

        // All columns in sync, `currentRow` is on matching index!
        if (numInSync == rowSets.length) {
          return Some(currentRow)
        }
      }

      // Scala compiler doens't like the final statement being an infinite loop
      // I guess it would prefer a tail-recursive form but it kinda feels odd here
      None
    }
  }

  /** Wraps an itearator of row indices and translates the results to data rows */
  class RowDataIterator(private val rowData: ArrayBuffer[Row], private val indexIterator: Iterator[RowIndex])
    extends Iterator[Row] {
    def hasNext: Boolean = indexIterator.hasNext
    def next(): Row = rowData(indexIterator.next())
  }
}

/**
  * Mutable database with fixed amount of columns.
  * Optimized for querying with exact values, not for ranges.
  * @param numColumns Number of columns in the database.
  */
class Table(val numColumns: Int) extends Queryable {

  def arity: Int = numColumns

  /** Indexing store for every column */
  protected val dbIndices: Array[DbIndex] = Array.fill(numColumns) { new DbIndex() }

  /** Per-column data storage */
  protected val rowData: ArrayBuffer[Row] = new ArrayBuffer[Row]()

  /** Rows which are 'alive', required so gaps don't hurt iterating performance */
  protected val definedRows: TreeSet[RowIndex] = new TreeSet[RowIndex]()

  /** Contains deleted row indices so they can be re-used */
  protected val freeList: ArrayStack[RowIndex] = new ArrayStack[RowIndex]()

  /** Query the database for rows matching `pattern`.
    * Returns matched row indices, use query() for returning data.
    */
  def queryRows(pattern: Pattern): Iterator[RowIndex] = {
    // Zip matching pattern and database index columns and find row sets for
    // the defined values
    val maybeRowSets = for {
      (maybePat, dbIndex) <- pattern.zip(dbIndices)
      pat <- maybePat
    } yield dbIndex.lift(pat)

    // If there is a value in the pattern that no row set was found for there
    // doesn't exist a matching row in the table.
    if (maybeRowSets.exists(_.isEmpty)) {
      return new NoneIterator[RowIndex]()
    }

    // Choose a fitting iteration strategy for the number of constraints
    val rowSets = maybeRowSets.flatten
    rowSets.length match {
      // Zero constraints => iterate through the whole table
      case 0 => definedRows.toIterator
      // Single constraint => iterate over the results in the value index
      case 1 => rowSets(0).toIterator
      // Multiple constraints => iterate over all the row sets
      case _ => new MultiIndexIterator(rowSets)
    }
  }

  /** Query the database for rows matching `pattern`.
    * Returns matched row indices, use queryRows() for returning indices.
    */
  def query(pattern: Pattern): Iterator[Row] = {
    assert(pattern.length == numColumns)
    val indexIterator = queryRows(pattern)
    new RowDataIterator(rowData, indexIterator)
  }

  /** Insert a new row into the table */
  def insert(row: Row): Unit = {
    assert(row.length == numColumns)

    // Try to re-use a removed row index or append to the back
    val rowIx = if (freeList.nonEmpty) {
      freeList.pop()
    } else {
      rowData += null
      rowData.length - 1
    }

    // Add the data to all the indices
    for ((key, dbIndex) <- row.zip(dbIndices)) {
      // Create a new row set if it doesn't not exist for this key
      val rowSet = dbIndex.lift(key).getOrElse {
        var newSet = new RowSet()
        dbIndex(key) = newSet
        newSet
      }
      rowSet.insert(rowIx)
    }

    // Insert row presence and data
    definedRows += rowIx
    rowData(rowIx) = row
  }

  /** Remove a row at `rowIndex` */
  def removeRow(rowIndex: RowIndex): Unit = {
    // Remove the row from all the column indices, removing the row sets
    // themselves if they become empty.
    val row = rowData(rowIndex)
    for ((key, dbIndex) <- row.zip(dbIndices)) {
      val rowSet = dbIndex(key)
      rowSet.remove(rowIndex)
      if (rowSet.isEmpty) {
        dbIndex.remove(key)
      }
    }

    // Remove the row presence and data lists and add to free list
    definedRows -= rowIndex
    rowData(rowIndex) = null
    freeList += rowIndex
  }

  /** Remove all rows matching a pattern */
  def remove(pattern: Pattern): Unit = {
    assert(pattern.length == numColumns)
    // Note: toArray is required to finish iteration before removing rows
    for (rowIx <- queryRows(pattern).toArray) {
      removeRow(rowIx)
    }
  }
}

