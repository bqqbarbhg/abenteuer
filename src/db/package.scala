package object db {

  /** An abstract tuple of data.
    * Note: Even though it's a mutable array it should be treated as immutable!
    *       There is no structure in Scala that would be:
    *       - Lightweight (not Vector)
    *       - Varying size (not Tuple)
    *       - Immutable (not Array)
    *       There could be made an argument that Vector would be better as it
    *       is more pure, but since this data structure is central to many of
    *       the operations of the program it should be lightweight.
    **/
  type Row = Array[Any]

  /** An abstract tuple of potentially bound values for query.
    * Note: Even though it's a mutable array it should be treated as immutable!
    *       See `Row` for further explanation.
    */
  type Pattern = Array[Option[Any]]

  /** Simple Row constructor */
  object Row {
    def apply(args: Any*): Row = args.toArray
  }

  /** Simple Pattern constructor */
  object Pattern {
    def apply(args: Option[Any]*): Pattern = args.toArray
  }

  trait Queryable {
    def query(pattern: Pattern): Iterator[Row]
  }
}
