package util

/** A simple iterator that yields single value once */
class SingleValueIterator[T](val t: T) extends Iterator[T] {
  var exhausted = false
  def hasNext(): Boolean = !exhausted
  def next(): T = {
    exhausted = true
    t
  }
}

