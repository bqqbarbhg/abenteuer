package util

/** A simple iterator that never yields anything */
class NoneIterator[T] extends Iterator[T] {
  def hasNext(): Boolean = false
  def next(): T = { throw new RuntimeException("Calling next() on an always-empty NoneIterator") }
}
