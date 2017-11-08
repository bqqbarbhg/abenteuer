package util

/** A nicer way to write iterators. Instead of managing two methods
  * `hasNext` and `next` only one is required.
  */
abstract class SimpleIterator[T] extends Iterator[T] {

  /** Return Some(T) if there is a result and None if the iterator is exhausted
    * Guaranteed not to be called again once exhausted (has returned None)!
    */
  def nextOption(): Option[T]

  private var element: Option[T] = None
  private var initialized: Boolean = false

  def hasNext: Boolean = {
    if (!initialized) {
      element = nextOption()
      initialized = true
    }
    element.isDefined
  }

  def next(): T = {
    if (!initialized) {
      element = nextOption()
      initialized = true
    }

    if (element.isEmpty) {
      throw new RuntimeException("Attempting to call next() on an exhausted iterator")
    }

    val result = element.get
    element = nextOption()
    result
  }
}

