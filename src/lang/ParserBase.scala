package lang

/**
  * A base structure for writing parsers with.
 *
  * @param scanner A source for the tokens.
  */
class ParserBase(val scanner: Scanner) {

  /**
    * The current token.
    */
  var token: Token = scanner.scan()

  /**
    * Accept the current token if the partial function is applicable to the it.
    *
    * For example, `accept { case TokenNumber(num) => num * 2 }` would result in for next token:
    * `TokenNumber(100) => Some(200)`
    * `TokenIdentifier(asd) => None`
    *
    * @param fn Partial function that is applied to the current token if possible.
    * @tparam T The return type of the function, doesn't need to be explicitly specified.
    * @return The result of `fn` wrapped in `Some` or `None` if the function is not applicable.
    */
  def accept[T](fn: PartialFunction[Token, T]): Option[T] = {
    val tok = token
    if (fn.isDefinedAt(tok)) {
      token = scanner.scan()
      Some(fn(tok))
    } else {
      None
    }
  }

  /**
    * Like `accept`, but accepts tokens as long as `fn` is applicable to the current token.
    * @param fn Partial function to repeadetly apply to the token stream.
    * @tparam T The return type of the function, doesn't need to be explicitly specified.
    * @return An iterator that yields results from `fn` being applied to matching tokens.
    */
  def acceptMany[T](fn: PartialFunction[Token, T]): Iterator[T] = {
    Iterator.continually({ accept(fn) }).takeWhile(_.isDefined).map(_.get)
  }

  /**
    * Like `accept`, but throws a `CompileError` if `fn` is not applicable.
    * @param expected Used for the potential error message, what did the parser expect here?
    * @param fn Partial function that _needs_ to be applicable to the next token (if the source is well formed)
    * @tparam T The return type of the function, doesn't need to be explicitly specified.
    * @return The result of `fn`, an exception is thrown if not applicable.
    */
  def require[T](expected: String)(fn: PartialFunction[Token, T]): T = {
    accept(fn) match {
      case Some(t) => t
      case None => expectedHere(expected)
    }
  }
  /**
    * Yield values until an end condition is reached.
    * @param expected Compiler error message hint if the end is never reached.
    * @param end Partial function that is defined and returns true if the token is considered and end.
    * @param value Evaluated for every token until `end` is found.
    * @tparam T Type of `value`
    * @return Iterator that yields values from `value`.
    */
  def untilAccept[T](expected: String, end: PartialFunction[Token, Boolean])(value: => T): Iterator[T] = {
    Iterator.continually { () }.takeWhile(_ => token match {
        case token if end.isDefinedAt(token) => if (end(token)) {
          this.token = scanner.scan()
          false
        } else {
          true
        }
        case end: TokenEnd => throw new CompileError(end, s"File ended before closing $expected")
        case _ => true
    }).map(_ => value)
  }

  /**
    * Formats an 'unexpected token' error message at the current token.
    * @param expected What was expected instead?
    * @return Never returns.
    */
  def expectedHere(expected: String): Nothing = {
    errorHere(s"Unexpected token ${token.toString}, expected $expected")
  }

  /**
    * Throws an error at the current token.
    * @param message Description of the error
    * @return Never returns.
    */
  def errorHere(message: String): Nothing = {
    throw new CompileError(token, message)
  }

}

