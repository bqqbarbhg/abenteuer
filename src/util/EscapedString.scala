package util

import scala.collection.immutable.HashMap

object EscapeUtil {
  val controlMap = Map('\b' -> "\\b", '\t' -> "\\t", '\n' -> "\\n", '\f' -> "\\f", '\r' -> "\\r")
  val escapeMap = controlMap ++ Map('\\' -> "\\\\", '"' -> "\\\"")
  val unescapeMap = escapeMap.map({ case (k, v) => v(1) -> k.toString() }).updated('\\', "\\\\")
  val unescapeRegex = raw"""\\(.)""".r

  def escapeCharControl(c: Char): String = {
    controlMap.getOrElse(c, if (c.isControl) f"\\x${c.toInt}%02x" else c.toString)
  }
  def escapeChar(c: Char): String = {
    escapeMap.getOrElse(c, if (c.isControl) f"\\x${c.toInt}%02x" else c.toString)
  }

  implicit class EscapedString(val str: String) {

    /** Escapes non-printable control characters of the string.
      * `Hello\n "world"` -> `Hello\\n "world"`
      */
    def escapeControl: String = s"${str.flatMap(escapeCharControl)}"

    /** Escapes all non string literal friendly characters as well as control characters
      * `Hello\n "world"` -> `Hello\\n \"world\"`
      */
    def escape: String = s"${str.flatMap(escapeChar)}"

    /** Replaces escape sequences with their respective values
      * `Hello\\n \"world\"` -> `Hello\n "world"`
      */
    def unescape: String = unescapeRegex.replaceAllIn(str, m => unescapeMap.getOrElse(m.group(1)(0), m.group(0)))
  }
}

