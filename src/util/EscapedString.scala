package util

import scala.collection.immutable.HashMap

object EscapeUtil {
  val escapeMap = Map('\b' -> "\\b", '\t' -> "\\t", '\n' -> "\\n", '\f' -> "\\f",
    '\r' -> "\\r")

  def escapeChar(c: Char): String = {
    escapeMap.getOrElse(c, if (c.isControl) f"\\x${c.toInt}%02x" else c.toString)
  }

  implicit class EscapedString(val str: String) {
    def escape: String = s"${str.flatMap(escapeChar)}"
  }
}

