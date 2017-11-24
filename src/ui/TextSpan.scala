package ui

abstract class TextStyle {
  def bold: Boolean
}

object TextStyle {
  case object Normal extends TextStyle { def bold = false }
  case object Bold extends TextStyle { def bold = true }
}

case class TextSpan(text: String, style: TextStyle)

