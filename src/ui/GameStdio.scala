package ui

import scala.annotation.tailrec

object GameStdio extends App {
  val theGame = new game.Game()

  def style(span: TextSpan): String = span.style match {
    case TextStyle.Normal => span.text
    case TextStyle.Bold => s"*${span.text}*"
  }
  def appendSpans(spans: Seq[TextSpan]) = spans.foreach(span => print(style(span)))

  @tailrec
  def processCommand(command: String, showPrompt: Boolean): Unit = {
    var result = theGame.interact(command)
    if (showPrompt)
      result.overridePrompt.foreach(prompt => println(s"> $prompt"))
    appendSpans(result.spans)

    result.autoCommand match {
      case Some(cmd) => processCommand(cmd, false)
      case None =>
    }
  }

  processCommand("/hello", false)

  while (true) {
    Console.print("\n> ")
    Console.flush()

    val line = scala.io.StdIn.readLine()
    processCommand(line, true)
  }
}
