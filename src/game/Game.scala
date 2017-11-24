package game

import scala.collection.mutable.ArrayBuffer

case class GameText(spans: Vector[ui.TextSpan], ephemeral: Boolean = false)

class Game {

  private def fmt(str: String): Vector[ui.TextSpan] = {
    val parts = str.split("\\*\\*")
    var isBold = false
    val spans = new ArrayBuffer[ui.TextSpan](parts.length)
    for (part <- parts) {
      spans += ui.TextSpan(part, if (isBold) ui.TextStyle.Bold else ui.TextStyle.Normal)
      isBold = !isBold
    }
    spans.toVector
  }

  def interact(input: String): GameText = {

    if (input.startsWith("/")) {
      val parts = input.drop(1).split(' ')
      if (parts.isEmpty) return GameText(fmt("Expected a command"), true)

      parts(0) match {
        case "hello" =>
          val msg =
            """
              |Welcome to **Abenteuer** - text adventure.
              |
              |At any point type **help** to get a list of available commands.
              |
              |(Or /help for developer commands)
              |
              |""".stripMargin
          GameText(fmt(msg))

        case "title" => GameText(fmt("Abenteuer"))

        case "help" =>
          val msg = """Note: This is the debug command help, for in-game help use only:
            |  > help
            |
            |  **/hello** - Display startup message
            |  **/help** - Show this help
            |  **/title** - Get window title
            |  **/query** - Query a value from a table
            |
            |""".stripMargin
          GameText(fmt(msg))

        case other => GameText(fmt(s"Unknown command: **${other}**"), true)
      }
    } else {
      GameText(fmt("Temporary response"), true)
    }
  }

}
