package game

import scala.collection.mutable.ArrayBuffer
import scala.util.{Try, Success, Failure}

case class GameText(spans: Vector[ui.TextSpan], ephemeral: Boolean = false, overridePrompt: Option[String] = None)

case class DelayedCommand(command: vm.Entity, options: Vector[vm.Entity], previousLine: String)

class Game {

  val context = Initialization.compileGame()

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
            |  **/list** - Dump table contents
            |
            |""".stripMargin
          GameText(fmt(msg))

        case "list" =>
          parts.lift(1).flatMap(t => context.queryables.get(t)) match {
            case Some(table) =>
              val result = table.query(Array.fill[Option[Any]](table.arity)(None)).map(_.mkString(" ")).mkString("\n")
              GameText(fmt(result), false)
            case None => return GameText(fmt("Expected table name"), true)
          }

        case other => GameText(fmt(s"Unknown command: **${other}**"), true)
      }
    } else {
      runCommand(input)
    }
  }

  val tokenRegex = raw"""[a-z]+""".r

  // Tables defined in the code
  val tabCmdKeyword = context.query[vm.Entity, String]("cmd.keyword") _
  val tabCmdSelect = context.query[vm.Entity, vm.Rule]("cmd.select") _
  val tabCmdDo = context.query[vm.Entity, vm.Rule, Int]("cmd.do") _
  val tabCmdAbbrev = context.query[vm.Entity, String, String]("cmd.abbrev") _
  val tabName = context.query[vm.Entity, String]("name") _
  val tabKeyword = context.query[vm.Entity, String]("keyword") _

  var commandUnderSelect: Option[DelayedCommand] = None

  def runCommand(command: String): GameText = {
    val lower = command.toLowerCase
    val parts = tokenRegex.findAllIn(lower).toVector
    val line = parts.mkString(" ")

    // If there is a selection prompt do it
    Try(lower.trim.toInt).toOption match {
      case Some(number) =>
        val index = number - 1

        val command = commandUnderSelect match {
          case Some(command) => command
          case None => return GameText(fmt("Nothing to select."), true)
        }
        commandUnderSelect = None

        val selected = command.options.lift(index) match {
          case Some(selected) => selected
          case None =>
            return GameText(fmt(s"Invalid selection"), true)
        }

        val result = executeCommandRule(command.command, Some(selected))
        return GameText(result.spans, result.ephemeral, Some(command.previousLine + " " + number))
      case None =>
        commandUnderSelect = None
    }

    val commandAndKeyword = tabCmdKeyword(None, None).find(kw => line.startsWith(kw._2)).orElse {
      parts.lift(0).flatMap(part => tabCmdAbbrev(None, Some(part), None).map(ab => (ab._1, ab._3)).toStream.headOption)
    }

    if (commandAndKeyword.isEmpty) return GameText(fmt("I don't know how to do that"), true)
    val (cmd, matchedKeyword) = commandAndKeyword.get

    val restOfLine = line.drop(matchedKeyword.length).trim

    val selectRules = tabCmdSelect(Some(cmd), None).map(_._2).toStream
    if (!selectRules.isEmpty) {
      // Do selection based command

      // Gather all entities that match the selection of the command
      val options = selectRules.flatMap(_.query()).map(_(0).get.asInstanceOf[vm.Entity]).toSet

      val filteredOptions = if (restOfLine.nonEmpty) {
        // Filter to entities with matching keywords
        options.filter(e => tabKeyword(Some(e), None).exists(row => row._2 == restOfLine)).toVector
      } else {
        // Show all of them
        options.toVector
      }

      if (filteredOptions.size == 0) {
        // No options -> fail
        GameText(fmt(s"Found nothing relevant to ${matchedKeyword}"), true)
      } else if (filteredOptions.size == 1 && restOfLine.nonEmpty) {
        // Single selected option -> execute command
        executeCommandRule(cmd, Some(filteredOptions.head))
      } else {
        // Multiple or unselected options, show choices
        commandUnderSelect = Some(DelayedCommand(cmd, filteredOptions, line))

        val names = for ((option, index) <- filteredOptions.zipWithIndex) yield {
          val name = tabName(Some(option), None).toStream.headOption.map(_._2).getOrElse("(unknown)")
          s"${index + 1}. ${name}"
        }

        val prefix = if (names.length > 1) s"Select by typing 1-${names.length}" else "Select by typing 1"
        GameText(fmt(prefix + "\n" + names.mkString("\n")), true)
      }

    } else {
      // Do non-selection command
      executeCommandRule(cmd, None)
    }

  }

  def executeCommandRule(command: vm.Entity, entity: Option[vm.Entity]): GameText = {
    val tryRules = tabCmdDo(Some(command), None, None).toVector.sortBy(row => row._3).map(_._2)
    val text = LangActions.listenToPrint {
      for (rule <- tryRules) {
        for (pattern <- rule.query(entity.map(Some(_)).toArray)) {
          rule.execute(pattern)
        }
      }
    }
    GameText(fmt(text.mkString("\n")))
  }

}
