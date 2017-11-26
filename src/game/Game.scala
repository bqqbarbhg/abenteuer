package game

import scala.collection.mutable.ArrayBuffer
import scala.util.{Try, Success, Failure}

case class GameText(spans: Vector[ui.TextSpan], ephemeral: Boolean = false, overridePrompt: Option[String] = None)

case class DelayedCommand(command: vm.Entity, options: Vector[vm.Entity], previousLine: String)

class Game {

  private val context = Initialization.compileGame()

  // Tables defined in the code
  private val tabCmdKeyword = context.query[vm.Entity, String]("cmd.keyword") _
  private val tabCmdSelect = context.query[vm.Entity, vm.Rule]("cmd.select") _
  private val tabCmdDo = context.query[vm.Entity, vm.Rule, Int]("cmd.do") _
  private val tabCmdAbbrev = context.query[vm.Entity, String, String]("cmd.abbrev") _
  private val tabName = context.query[vm.Entity, String]("name") _
  private val tabKeyword = context.query[vm.Entity, String]("keyword") _
  private val tabGameTitle = context.query[String]("game.title") _
  private val tabGameWelcome = context.query[String]("game.welcome") _

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

  /**
    * This is the only public API of the game engine!
    *
    * Passes some input to the game and returns the response from the game.
    *
    * All specialized API functions are done using '/commands', for example to
    * retrieve the title of the game use '/title'.
    *
    * @param input Input for the game or engine using '/commands'
    * @return Text to print to the user.
    */
  def interact(input: String): GameText = {

    if (input.startsWith("/")) {
      val parts = input.drop(1).split(' ')
      if (parts.isEmpty) return GameText(fmt("Expected a command"), true)

      parts(0) match {
        case "title" =>
          val msg = tabGameTitle(None).toStream.headOption.getOrElse("(no title defined)")
          GameText(fmt(msg))

        case "hello" =>
          val msg = tabGameWelcome(None).toStream.headOption.getOrElse("(no hello message defined)")
          GameText(fmt(msg))

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

  private val tokenRegex = raw"""[a-z]+""".r
  private var commandUnderSelect: Option[DelayedCommand] = None

  private def runCommand(command: String): GameText = {
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

    val restOfLine = if (line.startsWith(matchedKeyword)) line.drop(matchedKeyword.length).trim else line.drop(parts(0).length).trim

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

  private def executeRuleChain(rules: Vector[vm.Rule], entity: Option[vm.Entity]): Unit = {
    for (rule <- rules) {
      for (pattern <- rule.query(entity.map(Some(_)).toArray).toVector) {
        rule.execute(pattern)
        if (LangActions.hasFailed) return
      }
    }
  }

  private def executeCommandRule(command: vm.Entity, entity: Option[vm.Entity]): GameText = {
    LangActions.hasFailed = false
    val tryRules = tabCmdDo(Some(command), None, None).toVector.sortBy(row => row._3).map(_._2)
    val text = LangActions.listenToPrint {
      executeRuleChain(tryRules, entity)
    }
    GameText(fmt(text.mkString("\n")))
  }

}
