package game

import ui.TextSpan

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

/**
  * Result of interacting with the game
  * @param spans Styled text content to display to the user
  * @param ephemeral If this is true this content can be replaced with the next result
  * @param overridePrompt Possible replacement for what the user entered
  * @param autoCommand The client _should_ behave like the user input this command right after the last one
  */
case class GameText(spans: Vector[TextSpan], ephemeral: Boolean = false, overridePrompt: Option[String] = None, autoCommand: Option[String] = None)

case class CmdTargetPair(target: vm.Entity, command: vm.Entity)
case class DelayedCommand(options: Vector[CmdTargetPair], previousLine: String, hadKeyword: Boolean)

class GameInstance(val path: String, val module: String = "main", val shared: vm.SharedContext = new vm.SharedContext) {

  private val (context, actions) = Initialization.compileGame(path, module, shared)
  actions.gameInstance = this

  // Tables defined in the code
  private val tabCmdKeyword = context.query[vm.Entity, String]("cmd.keyword") _
  private val tabCmdSelect = context.query[vm.Entity, vm.Rule]("cmd.select") _
  private val tabCmdDiscard = context.query[vm.Entity, vm.Rule]("cmd.discard") _
  private val tabCmdAddKeyword = context.query[vm.Entity, vm.Rule]("cmd.add-keyword") _
  private val tabCmdImplement = context.query[vm.Entity, vm.Entity]("cmd.implement") _
  private val tabCmdDo = context.query[vm.Entity, vm.Rule, Int]("cmd.do") _
  private val tabCmdOverload = context.query[vm.Entity, vm.Entity, vm.Rule, Int]("cmd.overload") _
  private val tabCmdAbbrev = context.query[vm.Entity, String, String]("cmd.abbrev") _
  private val tabCmdDebug = context.query[String, vm.Rule, Int]("cmd.debug") _
  private val tabTick = context.query[Any, vm.Rule, Int]("tick") _
  private val tabName = context.query[vm.Entity, String]("name") _
  private val tabKeyword = context.query[vm.Entity, String]("keyword") _
  private val tabSelectOrder = context.query[vm.Entity, Int]("select-order") _
  private val tabGameTitle = context.query[String]("game.title") _
  private val tabGameWelcome = context.query[String]("game.welcome") _
  private val tabSubgameInit = context.query[String, vm.Rule, Int]("subgame.init") _
  private val tabSubgameFini = context.query[vm.Rule, Int]("subgame.fini") _

  private def fmt(str: String): Vector[TextSpan] = {
    val parts = str.split("\\*\\*")
    var isBold = false
    val spans = new ArrayBuffer[TextSpan](parts.length)
    for (part <- parts) {
      spans += TextSpan(part, if (isBold) ui.TextStyle.Bold else ui.TextStyle.Normal)
      isBold = !isBold
    }
    spans.toVector
  }

  /**
    * Runs the subgame initializers
    * @param context Argument passed from the parent game
    */
  def startSubgame(context: String): ArrayBuffer[String] = {
    actions.wantsToPopSubGame = false
    actions.listenToPrint {
      var inits = tabSubgameInit(Some(""), None, None).toSeq
      if (context != "") inits ++= tabSubgameInit(Some(context), None, None).toSeq
      val sortedInits = inits.sortBy(_._3)
      for (init <- sortedInits) {
        val rule = init._2
        rule.query().foreach(bind => rule.execute(bind))
      }
    }
  }

  /**
    * Runs the subgame finalizers
    */
  def stopSubgame(): Unit = {
    var finis = tabSubgameFini(None, None).toSeq
    val sortedFinis = finis.sortBy(_._2)
    for (fini <- sortedFinis) {
      val rule = fini._1
      rule.query().foreach(bind => rule.execute(bind))
    }
  }

  /**
    * Should the game pop as a subgame
    */
  def wantsToPop = actions.wantsToPopSubGame

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

    // Route to subgame (unless some special queries)
    if (input.trim != "/hello" && input.trim != "/title") {
      actions.activeSubGame match {
        case Some(subGame) =>
          if (subGame.wantsToPop) {
            actions.activeSubGame = None
          } else {
            return subGame.interact(input)
          }
        case None =>
      }
    }

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
          val msg =
            """Note: This is the debug command help, for in-game help use only:
            |  > help
            |
            |  **/hello** - Display startup message
            |  **/help** - Show this help
            |  **/title** - Get window title
            |  **/reload** - Reload the game
            |  **/replay** - Reload the game and replay all the input
            |  **/list** - Dump table contents
            |  **/debug** - Run debug commands (or list if no argument)
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

        case "debug" =>
          parts.lift(1) match {
            case Some(cmd) =>
              val rules = tabCmdDebug(Some(cmd), None, None).toSeq.sortBy(row => row._3).map(_._2).toVector
              val text = actions.listenToPrint {
                executeRuleChain(rules, None)
              }
              GameText(fmt(text.mkString("")))
            case None =>
              val debugs = tabCmdDebug(None, None, None).map(row => s"**${row._1}**").toSet.mkString(", ")
              GameText(fmt(s"Available debug commands: $debugs"))
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

        val result = executeCommandRule(selected.command, Some(selected.target))

        val keyword = if (!command.hadKeyword) {
          tabKeyword(Some(selected.target), None).toStream.headOption.map(row => row._2 + " ").getOrElse("")
        } else {
          ""
        }

        return GameText(result.spans, result.ephemeral, Some(s"${command.previousLine} $keyword ($number)"), result.autoCommand)
      case None =>
        commandUnderSelect = None
    }

    val commandAndKeyword = tabCmdKeyword(None, None).find(kw => line.startsWith(kw._2)).orElse {
      parts.lift(0).flatMap(part => tabCmdAbbrev(None, Some(part), None).map(ab => (ab._1, ab._3)).toStream.headOption)
    }

    if (commandAndKeyword.isEmpty) return GameText(fmt("I don't know how to do that"), true)
    val (matchedCommand, matchedKeyword) = commandAndKeyword.get

    val restOfLine = if (line.startsWith(matchedKeyword)) line.drop(matchedKeyword.length).trim else line.drop(parts(0).length).trim

    val selectRules = tabCmdSelect(Some(matchedCommand), None).map(row => (row._2, matchedCommand)).toStream
    if (!selectRules.isEmpty) {
      // Do selection based command

      // Gather all the implementations of the command
      val alts = tabCmdImplement(None, Some(matchedCommand)).flatMap( { case (alt, _) =>
        tabCmdSelect(Some(alt), None).map(row => (row._2, alt)).toStream
      })
      val impls = selectRules ++ alts

      // Gather all entities accross all implementations that match the selection of the command
      //val options = selectRules.flatMap(_.query()).map(row => (row(0).get.asInstanceOf[vm.Entity], impl))
      val options = impls.flatMap({ case (rule, cmd) =>
          rule.query().map(row => CmdTargetPair(row(0).get.asInstanceOf[vm.Entity], cmd))
      })

      val filteredOptions = if (restOfLine.nonEmpty) {
        // Filter to entities with matching keywords
        options.filter({ case CmdTargetPair(entity, cmd) =>
          val entityKeywords = tabKeyword(Some(entity), None).map(_._2).toSeq
          val addKeywords = tabCmdAddKeyword(Some(cmd), None).flatMap(row => {
            val rule = row._2
            rule.query(db.Pattern(Some(entity), None)).map(_(1).get.asInstanceOf[String])
          }).toSeq
          val keywords = entityKeywords ++ addKeywords

          keywords.exists(_ == restOfLine)
        }).toVector
      } else {
        // Show all of them
        options.toVector
      }

      def getDiscards(cmd: vm.Entity): (vm.Entity, Seq[vm.Rule]) = cmd -> tabCmdDiscard(Some(cmd), None).map(_._2).toSeq
      val discards = filteredOptions.map(_.command).toSet.map(getDiscards).toMap

      val finalOptions = filteredOptions.filterNot({ case CmdTargetPair(entity, cmd) =>
        discards(cmd).exists(rule => rule.query(db.Pattern(Some(entity))).nonEmpty)
      })

      val sortedOptions = finalOptions.map(pair => {
        val order = tabSelectOrder(Some(pair.target), None).toStream.headOption.map(_._2).getOrElse(0)
        (pair, order)
      }).sortBy(_._2).map(_._1).toVector

      if (sortedOptions.size == 0) {
        // No options -> fail
        GameText(fmt(s"Found nothing relevant to ${matchedKeyword}"), true)
      } else if (sortedOptions.size == 1 && restOfLine.nonEmpty) {
        // Single selected option -> execute command
        val CmdTargetPair(entity, cmd) = sortedOptions.head
        executeCommandRule(cmd, Some(entity))
      } else {
        // Multiple or unselected options, show choices
        commandUnderSelect = Some(DelayedCommand(sortedOptions, line, restOfLine.nonEmpty))

        val names = for ((option, index) <- sortedOptions.zipWithIndex) yield {
          val name = tabName(Some(option.target), None).toStream.headOption.map(_._2).getOrElse("(unknown)")
          s"${index + 1}. ${name}"
        }

        val prefix = if (names.length > 1) s"Select by typing 1-${names.length}" else "Select by typing 1"
        GameText(fmt(prefix + "\n" + names.mkString("\n")), true)
      }

    } else {
      // Do non-selection command
      executeCommandRule(matchedCommand, None)
    }

  }

  /** Execute ordered rules with fail: command support, returns if any rule matched */
  private def executeRuleChain(rules: Vector[vm.Rule], entity: Option[vm.Entity]): Boolean = {
    var anyMatch = false

    val pattern = entity.map(Some(_)).toArray[Option[Any]]
    val matches = rules.map(rule => (rule, rule.query(pattern).toVector)).toVector
    for ((rule, patterns) <- matches) {
      actions.onceRequested = false
      for (pattern <- patterns.iterator.takeWhile(_ => !actions.onceRequested)) {
        anyMatch = true
        rule.execute(pattern)
        if (actions.hasFailed) return anyMatch
      }
    }

    anyMatch
  }

  private def executeCommandRule(command: vm.Entity, entity: Option[vm.Entity]): GameText = {
    actions.hasFailed = false
    actions.autoCommand = None

    val dos = tabCmdDo(Some(command), None, None).toVector
    val overloads = entity.map(entity => {
      tabCmdOverload(Some(command), Some(entity), None, None).map(row => (row._1, row._3, row._4)).toVector
    }).getOrElse(Vector[(vm.Entity, vm.Rule, Int)]())
    val tryRules = (dos ++ overloads).sortBy(row => row._3).map(_._2)

    val text = actions.listenToPrint {
      executeRuleChain(tryRules, entity)

      var somethingMatched: Boolean = false
      var matchCounter: Int = 0
      do {
        somethingMatched = false

        matchCounter += 1
        if (matchCounter > 1000) {
          throw new RuntimeException("Tried to apply tick rules for over 1000 iterations, maybe there is a loop!")
        }

        val ticks = tabTick(None, None, None).toStream.groupBy(_._1)
        for ((_, rows) <- ticks) {
          val sorted = rows.sortBy(_._3).map(_._2).toVector
          val matched = executeRuleChain(sorted, None)
          somethingMatched ||= matched
        }
      } while (somethingMatched)
    }

    GameText(fmt(text.mkString("")), autoCommand = actions.autoCommand)
  }

}

class Game {
  val gamePath = "New_Game"

  var instance = new GameInstance(gamePath)
  val inputs = new ArrayBuffer[String]()

  private def errorToText(err: lang.CompileError): GameText = {
    GameText(Vector(TextSpan("Failed to compile game\n", ui.TextStyle.Bold),
      TextSpan(err.getMessage, ui.TextStyle.Normal)), true)
  }

  def interact(input: String): GameText = {
    try {
      input.trim match {
        case "/reload" =>
          try {
            instance = new GameInstance(gamePath)
            inputs.clear()
            instance.interact("/hello")
          } catch {
            case err: lang.CompileError => errorToText(err)
          }
        case "/replay" =>
          try {
            instance = new GameInstance(gamePath)
            val allInputs = Vector("/hello") ++ inputs
            for (input <- allInputs.dropRight(1)) {
              instance.interact(input)
            }
            instance.interact(allInputs.last)
          } catch {
            case err: lang.CompileError => errorToText(err)
          }
        case _ =>
          inputs += input
          instance.interact(input)
      }
    } catch {
      case err: Exception =>
        println(s"Error on interact: ${err.getMessage}")
        err.printStackTrace()
        val spans = Vector(TextSpan("Runtime error:", ui.TextStyle.Bold),
          TextSpan("\n" + err.toString, ui.TextStyle.Normal))
        GameText(spans, true)
    }
  }
}
