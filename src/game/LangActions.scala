package game

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.Map
import scala.collection.mutable

/** Contains actions that can be used from the language */
class LangActions(val context: vm.Context) {

  val tabValueMissing = context.query[String, String]("value-missing") _

  var printTarget: Option[ArrayBuffer[String]] = None
  val templateRegex = raw"""\{([^}]*)\}""".r

  var hasFailed: Boolean = false
  var onceRequested: Boolean = false

  var subGames: Map[String, GameInstance] = Map[String, GameInstance]()
  var gameInstance: GameInstance = null // Sorry :(

  var activeSubGame: Option[GameInstance] = None
  var wantsToPopSubGame: Boolean = false

  def listenToPrint(block: => Unit): ArrayBuffer[String] = {
    val buffer = new ArrayBuffer[String]()
    printTarget = Some(buffer)
    block
    printTarget = None
    buffer
  }

  def print(printNewline: Boolean)(rule: vm.Rule, binds: db.Pattern, mapping: Vector[Int]): Unit = {
    val args = rule.mapArgs(mapping, binds)
    printTarget match {
      case Some(buffer) =>
        val template = args(0).get.toString
        val msg = templateRegex.replaceAllIn(template, m => {
          val replace = m.group(1).split(':') match {
            case Array(bindName) =>
              val ix = rule.bindNames.indexOf(bindName)
              binds(ix).get.toString
            case Array(table, bindName) =>
              val ix = rule.bindNames.indexOf(bindName)
              val bind = binds(ix).get
              val pattern = db.Pattern(Some(bind), None)
              val maybeText = context.query(table, pattern).toStream.headOption.flatMap(row => row.lift(1)).flatMap(a => Option(a.asInstanceOf[String]))
              val text = maybeText.orElse(tabValueMissing(Some(table), None).toStream.headOption.map(_._2)).getOrElse("(???)")
              text
            case _ =>
              "(INVALID SYNTAX)"
          }
          replace.replace("\\", "\\\\")
        })
        val finalMsg = if (printNewline) msg + "\n" else msg
        buffer += finalMsg

      case None =>
    }
  }

  def fail(rule: vm.Rule, binds: db.Pattern, mapping: Vector[Int]): Unit = {
    hasFailed = true
  }

  def once(rule: vm.Rule, binds: db.Pattern, mapping: Vector[Int]): Unit = {
    onceRequested = true
  }

  def subgamePush(rule: vm.Rule, binds: db.Pattern, mapping: Vector[Int]): Unit = {
    val args = rule.mapArgs(mapping, binds)
    val game = subGames(args(0).get.asInstanceOf[String])
    val prints = game.startSubgame(args(1).getOrElse("").asInstanceOf[String])
    printTarget.foreach(_.appendAll(prints))
    activeSubGame = Some(game)
  }

  def subgamePop(rule: vm.Rule, binds: db.Pattern, mapping: Vector[Int]): Unit = {
    gameInstance.stopSubgame()
    wantsToPopSubGame = true
  }

}
