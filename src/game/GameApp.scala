package game

import scala.util.{Failure, Success, Try}
import scala.collection.immutable

import util.EscapeUtil.EscapedString

object GameApp extends App {

  val context = Initialization.compileGame()

  val tabCmdKeyword = context.query[vm.Entity, String]("cmd.keyword") _
  val tabTry = context.query[vm.Entity, vm.Rule, Int]("cmd.try") _
  val tabSelect = context.query[vm.Entity, vm.Rule]("cmd.select") _
  val tabName = context.query[vm.Entity, String]("name") _
  val tabKeyword = context.query[vm.Entity, String]("keyword") _

  val keywords = tabCmdKeyword(None, None).toVector

  var pendingCommandAndEntities: Option[(vm.Entity, Vector[vm.Entity])] = None

  while (true) {
    Console.print("> ")
    Console.flush()
    val line = scala.io.StdIn.readLine().toLowerCase

    for {
      (cmd, options) <- pendingCommandAndEntities
      choice <- Try(line.toInt).toOption
      selected <- options.lift(choice - 1)
    } {
      val rules = tabTry(Some(cmd), None, None)
      val printed = LangActions.listenToPrint {
        for (rule <- rules.map(_._2)) {
          val pattern = db.Pattern(Some(selected))
          rule.query(pattern).foreach(rule.execute)
        }
      }
      println(printed.mkString("\n"))
    }
    pendingCommandAndEntities = None

    keywords.find(kw => line.startsWith(kw._2)) match {
      case None => println("Unknown!")
      case Some((cmd, kw)) =>
        val restOfLine = line.drop(kw.length)
        val keywords = restOfLine.trim.split("\\s+").toSet.filter(_.nonEmpty)

        val select = tabSelect(Some(cmd), None).toVector
        if (select.nonEmpty) {
          var options = select.head._2.query().map(row => row(0).get.asInstanceOf[vm.Entity]).toVector

          if (keywords.nonEmpty) {
            options = options.filter(e => tabKeyword(Some(e), None).exists(kw => keywords.contains(kw._2)))
          }

          if (options.length > 1) {
            println("Multiple options:")

            for ((entity, ix) <- options.zipWithIndex) {
              val name = tabName(Some(entity), None).next()._2
              println(s"  ${ix + 1}: $name")
            }

            pendingCommandAndEntities = Some((cmd, options))
          } else if (options.length == 1) {
            val rules = tabTry(Some(cmd), None, None)
            val printed = LangActions.listenToPrint {
              for (rule <- rules.map(_._2)) {
                val pattern = db.Pattern(Some(options(0)))
                rule.query(pattern).foreach(rule.execute)
              }
            }
            println(printed.mkString("\n"))
          } else {
            println("Could not do thing!")
          }
        } else {
          val rules = tabTry(Some(cmd), None, None)
          val printed = LangActions.listenToPrint {
            for (rule <- rules.map(_._2)) {
              rule.query().foreach(rule.execute)
            }
          }
          println(printed.mkString("\n"))
        }
    }
  }
}
