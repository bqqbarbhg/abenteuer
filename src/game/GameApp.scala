package game

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import scala.util.{Failure, Success, Try}
import scala.collection.immutable

import util.EscapeUtil.EscapedString

object GameApp extends App {

  /** Due to the differences of IDE default working directories the data/
    * folder may be located in another place. This searches for it.
    */
  def findDataFolder(): Option[File] = {
    for (back <- 0 until 10) {
      val path = "../" * back + "script/"
      val file = new File(path + "engine.abt")
      if (file.exists) {
        val root = new File(path)
        assert(root.isDirectory)
        return Some(root)
      }
    }
    None
  }

  /** Walks a directory and lists all files even in nested folders */
  def listFilesRecursive(root: File): Seq[File] = {
    root.listFiles.flatMap(file => {
      if (file.isDirectory) listFilesRecursive(file)
      else Some(file)
    })
  }

  // -- Find the script root directory
  val scriptRoot: File = findDataFolder().getOrElse {
    throw new RuntimeException("Could not find script root folder!")
  }

  // -- Load the sources from files
  val sources = listFilesRecursive(scriptRoot).flatMap(file => {
    if (file.getName.endsWith(".abt") && file.canRead) {
      val bytes = Files.readAllBytes(file.toPath())
      val source = new String(bytes, StandardCharsets.UTF_8)
      Some((file.getName, source))
    } else {
      None
    }
  }).toMap


  /** Prints a `SourceLocation` underlined on the standard output */
  def printErrorLine(location: lang.SourceLocation) = {
    sources.get(location.file).foreach(source => println(lang.Scanner.errorLine(source, location)))
  }

  // -- Setup the builtins for the compiler
  val context = new vm.Context
  context.externals("print") = vm.ExternalAction(LangActions.print)
  context.externals("fail") = vm.ExternalAction(LangActions.fail)

  // -- Compile the script sources
  try {
    val asts = sources.map { case (key, value) => lang.Parser.parse(value, key) }.toSeq
    lang.Codegen.codegen(context, asts)
  } catch {
    case err: lang.CompileError =>
      println("Compile error: " + err.getMessage)
      printErrorLine(err.location)
      for (loc <- err.auxLoc) printErrorLine(loc)

      // Terminate game and print the stack for debugging the compiler itself!
      throw err
  }

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
