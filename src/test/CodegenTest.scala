package test

import scala.collection.immutable.Vector
import lang.AstPrinter.AstWithPrettyPrint
import lang.CompileError

object CodegenTest extends App {

  val source1 =
    """
      |
      | cmd {
      |   table command cmd            { unique cmd }
      |   table try     cmd func order { unique cmd; default order 0 }
      |   table do      cmd func order { unique cmd; default order 0 }
      | }
      |
      | entity Player
      |
      | external print "print"
      |
    """.stripMargin

  val source2 =
    """
      | table has self thing
      | table item self
      | table thing self
      |
      | entity Foo
      | entity Sword { item }
      |
      | has Foo Sword
      |
      | define announce (Item) -> {
      |   print: "You pick up {Item}"
      | }
      |
      | entity get {
      |   cmd.command
      |   cmd.try (Item) {
      |     item Item
      |     ! has *Player Item
      |   } -> {
      |     announce: Item
      |     has *Player Item
      |   }
      | }
      |
    """.stripMargin

  val sources = Map("source1" -> source1, "source2" -> source2)

  def errorLine(loc: lang.SourceLocation): Unit = {
    val src = sources(loc.file)
    val lines = src.split('\n')
    val line = lines(loc.line - 1).replace("\t", "  ")
    var arrow = ""
    for ((c, i) <- line.zipWithIndex) {
      arrow += {
        if (i < loc.column - 1) ' '
        else if (i == loc.column - 1) '^'
        else if (i < loc.column + loc.data.length - 2) '~'
        else if (i == loc.column + loc.data.length - 2) '^'
        else ' '
      }
    }

    println(line.stripLineEnd)
    println(arrow)
  }

  try {

    val ast1 = lang.Parser.parse(source1, "source1")
    val ast2 = lang.Parser.parse(source2, "source2")


    println(ast1.prettyPrint)
    println(ast2.prettyPrint)

    val templateRegex = raw"""\{([^}]*)\}""".r

    def printAction(rule: vm.Rule, binds: db.Pattern, mapping: Vector[Int]): Unit = {
      val args = rule.mapArgs(mapping, binds)
      val str = args(0).get.toString.asInstanceOf[CharSequence]
      val msg = templateRegex.replaceAllIn(str, m => {
        val ix = rule.bindNames.indexOf(m.group(1))
        binds(ix).toString
      })
      println(">>> " + msg)
    }

    {
      val context = new vm.Context()
      context.externals("print") = vm.ExternalAction(printAction)

      val cg = new lang.Codegen(context)
      cg.doCodegen(Vector(ast1, ast2))
      println(context.queryables.mkString("\n"))
    }

    {
      val context = new vm.Context()
      context.externals("print") = vm.ExternalAction(printAction)

      val cg = new lang.Codegen(context)
      cg.doCodegen(Vector(ast2, ast1))

      for (row <- context.queryables("has").query(db.Pattern(None, None))) {
        println(row.mkString(", "))
      }

      val cmd = context.namedEntities("get.1")
      for (row <- context.queryables("cmd.try").query(db.Pattern(Some(cmd), None, None))) {
        println(row.mkString(", "))

        val rule = row(1).asInstanceOf[vm.Rule]
        println(rule.bindNames.mkString(" "))

        println("Query results:")
        for (row2 <- rule.query(context)) {
          println(row2.mkString(" "))

          rule.execute(row2)
        }
      }
    }

  } catch {
    case err: CompileError =>
      println(err.getMessage)
      errorLine(err.location)
      for (loc <- err.auxLoc)
        errorLine(loc)
  }
}

