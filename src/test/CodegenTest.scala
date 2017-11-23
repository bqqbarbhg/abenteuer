package test

import scala.collection.immutable.Vector
import lang.AstPrinter.AstWithPrettyPrint

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
    """.stripMargin

  val source2 =
    """
      |
      | entity get {
      |   cmd.command
      |   cmd.try (Item) {
      |     item Item
      |   } -> {
      |     thing Item
      |   }
      | }
      |
    """.stripMargin

  val ast1 = lang.Parser.parse(source1, "source1")
  val ast2 = lang.Parser.parse(source2, "source2")

  {
    val context = new vm.Context()
    val cg = new lang.Codegen(context)
    cg.doCodegen(Vector(ast1, ast2))
    println(context.queryables.mkString("\n"))
  }

  {
    val context = new vm.Context()
    val cg = new lang.Codegen(context)
    cg.doCodegen(Vector(ast2, ast1))
  }

}

