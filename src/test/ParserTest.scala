package test

import lang.AstPrinter.AstWithPrettyPrint

object ParserTest extends App {

  val source =
    """
      | # Table of who is holding what
      | table has self item
      |
    """.stripMargin

  val ast = lang.Parser.parse(source, "ParserTest.inline")
  println(ast)
  println(ast.prettyPrint())

}

