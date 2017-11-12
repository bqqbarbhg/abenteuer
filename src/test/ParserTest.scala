package test

import lang.AstPrinter.AstWithPrettyPrint

object ParserTest extends App {

  val source =
    """
      | # Table of who is holding what
      | table has self item {
      |   unique item, thing
      |   unordered self item
      | }
      |
      | table simple self
      |
    """.stripMargin

  val ast = lang.Parser.parse(source, "ParserTest.inline")
  println(ast.prettyPrint())

}

