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
      | table player self
      | table keyword self keyword
      |
      | entity Player {
      |   keyword "player", "self", "me"
      | }
      |
      | Entrance {
      |   entity Room
      |
      |   entity Table {
      |     in Room
      |   }
      | }
      |
    """.stripMargin

  val ast = lang.Parser.parse(source, "ParserTest.inline")
  println(ast.prettyPrint())

}

