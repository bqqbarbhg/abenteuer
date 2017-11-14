package test

import lang.AstPrinter.AstWithPrettyPrint

object ParserTest extends App {

  val source =
    """
      |
      | table try cmd func order { default order 0 }
      | table do  cmd func order { default order 0 }
      |
      | table arg cmd index name
      |
      | entity cmd-get {
      |   command
      |   keyword "get", "take", "pick up"
      |   arg 0 "item"
      | }
      |
      | try cmd-get (Item) {
      |   reach Player Item
      |   can-get Item
      |   ! has Someone Item
      | }
      |
      | do cmd-get (Item) -> {
      |   has Player Item
      | } -1
      |
    """.stripMargin

  val ast = lang.Parser.parse(source, "ParserTest.inline")
  println(ast.prettyPrint())

}

