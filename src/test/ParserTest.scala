package test

import lang.AstPrinter.AstWithPrettyPrint

object ParserTest extends App {

  val source =
    """
      |
      | cmd {
      |   table try cmd func order { default order 0 }
      |   table do  cmd func order { default order 0 }
      | }
      |
      | table arg cmd index name
      |
      | entity cmd-get {
      |   cmd.command
      |   cmd.keyword "get", "take", "pick up"
      |   cmd.arg 0 "item"
      |   cmd.do (foo)
      | }
      |
      | cmd.try cmd-get (Item) {
      |   reach Player Item
      |   can-get Item
      |   ! has Someone Item
      | }
      |
      | cmd.do cmd-get (Item) -> {
      |   has Player Item
      | } -1
      |
    """.stripMargin

  val ast = lang.Parser.parse(source, "ParserTest.inline")
  println(ast.prettyPrint())

}

