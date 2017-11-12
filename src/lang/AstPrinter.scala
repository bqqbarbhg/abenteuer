package lang

object AstPrinter {

  implicit class AstWithPrettyPrint(val ast: AstNode) {

    def prettyPrint(indent: Int = 0): String = {
      val it = "  " * indent
      ast match {
      case table: AstTable =>
        val block = table.constraints.prettyPrint(indent)
        s"${it}table ${table.name.id} ${table.columns.map(_.id).mkString(" ")} $block"

      case block: AstBlock =>
        if (block.statements.isEmpty) {
          "{ }"
        } else {
          s"{\n${block.statements.map(_.prettyPrint(indent + 1)).mkString("\n")}\n$it}"
        }

      case query: AstQueryStmt =>
        "  " * indent + s"${query.operator.id} ${query.values.map(_.id).mkString(" ")}"
      }
    }
  }
}
