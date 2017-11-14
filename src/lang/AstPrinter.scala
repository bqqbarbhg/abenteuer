package lang

import util.EscapeUtil.EscapedString

object AstPrinter {

  implicit class AstWithPrettyPrint(val ast: AstNode) {

    def prettyPrint(indent: Int = 0): String = {
      val it = "  " * indent
      ast match {

      case table: AstTable =>
        val block = table.constraints.prettyPrint(indent)
        s"${it}table ${table.name.id} ${table.columns.map(_.id).mkString(" ")} $block"

      case entity: AstEntity =>
        val block = entity.statements.prettyPrint(indent)
        s"${it}entity ${entity.name.id} $block"

      case namespace: AstNamespace =>
        val block = namespace.block.prettyPrint(indent)
        s"${it}${namespace.name.id} $block"

      case query: AstFreeQuery =>
        query.queries.map(_.prettyPrint(indent)).mkString("\n")

      case block: AstBlock =>
        if (block.statements.isEmpty) {
          "{ }"
        } else {
          s"{\n${block.statements.map(_.prettyPrint(indent + 1)).mkString("\n")}\n$it}"
        }

      case query: AstQueryStmt =>
        "  " * indent + s"${query.operator.id} ${query.values.map(_.prettyPrint()).mkString(" ")}"

      case t: AstExId => t.token.id
      case t: AstExString => '"' + t.token.value.escape + '"'
      case t: AstExNumber => t.token.value.toString

      }
    }
  }
}
