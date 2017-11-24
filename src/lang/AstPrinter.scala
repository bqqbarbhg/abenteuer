package lang

import util.EscapeUtil.EscapedString

object AstPrinter {

  implicit class AstWithPrettyPrint(val ast: AstNode) {

    def prettyPrint: String = prettyPrint(0)

    def prettyPrint(indent: Int = 0): String = {
      val it = "  " * indent
      ast match {

      case table: AstTable =>
        val block = table.constraints.prettyPrint(indent)
        s"table ${table.name.prettyPrint} ${table.columns.map(_.id).mkString(" ")} $block"

      case entity: AstEntity =>
        val block = entity.statements.prettyPrint(indent)
        s"entity ${entity.name.prettyPrint} $block"

      case define: AstDefine =>
        val value = define.value.prettyPrint
        s"define ${define.name.prettyPrint} $value"

      case external: AstExternal =>
        s"external ${external.name.prettyPrint} ${'"' + external.foreign.value.escape + '"'}"

      case namespace: AstNamespace =>
        val block = namespace.block.prettyPrint(indent)
        s"${namespace.name.prettyPrint()} $block"

      case query: AstFreeQuery =>
        query.queries.map(_.prettyPrint(indent)).mkString("\n")

      case block: AstBlock =>
        if (block.statements.isEmpty) {
          "{ }"
        } else {
          val it2 = it + "  "
          s"{\n${block.statements.map(it2 + _.prettyPrint(indent + 1)).mkString("\n")}\n$it}"
        }

      case block: AstStmtBlock =>
        if (block.statements.isEmpty) {
          "{ }"
        } else {
          val it2 = it + "  "
          s"{\n${block.statements.map(it2 + _.prettyPrint(indent + 1)).mkString("\n")}\n$it}"
        }

      case query: AstQueryStmt =>
        s"${query.operator.prettyPrint()} ${query.values.map(_.prettyPrint(indent)).mkString(" ")}"

      case not: AstNotStmt =>
        "! " + not.stmt.prettyPrint()

      case action: AstActionStmt =>
        s"${action.query.operator.prettyPrint()}: ${action.query.values.map(_.prettyPrint(indent)).mkString(" ")}"

      case t: AstExName => t.path.map(_.id).mkString(".")
      case t: AstExString => '"' + t.token.value.escape + '"'
      case t: AstExNumber => t.token.value.toString
      case t: AstExLambda =>
          s"(${t.arguments.map(_.id).mkString(" ")}) ${t.pre.prettyPrint(indent + 1)} -> ${t.post.prettyPrint(indent + 1)}"
      case t: AstExValueName => "*" + t.name.prettyPrint

      }
    }
  }
}
