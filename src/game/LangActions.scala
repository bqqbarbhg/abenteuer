package game

import scala.collection.mutable.ArrayBuffer

/** Contains actions that can be used from the language */
class LangActions(val context: vm.Context) {

  val tabValueMissing = context.query[String, String]("value-missing") _

  var printTarget: Option[ArrayBuffer[String]] = None
  val templateRegex = raw"""\{([^}]*)\}""".r

  var hasFailed: Boolean = false

  def listenToPrint(block: => Unit): ArrayBuffer[String] = {
    val buffer = new ArrayBuffer[String]()
    printTarget = Some(buffer)
    block
    printTarget = None
    buffer
  }

  def print(rule: vm.Rule, binds: db.Pattern, mapping: Vector[Int]): Unit = {
    val args = rule.mapArgs(mapping, binds)
    printTarget match {
      case Some(buffer) =>
        val template = args(0).get.toString
        val msg = templateRegex.replaceAllIn(template, m => {
          val replace = m.group(1).split(':') match {
            case Array(bindName) =>
              val ix = rule.bindNames.indexOf(bindName)
              binds(ix).get.toString
            case Array(table, bindName) =>
              val ix = rule.bindNames.indexOf(bindName)
              val bind = binds(ix).get
              val pattern = db.Pattern(Some(bind), None)
              val maybeText = context.query(table, pattern).toStream.headOption.flatMap(row => row.lift(1)).flatMap(a => Option(a.asInstanceOf[String]))
              val text = maybeText.orElse(tabValueMissing(Some(table), None).toStream.headOption.map(_._2)).getOrElse("(???)")
              text
            case _ =>
              "(INVALID SYNTAX)"
          }
          replace.replace("\\", "\\\\")
        })
        buffer += msg

      case None =>
    }
  }

  def fail(rule: vm.Rule, binds: db.Pattern, mapping: Vector[Int]): Unit = {
    hasFailed = true
  }

}
