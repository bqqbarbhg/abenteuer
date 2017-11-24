package game

import scala.collection.mutable.ArrayBuffer

/** Contains actions that can be used from the language */
object LangActions {

  var printTarget: Option[ArrayBuffer[String]] = None
  val templateRegex = raw"""\{([^}]*)\}""".r

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
          val ix = rule.bindNames.indexOf(m.group(1))
          binds(ix).get.toString
        })
        buffer += msg

      case None =>
    }
  }

  def fail(rule: vm.Rule, binds: db.Pattern, mapping: Vector[Int]): Unit = {
    println("Hello world!")
  }

}
