package ui

object GameStdio extends App {

  val theGame = new game.Game()

  def appendSpans(spans: Seq[TextSpan]): Unit = {
    for (span <- spans) {
      print(span.text)
    }
  }

  val result = theGame.interact("/hello")
  appendSpans(result.spans)

  while (true) {
    Console.print("\n> ")
    Console.flush()
    val line = scala.io.StdIn.readLine()
    val result = theGame.interact(line)
    result.overridePrompt.foreach(prompt => println(s"> $prompt"))
    appendSpans(result.spans)
  }

}
