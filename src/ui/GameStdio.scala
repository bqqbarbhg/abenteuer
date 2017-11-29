package ui

object GameStdio extends App {
  val theGame = new game.Game()

  def appendSpans(spans: Seq[TextSpan]) = spans.foreach(span => print(span.text))

  val hello = theGame.interact("/hello")
  appendSpans(hello.spans)

  while (true) {
    Console.print("\n> ")
    Console.flush()

    val line = scala.io.StdIn.readLine()

    var result = theGame.interact(line)
    // In case the game wants to override the user prmpt, print it!
    // Sadly, we can't replace already printed lines of the standard output.
    result.overridePrompt.foreach(prompt => println(s"> $prompt"))
    appendSpans(result.spans)

    // Run autoCommands
    while (result.autoCommand.isDefined) {
      val command = result.autoCommand.get
      result = theGame.interact(command)
      val prompt = result.overridePrompt.getOrElse(command)
      println(s"\n> $prompt")
      appendSpans(result.spans)
    }
  }
}
