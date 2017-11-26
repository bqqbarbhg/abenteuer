package ui

import java.io.{BufferedReader, BufferedWriter, InputStreamReader, OutputStreamWriter}
import java.net.ServerSocket

import util.EscapeUtil.EscapedString

import scala.util.Try

object GameServer extends App {

  val theGame = new game.Game()

  val contentLengthRegex = raw"""\s*content-length:\s*(\d+)\s*""".r

  val port = args.lift(1).flatMap(arg => Try(arg.toInt).toOption).getOrElse(8081)
  val socket = new ServerSocket(port)
  println(s"Serving at port $port")

  def spanToJson(span: TextSpan): String = {
    val style = s"""{ "bold": ${span.style.bold} }"""
    s"""{ "text": "${span.text.escape}", "style": $style }"""
  }

  while (true) {
    val clientSocket = socket.accept()
    val in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream))
    val out = new BufferedWriter(new OutputStreamWriter(clientSocket.getOutputStream))

    var line: String = ""
    var contentLength: Int = 0
    do {
      line = in.readLine()
      line.toLowerCase match {
        case contentLengthRegex(value) =>
          // Can't fail since regex requires \d+
          contentLength = value.toInt
        case _ =>
      }
    } while (line != "")

    val contentBytes = new Array[Char](contentLength)
    in.read(contentBytes)
    val content = new String(contentBytes)
    val command = content.trim
    println(s"> ${command.escape}")

    val result = theGame.interact(command)

    val spans = result.spans.map(spanToJson).mkString(", ")
    val overridePrompt = result.overridePrompt.map(a => "\"" + a.escape + "\"").getOrElse("null")

    val body =
      s"""
         |{
         |  "spans": [$spans],
         |  "ephemeral": ${result.ephemeral},
         |  "overridePrompt": ${overridePrompt}
         |}
       """.stripMargin

    val date = java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME.format(java.time.ZonedDateTime.now(java.time.ZoneId.of("GMT")))

    out.write("HTTP/1.0 200 OK\r\n")
    out.write(s"Date: $date\r\n")
    out.write("Server: Abenteuer\r\n")
    out.write("Connection: close\r\n")
    out.write("Access-Control-Allow-Origin: *\r\n")
    out.write("Content-Type: application/json\r\n")
    out.write(s"Content-Length: ${body.length}\r\n")
    out.write("\r\n")
    out.write(body)
    out.flush()

    clientSocket.close()

  }

}

