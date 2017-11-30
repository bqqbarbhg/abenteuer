package ui

import javax.swing.SpringLayout.Constraints

import scala.swing._
import scala.swing.event._
import scala.annotation.tailrec
import javax.swing.UIManager
import javax.swing.text.StyleConstants

object GameGui extends SimpleSwingApplication {
  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

  def top = new MainFrame {

    val look = new TextPane() {
      editable = false
    }
    val output = new TextPane() {
      editable = false
    }
    val input = new TextField(40) {
      font = new java.awt.Font("SansSerif", java.awt.Font.PLAIN, 14)
    }

    val lookScroll = new ScrollPane(look) {
    }
    val outputScroll = new ScrollPane(output) {
    }

    this.listenTo(input.keys)

    val theGame = new game.Game()
    var ephemeralBegin: Option[Int] = None

    class OutputView(val textPane: TextPane) {
      val doc = textPane.styledDocument
      val default = doc.addStyle("Default", null)
      val spacer = doc.addStyle("Spacer", default)
      val bold = doc.addStyle("Bold", default)
      val faded = doc.addStyle("Faded", default)
      StyleConstants.setFontSize(default, 12)
      StyleConstants.setFontSize(spacer, 8)
      StyleConstants.setBold(bold, true)
      StyleConstants.setForeground(faded, new Color(0x555555))

      def appendSpans(spans: Seq[TextSpan]): Unit = {
        for (span <- spans) {
          val style = span.style match {
            case TextStyle.Normal => default
            case TextStyle.Bold => bold
          }

          doc.insertString(doc.getLength, span.text, style)
        }
      }
    }

    val lookView = new OutputView(look)
    val outputView = new OutputView(output)

    def updateLook(): Unit = {
      return
      lookView.doc.remove(0, lookView.doc.getLength)
      lookView.appendSpans(theGame.interact("look").spans)
    }

    updateLook()

    @tailrec
    def processCommand(command: String, showPrompt: Boolean): Unit = {
      val doc = outputView.doc
      val spacer = outputView.spacer
      val faded = outputView.faded

      for (begin <- ephemeralBegin) {
        doc.remove(begin, doc.getLength - begin)
        ephemeralBegin = None
      }

      val begin = doc.getLength
      val result = theGame.interact(command)
      val prompt = result.overridePrompt.getOrElse(command)

      if (showPrompt) {
        doc.insertString(doc.getLength, "\n", spacer)
        doc.insertString(doc.getLength, "\n", spacer)
        doc.insertString(doc.getLength, s"> ${prompt}\n", faded)
        doc.insertString(doc.getLength, "\n", spacer)
      }

      outputView.appendSpans(result.spans)
      if (result.ephemeral)
        ephemeralBegin = Some(begin)

      doc.insertString(doc.getLength, "\n", spacer)

      // Reset caret to end to enable autoscroll to bottom
      output.caret.position = doc.getLength

      result.autoCommand match {
        case Some(cmd) => processCommand(cmd, false)
        case None =>
      }
    }

    processCommand("/hello", false)

    this.reactions += { case keyEvent: KeyPressed =>
        if (keyEvent.source == this.input && keyEvent.key == Key.Enter) {
          var command = this.input.text.trim
          this.input.text = ""
          if (command.nonEmpty) {
            processCommand(command, true)
            updateLook()
          }
        }
    }

    this.contents = new GridBagPanel {
      import scala.swing.GridBagPanel.Anchor._
      import scala.swing.GridBagPanel.Fill
      layout += outputScroll -> new Constraints(0, 0, 1, 1, 1, 1, NorthWest.id, Fill.Both.id, new Insets(5, 5, 5, 5), 0, 0)
      // layout += lookScroll -> new Constraints(0, 1, 1, 1, 1, 1, NorthWest.id, Fill.Both.id, new Insets(5, 5, 5, 5), 0, 0)
      layout += input -> new Constraints(0, 2, 1, 1, 0, 0, NorthWest.id, Fill.Both.id, new Insets(5, 5, 5, 5), 0, 0)
    }

    val titleResult = theGame.interact("/title")
    this.title = titleResult.spans.map(_.text).mkString("")

    this.minimumSize = new Dimension(400, 400)
    this.pack()
    this.size = new Dimension(400, 400)

    input.requestFocusInWindow()

  }
}
