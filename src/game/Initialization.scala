package game

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

object Initialization {

  /** Due to the differences of IDE default working directories the data/
    * folder may be located in another place. This searches for it.
    */
  def findDataFolder(): Option[File] = {
    for (back <- 0 until 10) {
      val path = "../" * back + "script/"
      val file = new File(path + "engine.abt")
      if (file.exists) {
        val root = new File(path)
        assert(root.isDirectory)
        return Some(root)
      }
    }
    None
  }

  /** Walks a directory and lists all files even in nested folders */
  def listFilesRecursive(root: File): Seq[File] = {
    root.listFiles.flatMap(file => {
      if (file.isDirectory) listFilesRecursive(file)
      else Some(file)
    })
  }

  def compileGame(): (vm.Context, LangActions) = {

    // -- Find the script root directory
    val scriptRoot: File = findDataFolder().getOrElse {
      throw new RuntimeException("Could not find script root folder!")
    }

    // -- Load the sources from files
    val sources = listFilesRecursive(scriptRoot).flatMap(file => {
      if (file.getName.endsWith(".abt") && file.canRead) {
        val bytes = Files.readAllBytes(file.toPath())
        val source = new String(bytes, StandardCharsets.UTF_8)
        Some((file.getName, source))
      } else {
        None
      }
    }).toMap

    def printErrorLine(location: lang.SourceLocation) = {
      sources.get(location.file).foreach(source => println(lang.Scanner.errorLine(source, location)))
    }

    // -- Setup the builtins for the compiler
    val context = new vm.Context
    val actions = new LangActions(context)

    context.externals("print") = vm.ExternalAction(actions.print)
    context.externals("fail") = vm.ExternalAction(actions.fail)

    // -- Compile the script sources
    try {
      val asts = sources.map { case (key, value) => lang.Parser.parse(value, key) }.toSeq
      lang.Codegen.codegen(context, asts)
    } catch {
      case err: lang.CompileError =>
        println("Compile error: " + err.getMessage)
        printErrorLine(err.location)
        for (loc <- err.auxLoc) printErrorLine(loc)

        // Terminate game and print the stack for debugging the compiler itself!
        throw err
    }

    (context, actions)
  }
}
