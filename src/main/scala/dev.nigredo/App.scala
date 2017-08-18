package dev.nigredo

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{FileSystems, Files}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.meta._
import scala.meta.parsers.Parsed.{Error, Success}
import scala.util.Try

object App {

  def main(args: Array[String]): Unit = {

    new scopt.OptionParser[CommandLineArgs]("annotator") {
      head("annotator")
      opt[String]('p', "path").action((x, c) => c.copy(path = x)).text("path to scala files").required().validate { x =>
        val file = new File(x)
        if ((file.isFile && file.getPath.endsWith(".scala")) || file.isDirectory) success
        else if (file.isFile && !file.getPath.endsWith(".scala")) failure(s"File '$x' is not .scala file")
        else failure(s"Path '$x' is not directory")
      }
      opt[String]('f', "loggerField").action((x, c) => c.copy(logger = x)).text("logger field name")
      opt[String]('a', "annotation").action((x, c) => c.copy(annotation = x.replaceAllLiterally("_NL_", "\r\n")))
        .text("annotate with").required()
    }.parse(args, CommandLineArgs()) match {
      case Some(config) =>
        val path = new File(config.path)
        if (path.isFile) run(List(path), config.annotation, config.logger)
        else if (path.exists && path.isDirectory) run(findFiles(path), config.annotation, config.logger)
      case None =>
    }

    def findFiles(path: File): List[File] = {
      import collection.JavaConverters._
      java.nio.file.Files.walk(FileSystems.getDefault.getPath(path.getPath)).iterator().asScala
        .filter(Files.isRegularFile(_))
        .toList
        .map(_.toFile)
        .filter(_.getPath.endsWith(".scala"))
    }

    def run(files: List[File], annotation: String, loggerField: String): Unit = {

      def isLogField(name: Term): Boolean = name.toString == loggerField

      def isStrLit(t: Tree, p: Tree) = {
        t match {
          case _: Term.Name => p.collect {
            case q"..$mods val ..$patsnel: $tpeopt = $expr" => patsnel.exists(t => expr.is[Lit.String])
            case _ => false
          }.nonEmpty
          case t1: Term.ApplyInfix => t1.lhs.is[Lit.String]
          case _: Lit.String => true
          case _ => false
        }
      }

      val annotated = ListBuffer.empty[Defn]

      def getParentClassPos(tree: Tree): Position = {

        @tailrec
        def rec(tree: Option[Tree], pos: Position = Position.None): Position =
          tree match {
            case Some(value: Defn.Trait) =>
              if (!annotated.contains(value)) {
                annotated += value
                rec(None, value.pos)
              } else rec(None, Position.None)
            case Some(value: Defn.Class) =>
              if (!annotated.contains(value)) {
                annotated += value
                rec(None, value.pos)
              } else rec(None, Position.None)
            case Some(value: Defn.Object) =>
              if (!annotated.contains(value)) {
                annotated += value
                rec(None, value.pos)
              } else rec(None, Position.None)
            case Some(value) => rec(value.parent, value.pos)
            case None => pos
          }

        rec(tree.parent)
      }

      files.foreach { file =>
        Try(file.parse[Source]) match {
          case scala.util.Success(source) => source match {
            case Error(pos, msg, details) => sys.error(msg)
            case Success(tree) =>
              val changes = ListBuffer.empty[Int]
              val tokens = tree.tokens.tokens
              val annotated = tree.collect {
                case t@q"$logField.$method($str)" if isLogField(logField) && isStrLit(str, tree) => getParentClassPos(t)
                case t@q"$logField.$method($str, $cause)" if isLogField(logField) && str.is[Lit.String] => getParentClassPos(t)
                case t@q"$logField.$method($str, ..$any)" if isLogField(logField) && str.is[Lit.String] => getParentClassPos(t)
                case t@q"$logField.$method($marker, $str)" if isLogField(logField) => getParentClassPos(t)
                case t@q"$logField.$method($marker, $str, $cause)" if isLogField(logField) => getParentClassPos(t)
                case t@q"$logField.$method($marker, $str, ..$any)" if isLogField(logField) => getParentClassPos(t)
              }.foldLeft(tokens) {
                case (zero, pos) =>
                  pos match {
                    case Position.None => zero
                    case _ =>
                      changes += pos.start.line
                      val indent = zero.filter(_.pos.start.line == pos.start.line).takeWhile(_.is[Token.Space])
                      val annot = annotation.tokenize.get.tokens.++:(indent)
                      zero.filter(_.pos.start.line < pos.start.line) ++ annot ++ zero.filter(_.pos.start.line >= pos.start.line)
                  }
              }
              if (changes.nonEmpty) {
                val bw = new BufferedWriter(new FileWriter(file))
                bw.write(annotated.mkString)
                bw.close()
                println("*" * 50)
                println(s"Annotate '${file.getPath}'. \r\n\t Lines: ${changes.mkString(",")}")
                println("*" * 50)
              }
          }
          case scala.util.Failure(exception) =>
            println("*" * 50)
            println(s"Could not anotate ${file.getPath} due to an error: \r\n\t${exception.getMessage}")
            println("*" * 50)
        }
      }
    }
  }
}
