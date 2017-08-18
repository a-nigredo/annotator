package dev.nigredo

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{FileSystems, Files}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.meta._

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
      opt[String]('a', "annotation").action((x, c) => c.copy(annotation = x.replaceAllLiterally("{NL}", "\r\n")))
        .text("annotate with").required()
    }.parse(args, CommandLineArgs()) match {
      case Some(config) =>
        val path = new File(config.path)
        if (path.isFile) showResult(run(List(path), config.annotation, config.logger))
        else if (path.exists && path.isDirectory) showResult(run(findFiles(path), config.annotation, config.logger))
      case None =>
    }

    def findFiles(path: File): List[File] = {
      import collection.JavaConverters._
      java.nio.file.Files.walk(FileSystems.getDefault.getPath(path.getPath)).iterator().asScala
        .filter(Files.isRegularFile(_))
        .toList
        .map(_.toFile)
    }

    def showResult(data: List[(String, Int)]): Unit = {
      println("*" * 50)
      data.foreach(x => println(s"Process '${x._1}'. Changes: ${x._2}"))
      println("*" * 50)
    }

    def run(files: List[File], annotation: String, loggerField: String): List[(String, Int)] = {

      def isLogField(name: Term): Boolean = name.toString == loggerField

      def isStrLit(t: Tree, p: Tree) = {
        if (t.is[Term.Name])
          p.collect {
            case q"..$mods val ..$patsnel: $tpeopt = $expr" => patsnel.exists(t => expr.is[Lit.String])
            case _ => false
          }.nonEmpty
        else t.is[Lit.String]
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

      val processed = ListBuffer.empty[(String, Int)]

      files.foreach { file =>
        val tree = file.parse[Source].get
        var changes = 0
        val annotated = tree.collect {
          case t@q"$logField.$method($str)" if isLogField(logField) && isStrLit(str, tree) => getParentClassPos(t)
          case t@q"$logField.$method($str, $cause)" if isLogField(logField) && str.is[Lit.String] => getParentClassPos(t)
          case t@q"$logField.$method($str, ..$any)" if isLogField(logField) && str.is[Lit.String] => getParentClassPos(t)
          case t@q"$logField.$method($marker, $str)" if isLogField(logField) => getParentClassPos(t)
          case t@q"$logField.$method($marker, $str, $cause)" if isLogField(logField) => getParentClassPos(t)
          case t@q"$logField.$method($marker, $str, ..$any)" if isLogField(logField) => getParentClassPos(t)
        }.foldLeft(tree.tokens.tokens) {
          case (zero, pos) =>
            pos match {
              case Position.None => zero
              case _ =>
                changes += 1
                val indent = zero.filter(_.pos.start.line == pos.start.line).takeWhile(_.is[Token.Space])
                val annot = annotation.tokenize.get.tokens.++:(indent)
                zero.filter(_.pos.start.line < pos.start.line) ++ annot ++ zero.filter(_.pos.start.line >= pos.start.line)
            }
        }
        val bw = new BufferedWriter(new FileWriter(file))
        bw.write(annotated.mkString)
        bw.close()
        processed.+=((file.getPath, changes))
      }
      processed.toList
    }
  }
}
