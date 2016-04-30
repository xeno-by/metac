package scala.meta
package tools

import scala.meta.dialects.Scala211
import scala.{meta => api}
import scala.meta.internal.{ast => m}
import scala.meta.internal.prettyprinters._

object Metac extends App {
  val (flags, Array(command, path, _*)) = args.partition(_.startsWith("-"))
  implicit val codec = scala.io.Codec(java.nio.charset.Charset.forName("UTF-8"))
  val source = scala.io.Source.fromFile(new java.io.File(path)).mkString
  command match {
    case "tokenize" =>
      val dialect: scala.meta.Dialect = {
        if (flags.contains("--scala211")) scala.meta.dialects.Scala211
        else if (flags.contains("--dotty")) scala.meta.dialects.Dotty
        else scala.meta.dialects.Scala211 // default is Scala211
      }
      val scannerTokens = new java.io.File(path).tokenize.get
      if (flags.contains("--censored")) {
        val parserTokens = new scala.meta.internal.parsers.ScalametaParser(source.tokenize.get, dialect).parserTokens
        // parserTokens.foreach(token => println(token.show[Structure] + " of class " + token.getClass))
        parserTokens.foreach(token => println(token.show[Structure]))
      } else {
        // scannerTokens.foreach(token => println(token.show[Structure] + " of class " + token.getClass))
        scannerTokens.foreach(token => println(token.show[Structure]))
      }
      // check #1: everything's covered
      val tokens = scannerTokens
      var isFail = false
      def fail(msg: String) = { isFail = true; println(msg) }
      val bitmap = new Array[Boolean](source.length)
      val tokenmap = scala.collection.mutable.Map[Int, List[Token]]()
      tokens.foreach(token => {
        var i = token.start
        while (i < token.end) {
          if (i < 0 || source.length <= i) fail("TOKEN OUT OF BOUNDS AT " + i + ": " + token)
          else {
            tokenmap(i) = token +: tokenmap.getOrElse(i, Nil)
            if (bitmap(i)) fail("TOKENS OVERLAP AT " + i + ": " + tokenmap(i).mkString(", "))
            bitmap(i) = true
          }
          i += 1
        }
      })
      bitmap.zipWithIndex.filter(!_._1).foreach{ case (_, i) => fail("TOKENS DON'T COVER " + i) }
      // check #2: tostring works
      if (!isFail && source != tokens.map(_.show[Syntax]).mkString) {
        isFail = true
        println("CORRELATION FAILED")
        println("EXPECTED: \n" + source)
        println("ACTUAL: \n" + tokens.map(_.show[Syntax]).mkString)
      }
    case "parse" =>
      implicit val dialect: scala.meta.Dialect = {
        if (flags.contains("--scala211")) scala.meta.dialects.Scala211
        else if (flags.contains("--dotty")) scala.meta.dialects.Dotty
        else scala.meta.dialects.Scala211 // default is Scala211
      }
      implicit val sliceStyle: SliceStyle = {
        if (flags.contains("--sliced")) SliceStyle.Show
        else SliceStyle.Hide
      }
      val result = {
        val doesntHavePackages = !source.contains("package ")
        if (doesntHavePackages) new java.io.File(path).parse[Stat].get
        else new java.io.File(path).parse[Source].get
      }
      println(result.show[Syntax])
      println(result.show[Positions])
      def check(tree: Tree): Boolean = {
        def loop(x: Any): Boolean = x match {
          case x: Tree => check(x)
          case x: ::[_] => x.forall(loop)
          case x: Some[_] => loop(x.get)
          case x => true
        }
        tree.tokens.isAuthentic && tree.productIterator.toList.forall(loop)
      }
      if (!check(result)) println("BROKEN POSITIONS")
    // case "typecheck" =>
    //   implicit val c = {
    //     val scalaLibraryJar = "/Users/xeno_by/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.11.7.jar"
    //     val options = {
    //       // http://stackoverflow.com/questions/5711084/java-runtime-getruntime-getting-output-from-executing-a-command-line-program
    //       val cmd = Array("scalac-options")
    //       val scanner = new java.util.Scanner(Runtime.getRuntime().exec(cmd).getInputStream()).useDelimiter("\\A")
    //       val options = if (scanner.hasNext()) scanner.next() else ""
    //       var optionLines = options.split('\n')
    //       optionLines :+= ("-cp " + scalaLibraryJar)
    //       optionLines.mkString(" ")
    //     }
    //     def Toolbox(options: String, artifacts: Artifact*)(implicit resolver: Resolver): Context = {
    //       import scala.meta.internal.hosts.scalac.contexts.{Compiler => Compiler}
    //       import scala.meta.internal.hosts.scalac.contexts.{Adapter => AdapterImpl}
    //       new AdapterImpl(Compiler(options), Domain(artifacts: _*)) {
    //         override def toString = s"""Toolbox("$options", ${artifacts.mkString(", ")})"""
    //       }
    //     }
    //     if (flags.contains("--scala211")) Toolbox(options, Artifact(scalaLibraryJar))
    //     else if (flags.contains("--dotty")) sys.error("scala.meta can't be hosted in Dotty yet")
    //     else Toolbox(options, Artifact(scalaLibraryJar)) // default is Scala211
    //   }
    //   val doesntHavePackages = !source.contains("package ")
    //   val possiblyWrappedString = if (doesntHavePackages) s"class Dummy { def dummy: Unit = { locally { $source }; (); }; }" else source
    //   val possiblyWrappedSource = possiblyWrappedString.parse[Source]
    //   val possiblyWrappedArtifact = c.load(Artifact(possiblyWrappedSource))
    //   val Seq(possiblyWrappedResult) = possiblyWrappedArtifact.sources
    //   val result = {
    //     if (doesntHavePackages) {
    //       possiblyWrappedResult match {
    //         case m.Source(List(
    //               m.Defn.Class(_, _, _, _,
    //                 m.Template(_, _, _, Some(List(
    //                   m.Defn.Def(_, _, _, _, _,
    //                     m.Term.Block(List(m.Term.Apply(_, List(result)), m.Lit(())))))))))) =>
    //           result match {
    //             case m.Term.Block(List(single)) => single
    //             case other => other
    //           }
    //         case _ =>
    //           possiblyWrappedResult
    //       }
    //     } else {
    //       possiblyWrappedResult
    //     }
    //   }
    //   println(result.show[Code])
    //   println(result.show[Semantics])
  }
}