package scala.meta
package tools

import scala.meta.dialects.Scala211
import scala.{meta => api}
import scala.meta.internal.{ast => m}
import scala.meta.internal.prettyprinters._
import PositionStyle.Colorful

object Metac extends App {
  val (flags, Array(command, path, _*)) = args.partition(_.startsWith("-"))
  implicit val codec = scala.io.Codec(java.nio.charset.Charset.forName("UTF-8"))
  val source = scala.io.Source.fromFile(new java.io.File(path)).mkString
  command match {
    case "benchmark" =>
      var files = List[java.io.File]()
      def loop(dir: java.io.File): Unit = {
        dir.listFiles.filter(_.isFile).filter(_.getName.endsWith(".scala")).map(file => files = file +: files)
        dir.listFiles.filter(_.isDirectory).map(loop)
      }
      loop(new java.io.File("/Users/xeno_by/Projects/Scala2117/src/library"))
      loop(new java.io.File("/Users/xeno_by/Projects/Scala2117/src/reflect"))
      loop(new java.io.File("/Users/xeno_by/Projects/Scala2117/src/compiler"))
      val contents = files.map(file => (file, scala.io.Source.fromFile(file).mkString)).toMap
      val compilerCommand = new scala.tools.nsc.CompilerCommand(Nil, _ => ???)
      val reporter = new scala.tools.nsc.reporters.StoreReporter
      val global = new scala.tools.nsc.Global(compilerCommand.settings, reporter)
      val run = new global.Run
      def parseReflect(file: java.io.File, contents: String): Unit = {
        global.newUnitParser(contents, "<adhoc>").parse
        if (reporter.infos.nonEmpty) println(file.getAbsolutePath)
        reporter.reset()
      }
      val startReflect = System.currentTimeMillis()
      var i = 0
      while (i < 40) {
        if (((i + 1) % 10) == 0) println(i + 1)
        contents.foreach{ case (f, content) => parseReflect(f, content) }
        i += 1
      }
      val endReflect = System.currentTimeMillis()
      println("Reflect parsing: " + (endReflect - startReflect) / 1000.0)
      def parseMeta(file: java.io.File, contents: String): Unit = {
        import scala.meta.dialects.Scala211
        try contents.parse[Source]
        catch { case ex: Throwable => println(file.getAbsolutePath); throw ex }
      }
      val startMeta = System.currentTimeMillis()
      var j = 0
      while (j < 40) {
        if (((j + 1) % 10) == 0) println(j + 1)
        contents.foreach{ case (f, content) => parseMeta(f, content) }
        j += 1
      }
      val endMeta = System.currentTimeMillis()
      println("Meta parsing: " + (endMeta - startMeta) / 1000.0)
      println(((endMeta - startMeta) / (endReflect - startReflect)) + "x")
    case "tokenize" =>
      implicit val dialect: scala.meta.Dialect = {
        if (flags.contains("--scala211")) scala.meta.dialects.Scala211
        else if (flags.contains("--dotty")) scala.meta.dialects.Dotty
        else scala.meta.dialects.Scala211 // default is Scala211
      }
      val scannerTokens = new java.io.File(path).tokens
      if (flags.contains("--censored")) {
        val parserTokens = new scala.meta.internal.parsers.ScalametaParser(Input.String(source)).parserTokens
        // parserTokens.foreach(token => println(token.show[Raw] + " of class " + token.getClass))
        parserTokens.foreach(token => println(token.show[Raw]))
      } else {
        // scannerTokens.foreach(token => println(token.show[Raw] + " of class " + token.getClass))
        scannerTokens.foreach(token => println(token.show[Raw]))
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
      if (!isFail && source != tokens.map(_.show[Code]).mkString) {
        isFail = true
        println("CORRELATION FAILED")
        println("EXPECTED: \n" + source)
        println("ACTUAL: \n" + tokens.map(_.show[Code]).mkString)
      }
    case "parse" =>
      implicit val dialect: scala.meta.Dialect = {
        if (flags.contains("--scala211")) scala.meta.dialects.Scala211
        else if (flags.contains("--dotty")) scala.meta.dialects.Dotty
        else scala.meta.dialects.Scala211 // default is Scala211
      }
      val result = {
        val doesntHavePackages = !source.contains("package ")
        if (doesntHavePackages) new java.io.File(path).parse[Stat]
        else new java.io.File(path).parse[Source]
      }
      println(result.show[Code])
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
    case "typecheck" =>
      implicit val c = {
        val scalaLibraryJar = "/Users/xeno_by/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.11.7.jar"
        val options = {
          // http://stackoverflow.com/questions/5711084/java-runtime-getruntime-getting-output-from-executing-a-command-line-program
          val cmd = Array("scalac-options")
          val scanner = new java.util.Scanner(Runtime.getRuntime().exec(cmd).getInputStream()).useDelimiter("\\A")
          val options = if (scanner.hasNext()) scanner.next() else ""
          var optionLines = options.split('\n')
          optionLines :+= ("-cp " + scalaLibraryJar)
          optionLines.mkString(" ")
        }
        def Toolbox(options: String, artifacts: Artifact*)(implicit resolver: Resolver): Context = {
          import scala.meta.internal.hosts.scalac.contexts.{Compiler => Compiler}
          import scala.meta.internal.hosts.scalac.contexts.{Proxy => ProxyImpl}
          new ProxyImpl(Compiler(options), Domain(artifacts: _*)) {
            override def toString = s"""Toolbox("$options", ${artifacts.mkString(", ")})"""
          }
        }
        if (flags.contains("--scala211")) Toolbox(options, Artifact(scalaLibraryJar))
        else if (flags.contains("--dotty")) sys.error("scala.meta can't be hosted in Dotty yet")
        else Toolbox(options, Artifact(scalaLibraryJar)) // default is Scala211
      }
      val doesntHavePackages = !source.contains("package ")
      val possiblyWrappedString = if (doesntHavePackages) s"class Dummy { def dummy: Unit = { locally { $source }; (); }; }" else source
      val possiblyWrappedSource = possiblyWrappedString.parse[Source]
      val possiblyWrappedArtifact = c.load(Artifact(possiblyWrappedSource))
      val Seq(possiblyWrappedResult) = possiblyWrappedArtifact.sources
      val result = {
        if (doesntHavePackages) {
          possiblyWrappedResult match {
            case m.Source(List(
                  m.Defn.Class(_, _, _, _,
                    m.Template(_, _, _, Some(List(
                      m.Defn.Def(_, _, _, _, _,
                        m.Term.Block(List(m.Term.Apply(_, List(result)), m.Lit.Unit()))))))))) =>
              result match {
                case m.Term.Block(List(single)) => single
                case other => other
              }
            case _ =>
              possiblyWrappedResult
          }
        } else {
          possiblyWrappedResult
        }
      }
      println(result.show[Code])
      println(result.show[Semantics])
  }
}