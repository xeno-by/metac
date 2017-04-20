package scala.meta
package tools

import scala.meta.internal.prettyprinters._
import scala.meta.internal.parsers.ScalametaParser

object Metac extends App {
  val (flags, List(command, path, _*)) = args.toList.partition(_.startsWith("-"))
  lazy val input = {
    implicit val codec = scala.io.Codec(java.nio.charset.Charset.forName("UTF-8"))
    scala.io.Source.fromFile(new java.io.File(path)).mkString
  }
  val verbose = flags.contains("--verbose") || flags.contains("-v")
  implicit val dialect: scala.meta.Dialect = {
    val prefix = "--dialect="
    val s_dialect = flags.find(_.startsWith(prefix)).map(_.stripPrefix(prefix))
    s_dialect.flatMap(s => Dialect.standards.find(_._1 == s).map(_._2)).getOrElse(scala.meta.dialects.Scala211)
  }
  command match {
    case "tokenize" =>
      def printToken(token: Token): Unit = {
        var message = token.show[Structure]
        if (verbose) message = token.productPrefix + " " + message
        println(message)
      }
      val scannerTokens = input.tokenize.get
      if (flags.contains("--censored")) {
        val parserTokens = new ScalametaParser(Input.String(input), scala.meta.dialects.Scala211).parserTokens
        // parserTokens.foreach(token => println(token.show[Structure] + " of class " + token.getClass))
        parserTokens.foreach(printToken)
      } else {
        // scannerTokens.foreach(token => println(token.show[Structure] + " of class " + token.getClass))
        scannerTokens.foreach(printToken)
      }
      // check #1: everything's covered
      val tokens = scannerTokens
      var isFail = false
      def fail(msg: String) = { isFail = true; println(msg) }
      val bitmap = new Array[Boolean](input.length)
      val tokenmap = scala.collection.mutable.Map[Int, List[Token]]()
      tokens.foreach(token => {
        var i = token.start
        while (i < token.end) {
          if (i < 0 || input.length <= i) fail("TOKEN OUT OF BOUNDS AT " + i + ": " + token)
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
      if (!isFail && input != tokens.map(_.show[Syntax]).mkString) {
        isFail = true
        println("CORRELATION FAILED")
        println("EXPECTED: \n" + input)
        println("ACTUAL: \n" + tokens.map(_.show[Syntax]).mkString)
      }
    case "parse" =>
      implicit val sliceStyle: SliceStyle = if (verbose) SliceStyle.Show else SliceStyle.Hide
      val result = {
        val doesntHavePackages = !input.contains("package ")
        if (doesntHavePackages) new java.io.File(path).parse[Stat].get
        else new java.io.File(path).parse[Source].get
      }
      println(result.show[Syntax])
      println(result.show[Positions])
    case "unpickle" =>
      println(Database.fromClasspath(path))
    case "typecheck" =>
      println("not supported")
  }
}