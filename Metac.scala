package scala.meta
package tools

import scala.meta.internal.prettyprinters._

object Metac extends App {
  val (flags, Array(command, path, _*)) = args.partition(_.startsWith("-"))
  implicit val codec = scala.io.Codec(java.nio.charset.Charset.forName("UTF-8"))
  val input = scala.io.Source.fromFile(new java.io.File(path)).mkString
  implicit val dialect: scala.meta.Dialect = {
    val prefix = "--dialect="
    val s_dialect = flags.find(_.startsWith(prefix)).map(_.stripPrefix(prefix))
    s_dialect.map(Dialect.forName).getOrElse(scala.meta.dialects.Scala211)
  }
  command match {
    case "tokenize" =>
      val scannerTokens = input.tokenize.get
      if (flags.contains("--censored")) {
        val parserTokens = new scala.meta.internal.parsers.ScalametaParser(scannerTokens, dialect).parserTokens
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
      implicit val sliceStyle: SliceStyle = {
        if (flags.contains("--sliced")) SliceStyle.Show
        else SliceStyle.Hide
      }
      val result = {
        val doesntHavePackages = !input.contains("package ")
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
  }
}