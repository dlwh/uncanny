package dlwh.uncanny.openlibrary

import io.Source
import breeze.text.tokenize._
import breeze.text.segment.JavaSentenceSegmenter

/**
 *
 * @author dlwh
 */

object Tokenize extends App {
  val badInitialWords = Set("CONTENTS","PREFACE","Copyright","INTRODUCTION","INTKODUCTION", "Chapter I", "Chapter 1")
  var str2:String = _
  for(f <- args) {
    val in = System.currentTimeMillis()
    var str = Source.fromFile(f).mkString
    val destroyed = badInitialWords.map{w => val i = str.indexOf(w); if (i < 0) i else i + w.length}.foldLeft(0)(_ max _)
    str = str.substring(destroyed)
    str = str.replaceAll("(?m)\n\n.*\n\n","")
    str = "(?m)- \n\n*".r.replaceAllIn(str, "")
    str = "(?m) \n(.)".r.replaceAllIn(str, " $1")


    val sents = (new JavaSentenceSegmenter apply str).toIndexedSeq
    val toks = for(sent <- sents) yield PTBTokenizer(sent)
    println("out: " + (System.currentTimeMillis() - in))

    str2 = str

  }

}
