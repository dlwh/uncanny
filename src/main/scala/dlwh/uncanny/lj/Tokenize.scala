package dlwh.uncanny.lj

import xml.XML
import org.jsoup._
import breeze.text.tokenize.{PTBTokenizer, SentenceTokenizer}
import dlwh.uncanny.StandardTokenizer

/**
 *
 * @author dlwh
 */

object Tokenize extends App {

  // replaces misspellings or probable misspellings
  // with their correct words
  def antiidiotify(text: String) = (
    text.replaceAll("\\b[iI]m\\b","I'm").
      replaceAll("\\bcant\\b","can't").
      replaceAll("\\bCant\\b","Can't").
      replaceAll("\\bisnt\\b","isn't").
      replaceAll("\\bIsnt\\b","Isn't").
      replaceAll("\\bdont\\b","don't").
      replaceAll("\\bDont\\b","Don't").
      replaceAll("\\.I ",". I ").
      replaceAll("\\bi "," I ").
      replaceAll("\\bi'"," I'").
      replaceAll("[.] [Uu]\\b",". You ").
      replaceAll("\\b[Uu]\\b"," you ").
      replaceAll("[.] [Uu][rR]\\b",". Your ").
      replaceAll("\\b[Uu][rR]\\b"," your ").
      replaceAll("[.] [rR]\\b",". Are ").
      replaceAll("\\b[rR]\\b"," are ").
      replaceAll("[.] [Yy]\\b",". Why ").
      replaceAll("\\by\\b"," why ").
      replaceAll("([a-z])[.]([A-Z])","\1. \2")
    replaceAll("\ball?ot\b", " a lot ")
      replaceAll("\bAll?ot\b", " A lot ")
  )
  for(f <- args) {
    val xml = XML.loadFile(f)
    val lines = io.Source.stdin.getLines()
    for(x <- xml \\ "event" \ "string") {
      var xx = Jsoup.parseBodyFragment(x.text).body.text()
      println(xx)
      xx = antiidiotify(xx)
      println("Tokenized:")
      for(sent <- new SentenceTokenizer apply xx) {
        println(StandardTokenizer().apply(sent))
      }
      println("\n\n")
      if(lines.next().contains("q")) sys.exit(0)
    }

  }

}
