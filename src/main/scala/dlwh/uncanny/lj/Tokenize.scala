package dlwh.uncanny.lj

import xml.XML
import org.jsoup._
import dlwh.uncanny.StandardTokenizer
import breeze.text.segment.{JavaSentenceSegmenter, SentenceSegmenter}
import org.apache.lucene.analysis.{TokenStream, Tokenizer, Analyzer}
import java.io.Reader
import org.apache.lucene.analysis.Analyzer.TokenStreamComponents

/**
 *
 * @author dlwh
 */

object Tokenize {

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
      replaceAll("\\bHavent\\b","Haven't").
      replaceAll("\\bhavent\\b","haven't").
      replaceAll("\\bhasnt\\b","hasn't").
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


  val sentTokenizer = new JavaSentenceSegmenter
  val tok = StandardTokenizer
  def tokenize(text: String): Iterable[Iterable[String]] = {
    var xx = Jsoup.parseBodyFragment(text).body.text()
    xx = antiidiotify(xx)
    val words = for(sent <- sentTokenizer(xx)) yield
      for( w <- tok(sent) ) yield w

    words
  }

}

