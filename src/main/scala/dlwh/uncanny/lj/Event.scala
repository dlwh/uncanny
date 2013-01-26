package dlwh.uncanny.lj

import java.net.URL
import org.apache.lucene.index.{DirectoryReader, IndexReader}
import org.apache.lucene.store.{NIOFSDirectory, Directory}
import java.io.File
import org.apache.lucene.util.Version
import org.apache.lucene.analysis.util.CharArraySet
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search.{Query, ScoreDoc, IndexSearcher}
import util.matching.Regex
import io.Source
import java.util

/**
 *
 * @author dlwh
 */
case class Event(url: URL, user: String, postId: Int, eventType: String, position: Int)

case class EventMod(url: URL, kind: String)

case class ProcessedDocument(events: Map[Range, String], mods: Map[String, IndexedSeq[Int]])

case class FeaturizedDocument(features: IndexedSeq[Feature])

trait Feature
case class EventFeature(kind: String) extends Feature
case class ModdedEventFeature(kind: String, mod: String, bin: Int) extends Feature


/**
 *
 * @param eventType the name
 * @param query lucene query to run
 * @param regex regex to find matches.
 */
case class EventTemplate(eventType: String, query: String, regex: Regex)

case class ModTemplate(modName: String, regex: Regex, distanceBins: Array[Int] = Array(2,4,8), strictlyBefore: Boolean = true) {
  def binDistance(event: Range, modPositions: IndexedSeq[Int]) = {
    modPositions.map(distanceTo(_, event)).min
  }

  def distanceTo(pos: Int, event: Range) = {
    if(strictlyBefore && pos > event.last) -1
    else if (pos < event.head) flattenSign(util.Arrays.binarySearch(distanceBins, event.head - pos))
    else if(pos > event.last) flattenSign(util.Arrays.binarySearch(distanceBins, pos - event.last))
    else 0
  }

  private def flattenSign(x: Int) = if(x >= 0) x else ~x

}

object EventTemplates {

  val templates = Seq(GotSick,FailedTest,Graduated,BreakUp,Divorce)

  object GotSick extends EventTemplate("GotSick", "sick", "(felt|got|feel) sick".r)
  object FailedTest extends EventTemplate("FailedTest", "failed", "failed (exam|test)".r)
  object Graduated extends EventTemplate("Graduated", "graduated",  "graduated".r)
  object BreakUp extends EventTemplate("BreakUp", """ "my girlfriend broke" OR "my boyfriend broke" OR "dumped"  """, "(girlfriend|boyfriend) broke | dumped".r)
  object Divorce extends EventTemplate("Divorced", """divorced""", "(girlfriend|boyfriend) broke | dumped".r)
}

object ExtractPotentialMatches {
  def main(args: Array[String]) {
    val reader = DirectoryReader.open(new NIOFSDirectory(new File(args(0))))
    val searcher = new IndexSearcher(reader)
    val analyzer = new StandardAnalyzer(Version.LUCENE_40, CharArraySet.EMPTY_SET)
    val parser = new QueryParser(Version.LUCENE_40, "ptext", analyzer)
    for(t <- EventTemplates.templates) {
      println("Q: " + t.eventType)
      val q = parser.parse(t.query)
      val results = searcher.search(q,10000)
      for(d <- results.scoreDocs) {
        val doc = searcher.doc(d.doc)
        val text = doc.get("ptext")
        val m = t.regex.findFirstIn(text)

        val p = doc.get("path")
        val post = doc.get("url")
        println("H: " + p + " " + post + " " + m)
      }
    }
  }
}

object JustSearch {
  def main(args: Array[String]) {
    val reader = DirectoryReader.open(new NIOFSDirectory(new File(args(0))))
    val searcher = new IndexSearcher(reader)
    val analyzer = new StandardAnalyzer(Version.LUCENE_40, CharArraySet.EMPTY_SET)
    val parser = new QueryParser(Version.LUCENE_40, "ptext", analyzer)
    val source = Source.fromInputStream(System.in)
    var q :Query = null
    var lastD: ScoreDoc = null
    for(line <- source.getLines()) {
      if(line.trim == "") {
        val results = searcher.searchAfter(lastD, q, 30)
        for(d: ScoreDoc <- results.scoreDocs) {
          val doc = searcher.doc(d.doc)
          val text = doc.get("ptext")
          val p = doc.get("path")
          val post = doc.get("url")
          val mood = doc.getField("mood").numericValue()
          println(text)
          println("========")
          println("^-- " + p + " " + post + " " + mood)
          println("========")
          lastD = d
        }
      } else if(line.trim == ":quit") {
        return
      } else {
        q = parser.parse(line)
        val results = searcher.search(q,30)
        for(d: ScoreDoc <- results.scoreDocs) {
          val doc = searcher.doc(d.doc)
          val text = doc.get("ptext")
          val p = doc.get("path")
          val post = doc.get("url")
          val mood = doc.getField("mood").numericValue()
          println(text)
          println("========")
          println("^-- " + p + " " + post + " " + mood)
          println("========")
          lastD = d
        }
      }
    }

  }
}