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


trait EventTemplate {
  // examine all files that match query (coarse pass)
  def query: String
  // fine pass with an actual regex
  def regex: Regex
  def eventType: String
}

trait ModTemplate {
  def regex: Regex
  def modName: String
  def binDistance(doc: String, event: Range, positions: IndexedSeq[Int]):Int
}

object EventTemplates {

  val templates = Seq(GotSick,FailedTest,Graduated,BreakUp,Divorce)

  object GotSick extends EventTemplate {
    def query: String = "sick"
    def regex: Regex = "(felt|got|feel) sick".r

    def eventType = "GotSick"
  }

  object FailedTest extends EventTemplate {
    def query: String = "failed"
    def regex  = "failed (exam|test)".r

    def eventType = "FailedTest"
  }

  object Graduated extends EventTemplate {
    def query: String = "graduated"

    // fine pass with an actual regex
    def regex: Regex = "I .* graduate".r

    def eventType = "Graduated"
  }

  object BreakUp extends EventTemplate {
    def query: String = """ "my girlfriend broke" OR "my boyfriend broke """

    // fine pass with an actual regex
    def regex: Regex = "my (girlfriend|boyfriend) broke".r

    def eventType = "Breakup"
  }

  object Divorce extends EventTemplate {
    def query = "My parents divorced"
    def regex:Regex = "my parents .* divorce".r
    def eventType = "Divorce"
  }
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