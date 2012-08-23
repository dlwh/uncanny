package dlwh.uncanny.lj

import java.io.File
import org.apache.lucene.index.{IndexWriterConfig, IndexWriter}
import org.apache.lucene.util.Version
import org.apache.lucene.store.{NIOFSDirectory, Directory}
import xml.{NodeSeq, XML}
import java.util.{Collections, Date}
import java.text.SimpleDateFormat
import org.apache.lucene.document._
import org.apache.lucene.document.Field.Store
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.analysis.util.CharArraySet

/**
 *
 * @author dlwh
 */
object IndexCorpus extends App {
  var count = 0

  val out = new File(args(0))
  if (!out.exists) out.mkdirs()

  val analyzer = new StandardAnalyzer(Version.LUCENE_40, CharArraySet.EMPTY_SET)
  val iwc = new IndexWriterConfig(Version.LUCENE_40, analyzer)

  val writer = new IndexWriter(new NIOFSDirectory(out), iwc)

  val fmt = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

  val document = new Document()
  val userF = new StringField("user", "", Store.YES)
  document add userF
  val textF = new TextField("ptext", "", Store.YES)
  document add textF
  val pathF = new StringField("path", "", Store.YES)
  document add pathF
  val urlF = new StringField("url", "", Store.YES)
  document add urlF
  val moodF = new IntField("mood", -1, Store.YES)
  document add moodF
  val timeF = new LongField("time", -1, Store.YES)
  document add timeF

  val tokensF = new StringField("tokens","", Store.YES)
  document add tokensF

  for(dir <- new File(args(1)).listFiles.iterator) {
    println(dir)
    for( f <- dir.listFiles())  {
      try {

        val xml = XML.loadFile(f)
        for(post <- xml \\ "post") {
          val user = (post \ "user").text
          userF.setStringValue(user)

          val text = (post \ "event").text
          textF.setStringValue(text)

          val time = fmt.parse((post \ "eventtime").text).getTime
          timeF.setLongValue(time)

          val url = (post \ "url").text
          urlF.setStringValue(url)

          val path = f.getPath
          pathF.setStringValue(path)

          val mood = (post \ "current_moodid").text
          if(mood != "") {
            moodF.setIntValue(mood.toInt)
          } else {
            moodF.setIntValue(-1)
          }

          val tokens = Tokenize.tokenize(text).map(_.mkString(" ")).mkString("\n")
          tokensF.setStringValue(tokens)


          writer.addDocument(document)
        }

      } catch {
        case e : Exception =>
          println("Problem with " + f)
          e.printStackTrace()

      }
    }
    writer.commit()
  }

  writer.close()

}
