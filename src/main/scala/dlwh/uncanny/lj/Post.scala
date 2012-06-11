package dlwh.uncanny.lj

import java.net.URL
import xml.XML
import scala.io.Source
import scala.runtime.ScalaRunTime
import scalala.tensor.mutable.Counter
import scalanlp.text.tokenize.{PorterStemmer, SimpleEnglishTokenizer, StopWordFilter}
import scalanlp.io.FileIterable
import java.io._
import scala.Predef._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import collection.mutable.ArrayBuffer
import dlwh.uncanny.lj.Moods.Other
import scalanlp.util.{Encoder, Index}
import scalanlp.data.{Datasets, Example}
import scalanlp.stats.ContingencyStats
import scalala.tensor.sparse.SparseVector
import scalanlp.config.Configuration
import scalanlp.classify.{LogisticClassifier, SVM}
import scalanlp.optimize.FirstOrderMinimizer.OptParams
import scalanlp.collection.mutable.Beam
import scalala.tensor.dense.DenseVector

/**
 *
 * @author dlwh
 */

@SerialVersionUID(1L)
case class Post(mood: Int,
                user: String,
                subject: String,
                time: Long,
                url: URL,
                location: String,
                music: String,
                event: String) extends Example[Int, (String, Long, String)] {
  def features = (subject, time, event)

  def id = url.toString

  def label = mood

  override def toString = ScalaRunTime._toString(this)
}

object Post {
  def postsFromXML(n: xml.Node) = {
    for {
      post <- n \ "post"
    } yield {
      val user = (post \ "user").text
      val subject = (post \ "subject" \ "string").text
      val time = (post \ "event_timestamp" \ "int").text.toLong
      val url = new URL((post \ "url" \ "string").text)
      val event = (post \ "event" \ "string").text
      val props = post \ "props"
      val location = (props \ "current_location" \ "string").text
      val music = (props \ "current_music" \ "string").text
      val moodString = (props \ "current_moodid" \ "int").text
      val mood = if(moodString == "") -1 else moodString.toInt
      Post(mood, user, subject, time, url, location, music, event)
    }
  }

  def main(args: Array[String]) {
    for(a <- args) {
      val xml = XML.loadFile(a)
      println(Post.postsFromXML(xml))
    }

  }
}

object SerializePosts extends App {
  val base = new File(args(0))
  base.mkdirs()
  for(dir <- args.drop(1)) {
    println(dir)
    val postsForDir: Array[Array[Post]] = for {
      f <- new File(dir).listFiles if f.getName.endsWith(".xml")
    } yield for {
      post <- Post.postsFromXML(XML.loadFile(f)).toArray
    } yield post
    println(scalanlp.util.memoryString)
    System.gc()
    println(scalanlp.util.memoryString)

    scalanlp.util.writeObject(new File(base,new File(dir).getName +".ser.gz"), postsForDir)
  }

}

object BuildWordCounts {
  val filter = StopWordFilter()
  val tok = SimpleEnglishTokenizer()
  val stem = new PorterStemmer
  def pipe(x: String) = stem(filter(tok(x)).map(_.toLowerCase).filter(s => s.length < 20 && s.forall(_.isLetter)))
  def tokens(post: Post) = {
    val music = pipe(post.music).map("MUSIC_" + _)
    val subject = pipe(post.subject).map("SUBJ_" + _)
    val text: Iterable[String] = pipe(post.event)
    text.toArray ++ music ++ subject
  }
  def main(args: Array[String]) {

    var i = 0;
    val outDir = new File("processed/counts")
      outDir.mkdirs()

    for(file <- args.map(new File(_))) {
        println("\n" +file);
      var total = 0.0
      val counts = Counter[String,Int]()
      for(posts <- scalanlp.util.readObject[Array[Array[Post]]](file).iterator;
          post <- posts.par.map(tokens).iterator;
          _ = {i += 1; if (i % 1000 == 0) print(".")};
          tok <- post) {
        counts(tok) += 1
        total += 1

      }

      println("Num Types: " + counts.size)
      println("Num Tokens: " + total)
      var totalWritten = 0
      var countWritten = 0
      val fname = file.getName.takeWhile(_ != '.') + ".txt.gz"
      val out = new PrintStream(new GZIPOutputStream(new FileOutputStream(new File(outDir,fname))))
      for( (k,v) <- counts.pairsIterator) {
        if(v >= 5E-6 * total && v > 2) {
          out.println(k + " " + v)
          totalWritten += 1
          countWritten += v
        }
      }
      out.close()
      println("Written: " + totalWritten + " " + countWritten)
    }
  }
}

object SumCounts extends App {
  val result = Counter[String, Int]
  var total = 0.0
  for(f <- args; line <- Source.fromInputStream(new GZIPInputStream(new FileInputStream(f))).getLines()) {
    val lastSpace = line.lastIndexOf(' ')
    assert(lastSpace >= 0, line)
    val word = line.substring(0,lastSpace)
    val num = line.substring(lastSpace +1).toInt
    result(word) += num
    total += num
  }

  val fname = "results.txt.gz"
  val out = new PrintStream(new GZIPOutputStream(new FileOutputStream(new File(fname))))
  var totalWritten = 0
  var countWritten = 0.

  val finalWords = ArrayBuffer[String]()
  for( (k,v) <- result.pairsIterator) {
    if(v >= 5E-7 * total && v > 2) {
      out.println(k + " " + v)
      totalWritten += 1
      countWritten += v
      finalWords += k
    }
  }
  println("Written: " + totalWritten + " " + countWritten)
  out.close()
  val fname2 = "index.txt"
  val out2 = new PrintStream(new FileOutputStream(new File(fname2)))
  for( w <- finalWords.sortBy(-result(_))) {
    out2.println(w)
  }
  out2.close()



}

object BuildTransitions extends App {
  val outDir = new File("processed/transitions")
  outDir.mkdirs()
  for(file <- args.map(new File(_)).iterator) {
    println("\n" +file);
    val transitions = for(posts <- scalanlp.util.readObject[Array[Array[Post]]](file)) yield {
      val fposts = posts.filter( p => p.label >= 0 && p.label < Moods.moods.size)
      fposts.map(_.label)
    }
    val out = new File(outDir,file.getName)
    scalanlp.util.writeObject(out, transitions)
  }

  def write[T](it: Iterator[T], file: File) = {
    val out = new ObjectOutputStream(new BufferedOutputStream(new GZIPOutputStream(new FileOutputStream(file))));
    for(t <- it) out.writeObject(t);
    out.close();
    new FileIterable[T](file);
  }

}

object IndexFiles extends App {
  import BuildWordCounts._

  val index = Index { for(line <- Source.fromFile("index.txt").getLines()) yield line.trim}
  val outDir = new File("processed/indexed")
  outDir.mkdirs()

  for(file <- args.map(new File(_))) {
      println("\n" +file);
    val indexed  = {for(posts <- scalanlp.util.readObject[Array[Array[Post]]](file).iterator)
    yield {for(post <- posts.par) yield {
      val toks = tokens(post).map(index).filter(_ != -1).toArray
      Example(post.mood, toks, post.id)
    }}.seq}.toArray

    val out = new File(outDir,file.getName)
    scalanlp.util.writeObject(out, indexed)
  }

}


object BuildDataset extends App {
  val LEN_SAD = 7
  val LEN_CONTEXT = 5

  val outDir = new File("processed/dataset")
  outDir.mkdirs()

  type StreakExample = Example[Int, Array[Post]]

  def findSequences(array: Array[Post]) = {

    // ugh
    val result = ArrayBuffer[StreakExample]()

    var lastMood = -1
    var streak = 0
    var lastStart = -1
    var i = 0
    while(i < array.length) {
      val post = array(i)
      if(post.label != -1 && post.label < Moods.moods.size && Moods.coarseMoodArray(post.label) != Other.id) {
        val coarseMood = Moods.coarseMoodArray(post.label)
        if(coarseMood == lastMood) {
         streak += 1
        } else {
          lastMood = coarseMood
          lastStart = i-1
          streak = 0
        }

        if(streak == LEN_SAD && coarseMood != Moods.Dummy.id && coarseMood != Moods.Other.id) {
          val slice = array.slice(math.max(i - LEN_SAD - LEN_CONTEXT, 0), i - LEN_SAD)
          val coarseMoods = slice.map(_.label).map(i => if(i < 0 || i > Moods.coarseMoodArray.length) Moods.Dummy.id else Moods.coarseMoodArray(i))
          if(coarseMoods.count(_ == coarseMood) < LEN_CONTEXT/2 + 1) {
            result += Example(coarseMood, slice, post.id)
          }
        }
      }
      i += 1
    }

    result.toArray



  }

  for(file <- args.map(new File(_))) {
    println(file.getName)
    val seqs = {for(posts <- scalanlp.util.readObject[Array[Array[Post]]](file).par;
        seq: BuildDataset.StreakExample <- findSequences(posts)) yield {
      seq
    }}.toArray

    if(seqs.length > 0) {
      println(seqs.length, seqs.count(_.label == Moods.Sad.id))
      val out = new File(outDir,file.getName)
      scalanlp.util.writeObject(out, seqs)
    }
  }

}

object TestDataset extends App {
  scalanlp.util.logging.ConfiguredLogging.configuration = Configuration.fromMap(Map("level" -> "ERROR"))
  val numFolds = 10
  import BuildDataset.StreakExample
  val numIters = args(0).toInt
  val index = Index[String]()
   for(line <- Source.fromFile("index.txt").getLines()) index.index(line.trim)
  val intercept = index.index("INTERCEPT")
  for(m <- Moods.moods) {
    index.index("MOOD_" + m)
  }
  val examples = {for(file <- args.drop(1).map(new File(_)).iterator; ex <- scalanlp.util.readObject[Array[StreakExample]](file).par if ex.label <= 2) yield {
    val v = Encoder.fromIndex(index).mkSparseVector()
    for(datum <- ex.features; w <- BuildWordCounts.tokens(datum); i = index(w) if i != -1 && i < 8000) v(i) += 1
    for(datum <- ex.features if datum.label != -1 && datum.label < Moods.moods.size) v(index("MOOD_" + Moods.moods.get(datum.label))) += 1
    v(intercept) = 1
    Example(ex.label, v, ex.id)
  }}.toArray

  println(Counter.count(examples.map(_.label):_*))

  val result = Datasets.crossValidate(numFolds, examples) { (train, test) =>
//    val nb  = new SVM.Pegasos[Int,SparseVector[Double]](5) train(train)
    println(test.size, train.size)
      val nb  = new LogisticClassifier.Trainer[Int,SparseVector[Double]](OptParams(useStochastic=true,maxIterations=numIters,batchSize=1000,useL1=false,regularization=1.0)) train(train)
//      val nb  = new NaiveBayes(train)
      val stats = ContingencyStats(nb, test)
      println(stats)
      implicit val featureOrdering = new Ordering[((Int,Int),Double)] {
        def compare(x: ((Int, Int), Double), y: ((Int, Int), Double)) = x._2.abs.compare(y._2.abs)
      }
//            val beam = new Beam(50) ++= {nb.wordCounts(1).pairsIterator.map { case (f,v) => ((1,f), math.log( (v + nb.wordSmoothing) / (nb.wordCounts(2)(f) + nb.wordSmoothing)))}}
      val beam = new Beam(50) ++= {nb.featureWeights(1).pairsIterator.map { case (f,v) => ((1,f), v)}}
      for ( ((l,f:Int),v) <- beam.toIndexedSeq.sortBy( (kv: ((Int,Int),Double)) => -kv._2.abs)) {
        println(l + " " + index.get(f) + " " + v)
      }
      stats
  }

}


object BuildDataset2 extends App {
  val LEN_CONTEXT = 1

  val outDir = new File("processed/dataset2")
  outDir.mkdirs()

  type StreakExample = Example[Int, Array[Post]]

  def findSequences(arr: Array[Post]) = {
    val array = arr.reverse

    // ugh
    val result = ArrayBuffer[StreakExample]()

    var i = 0
    var streak = Long.MaxValue
    while(i < array.length-1) {
      val post = array(i)
      if(post.label != -1 && post.label < Moods.moods.size && Moods.coarseMoodArray(post.label) != Other.id) {
        val coarseMood = Moods.coarseMoodArray(post.label)
        if(post.time - streak >= 24 * 60 * 60 * 7) {
          val slice = array.slice(math.max(i - LEN_CONTEXT, 0), i)
          result += Example(coarseMood, slice, post.id)
        }
        if(coarseMood == Moods.Sad.id) {
          streak = math.min(streak,post.time)
        } else {
          streak = Long.MaxValue
        }
      }
      i += 1
    }

    result.toArray
  }

  for(file <- args.map(new File(_))) {
    println(file.getName)
    val seqs = {for(posts <- scalanlp.util.readObject[Array[Array[Post]]](file).par;
        seq: BuildDataset.StreakExample <- findSequences(posts)) yield {
      seq
    }}.toArray

    if(seqs.length > 0) {
      println(seqs.length, seqs.count(_.label == Moods.Sad.id))
      val out = new File(outDir,file.getName)
      scalanlp.util.writeObject(out, seqs)
    }
  }

}

object CollectStats extends App {
  val vs = for(file <- args.map(new File(_))) yield {
    println(file.getName)
    val vectors = {for(posts <- scalanlp.util.readObject[Array[Array[Post]]](file).par) yield {
      DenseVector(1, posts.length, posts.map(_.mood).count(m => m >= 0 && m < 134 && Moods.coarseMoodArray(m) == Moods.Happy.id), posts.map(_.mood).count(m => m >= 0 && m < 134 && Moods.coarseMoodArray(m) != Moods.Other.id))
    }}.toArray

    vectors.reduceLeft(_ + _)
  }
  println(vs.reduceLeft(_+ _))

}

object LenSad extends App {
  val LEN_SAD = 7
  val LEN_CONTEXT = 5

  def findSequences(array: Array[Post]) = {

    // ugh
    val result = ArrayBuffer[Int]()

    var i = 0
    while(i < array.length) {
      val post = array(i)
      if(post.label != -1 && post.label < Moods.moods.size && Moods.coarseMoodArray(post.label) != Other.id) {
        val coarseMood = Moods.coarseMoodArray(post.label)
        if(coarseMood == Moods.Sad.id) {
          var streak = 1
          var j = i + 1
          var allSad = true
          while(j < array.length && allSad) {
            val post2 = array(j)
            if(post2.label != -1 && post2.label < Moods.moods.size && Moods.coarseMoodArray(post2.label) != Other.id) {
              val coarseMood2 = Moods.coarseMoodArray(post2.label)
              allSad &&= coarseMood2 == coarseMood
              if(allSad) streak += 1
            }
            j += 1
          }
          result += streak
          i = j
        }
      }
      i += 1
    }

    result.toArray
  }

  val fullResults = ArrayBuffer[Int]()
  for(file <- args.map(new File(_))) {
    println(file.getName)
    val seqs = {for(posts <- scalanlp.util.readObject[Array[Array[Post]]](file).par;
        seq <- findSequences(posts)) yield {
      seq
    }}.toArray

    fullResults ++= seqs
  }

  val out = new PrintStream(new BufferedOutputStream(new FileOutputStream("text")))
  for(i <- fullResults) {
    out.println(i)
  }
  out.close()



}

object BuildDataset3 extends App {
  val LEN_CONTEXT = 1

  val outDir = new File("processed/dataset4")
  outDir.mkdirs()

  type StreakExample = Example[Int, Array[Post]]

  def findSequences(arr: Array[Post]) = {
    val array = arr.reverse

    // ugh
    val result = ArrayBuffer[StreakExample]()

    val INIT_SAD = 10

    var i = 0
    var sadRemaining = INIT_SAD
    while(i < array.length-1) {
      val post = array(i)
      val coarseMood = post.mood match {
        case -1 => -1
        case x if x >= Moods.moods.size => -1
        case m => Moods.coarseMoodArray(m)
      }
      if(coarseMood != -1 && coarseMood != Moods.Other.id) {
        if(coarseMood == Moods.Sad.id) {
          sadRemaining -= 1
        } else {
          sadRemaining = INIT_SAD
        }
      }
      if(sadRemaining < 0) {
        val endTime = array(i - INIT_SAD).time + 7 * 24 * 60 * 60
        val streak = array(i) +: array.iterator.drop(i).takeWhile(_.time < endTime).toArray
        val coarseMoods = streak.map(_.mood).collect{
          case -1 => -1
          case x if x > Moods.moods.size =>
          case m => Moods.coarseMoodArray(m)
        }
        val clss = if (coarseMoods.count(_ == Moods.Sad.id) > coarseMoods.size / 2) Moods.Sad.id else Moods.Happy.id
        val slice = array.slice(math.max(i - INIT_SAD/2, 0), i+1)
//        val slice = streak
        result += Example(clss, slice, post.id)
      }
      i += 1
    }

    result.toArray
  }

  for(file <- args.map(new File(_))) {
    println(file.getName)
    val seqs = {for(posts <- scalanlp.util.readObject[Array[Array[Post]]](file).par;
        seq: BuildDataset.StreakExample <- findSequences(posts)) yield {
      seq
    }}.toArray

    if(seqs.length > 0) {
      println(seqs.length, seqs.count(_.label == Moods.Sad.id))
      val out = new File(outDir,file.getName)
      scalanlp.util.writeObject(out, seqs)
    }
  }

}