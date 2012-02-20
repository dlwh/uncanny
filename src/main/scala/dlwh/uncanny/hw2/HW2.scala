package dlwh.uncanny.hw2

import scalanlp.optimize.FirstOrderMinimizer.OptParams
import java.nio
import nio.channels.FileChannel
import nio.{ByteOrder, IntBuffer}
import scalanlp.config.CommandLineParser
import java.util.zip.GZIPInputStream
import collection.mutable.ArrayBuffer
import io.Source
import collection.immutable.BitSet
import scalanlp.util.{Iterators, Index}
import java.io._
import scalala.tensor.dense.DenseVector
import scalanlp.optimize.{CachedBatchDiffFunction, BatchDiffFunction, DiffFunction}

/**
 * 
 * @author dlwh
 */
object HW2 extends App {
  case class Params(opt: OptParams, tokens: File, dict: File, l2Obj: Boolean = true)
  val config = CommandLineParser.parseArguments(args)._1
  val params = config.readIn[Params]("")
  println(params)
  println("Reading index...")
  val (index,counts) = IndexCounts.readIndexAndCounts(params.dict,minCount=5)

  val f = new Filters(index,counts)
  import f._
  println("Processing reviews")
  val reviews = new Reviews(params.tokens,index)
  val fixed = reviews.iterator.map(removeRating andThen topNNonStopWords(10000)).toIndexedSeq
  println("Optimizing...")

  val featureSize = fixed.iterator.flatMap(_.tokens).max + 1
  val obj = new BatchDiffFunction[DenseVector[Double]] {
    def fullRange = 0 until fixed.size

    def computeScoreAndCounts(toks: IndexedSeq[Int], weights: DenseVector[Double], target: Double, counts: DenseVector[Double]) = {
      var score = 0.0
      for(t <- toks) {
        score += weights(t)
      }
      val contribution = score - target
      val loss = math.pow(contribution,2) * .5
      for(t <- toks) {
        counts(t) += contribution
      }
      loss
    }

    def calculate(x: DenseVector[Double], batch: IndexedSeq[Int]) = {
      batch.map(fixed).par.aggregate( (0.0,null:DenseVector[Double]))({ (stats,r) =>
        val (loss,dvx) = stats
        val counts = if(dvx eq null) DenseVector.zeros[Double](featureSize) else dvx
        val myLoss = computeScoreAndCounts(r.tokens,x,r.rating,counts)
        (myLoss -> counts)
      }, { (a,b) => if(a._2 eq null) b else (a._1 + b._1, a._2 += b._2)})
    }

  }

  val cached = new CachedBatchDiffFunction(obj)
  var runningAverage = 0.0
  var adjRunningAverage = 0.0
  var n = 0
  val avg = if(params.opt.useStochastic) params.opt.batchSize else 1
  for( state <- params.opt.iterations(cached, DenseVector.zeros[Double](featureSize)).take(params.opt.maxIterations)) {
    n += 1
    runningAverage += (state.value - runningAverage)/n
    adjRunningAverage += (state.adjustedValue - adjRunningAverage)/n
    println(state.value +" " + runningAverage/avg +" " + adjRunningAverage/avg)
  }
}

class Filters(index: Index[String], counts: Array[Double]) {
  def filterByCounts(minCount: Int) = {(rev: Review) =>
    rev.copy(tokens=rev.tokens.filter(i => counts(i) >= minCount))
  }

  val removeRating = {
    val ratingStart = index("<rating>")
    val ratingEnd = index("</rating>");
    {(rev: Review) =>
      val buf = new ArrayBuffer[Int]()
      var skipping = false
      for(t <- rev.tokens) {
        if(t == ratingStart) skipping = true
        else if(t == ratingEnd) skipping = false
        else if(!skipping) buf += t
      }
      rev.copy(tokens=buf)
    }
  }

  def filterByRank(maxRank: Int, exclude: Int=>Boolean) = {
    val topN = BitSet() ++ (0 until index.size).filterNot(exclude).take(maxRank);

    {(rev: Review) =>
      rev.copy(tokens=rev.tokens.filter(topN))
    }

  }

  def topNNonStopWords(maxRank: Int) = {
    val stoplist = BitSet() ++ (scalanlp.text.tokenize.StopWordFilter("en").words.map(index).filter(_ >= 0))
    filterByRank(maxRank,stoplist)
  }
}

case class Review(idx:Int, rating: Int, tokens: IndexedSeq[Int]) {
  def decode(index: Index[String]) = {
    tokens.map(index.get(_))
  }
}

object IndexCounts {
  def readIndexAndCounts(dict: File, minCount: Int = 2) = {
    val index = Index[String]()
    val counts = new ArrayBuffer[Double]()
    index.index("DUMMYXXXXXXXX")
    counts += 0
    for(Array(_,idx,count,token) <- Source.fromFile(dict).getLines().map(_.split("\t",4)).takeWhile(_(2).toInt >= minCount)) {
      val trueIdx = index.index(token)
      if(trueIdx % 20000 == 0) println(trueIdx)
      else if(trueIdx % 2000 == 0) print('.')
      assert(idx.toInt == trueIdx, " " + idx + " " + trueIdx)
      counts += count.toInt
    }
    (index,counts.toArray)
  }
}

class Reviews(tokensF: File, index: Index[String]) extends Iterable[Review] {
  val reviewToken = index("<review>")
  assert(reviewToken >= 0)
  val endReviewToken = index("</review>")
  assert(endReviewToken >= 0)
  val ratingToken = index("<rating>")

  private def swapInt(v: Int) = (v >>> 24) | (v << 24) | ((v << 8) & 0x00FF0000) | ((v >> 8) & 0x0000FF00)

  def iterator:Iterator[Review] = {
    val in = new DataInputStream(new BufferedInputStream(new FileInputStream(tokensF),1024 * 1024 * 4))
    var idx = 0
    Iterators.fromProducer {
      val tokens = new ArrayBuffer[Int]()
      try {
        var ok = true
        while(ok) {
          in.readInt()
          in.readInt()
          val tok = swapInt(in.readInt())
          ok = tok != endReviewToken
          if(tok < index.size)
            tokens += tok
        }
        val indexOfRating = tokens.indexOf(ratingToken)
        val rating = index.get(tokens(indexOfRating+1)).toInt
        idx += 1
        if(idx % 10000 == 0) println(idx)
        Some(new Review(idx-1, rating,tokens))
      } catch {
        case ex: EOFException => None
      }

    }
  }
}