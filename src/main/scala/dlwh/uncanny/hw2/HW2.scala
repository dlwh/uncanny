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
import scalanlp.data.Datasets
import scalanlp.optimize.{RandomizedGradientCheckingFunction, CachedBatchDiffFunction, BatchDiffFunction, DiffFunction}
import scalanlp.util.logging.ConfiguredLogging
import scalanlp.stats.ContingencyStats
import scalala.library.Library
import scalanlp.collection.mutable.Beam

/**
 * 
 * @author dlwh
 */
object HW2 extends App {
  case class Params(opt: OptParams, tokens: File, dict: File, l2Obj: Boolean = true, output: File = null)
  val config = CommandLineParser.parseArguments(args)._1
  ConfiguredLogging.configuration = config
  val params = config.readIn[Params]("")
  println(params)
  println("Reading index...")
  val (index,counts) = IndexCounts.readIndexAndCounts(params.dict,minCount=5)

  val f = new Filters(index,counts)
  import f._
  println("Processing reviews")
  val reviews = new Reviews(params.tokens,index)
  val fixed = reviews.iterator.map(removeRating andThen topNNonStopWords(40000)).toIndexedSeq
  println("Optimizing...")

  val featureSize = fixed.iterator.flatMap(_.tokens).max + 1
  val norm = if(params.l2Obj) 2.0 else 1.0
  val cv = Datasets.crossValidate(10,fixed){ def dotProduct(toks: scala.IndexedSeq[Int], weights: Array[Double]): Double = {
    var score = 0.0
    var i = 0
    while (i < toks.length) {
      score += weights(toks(i))
      i += 1
    }
    score
  }
    (train,test) =>
    val obj = new BatchDiffFunction[DenseVector[Double]] {
      def fullRange = 0 until train.size

      def computeSignedError(toks: scala.IndexedSeq[Int], weights: Array[Double], target: Double): Double = {
        var score: Double = dotProduct(toks, weights)
        val contribution = score - target
        contribution
      }

      def computeScoreAndCounts(toks: IndexedSeq[Int], weights: Array[Double], target: Double, counts: DenseVector[Double]) = {
        val contribution: Double = computeSignedError(toks, weights, target)
        val loss = math.pow(math.abs(contribution),norm)/norm
        if(norm == 2.0) {
          var i = 0
          for(t <- toks) {
            counts(t) += contribution
          }
        } else {
          var i = 0
          while(i < toks.length) {
            counts(toks(i)) += math.signum(contribution)
            i += 1
          }
        }
        loss
      }

      def testLoss(x: DenseVector[Double]) = {
        val w = x.data
        test.par.map(ex => math.pow(computeSignedError(ex.tokens,w,ex.rating),2)).reduceLeft(_ + _) / test.size
      }

      def calculate(x: DenseVector[Double], batch: IndexedSeq[Int]) = {
        val (loss,gradient) = batch.map(train).par.aggregate( (0.0,null:DenseVector[Double]))({ (stats,r) =>
          val (loss,dvx) = stats
          val counts = if(dvx eq null) DenseVector.zeros[Double](featureSize) else dvx
          val myLoss = computeScoreAndCounts(r.tokens,x.data,r.rating,counts)
          (loss + myLoss) -> counts
        }, { (a,b) => if(a._2 eq null) b else (a._1 + b._1, a._2 += b._2)})
        (loss/batch.size,gradient/=batch.size)
      }

    }

    val cached = new CachedBatchDiffFunction(obj)
    var numEvaluated = 0
    var placeInTrainingSet = 0
    var runningAverage = 0.0
    val numPerIteration = if(params.opt.useStochastic) params.opt.batchSize else train.size
    var n = 0
    val testSetEval = new ArrayBuffer[Double]
    var testSetClassificationStats: ContingencyStats[Boolean] = null
    val maxPasses = if(params.opt.maxIterations < 0) 25 else params.opt.maxIterations
    val maxIterations = if(params.opt.useStochastic) maxPasses * (train.size + params.opt.batchSize) / params.opt.batchSize else maxPasses
    var theta: DenseVector[Double] = null
    for( state <- params.opt.iterations(cached, DenseVector.zeros[Double](featureSize)).takeWhile(_.grad.norm(2) > 1E-7).take(maxIterations)) {
      n += 1
      runningAverage = (runningAverage * 9 + state.value)/10
      numEvaluated += numPerIteration
      placeInTrainingSet += numPerIteration
      if(placeInTrainingSet >= train.size) {
        val pass = numEvaluated / train.size
        testSetEval += math.sqrt(obj.testLoss(state.x))

        println("Test Loss!" + testSetEval.last)
        placeInTrainingSet -= train.size
      }
      theta = state.x
      println(state.value +" " + runningAverage)
    }
    testSetClassificationStats = ContingencyStats(test.map(_.rating > 3),test.map(ex => dotProduct(ex.tokens,theta.data) > 3))
    println("Finished: " + testSetEval)
    (testSetEval,theta,testSetClassificationStats)
  }
  if(params.output != null) {
    for( (run: (ArrayBuffer[Double], DenseVector[Double], ContingencyStats[Boolean]),i) <- cv.zipWithIndex) {
      val strm = new PrintStream(new FileOutputStream(new File(params.output + "-run-"+i+".txt")))
      for(x <- run._1) strm.println(x)
      strm.close()
      val strm2 = new PrintStream(new FileOutputStream(new File(params.output + "-eval-"+i+".txt")))
      val x = run._3
      strm2.println(x.macroaveraged.precision + "\t" + x.macroaveraged.recall + "\t" + x.macroaveraged.f
        + "\t" + x.microaveraged.precision + "\t" + x.microaveraged.recall + "\t" + x.microaveraged.f
      )

      strm2.close()
    }
  }
  println("All done!" + cv.map(_._1.last).sum/cv.length)
  println("All done!" + cv.map(_._3.macroaveraged.f).sum/cv.length)
  val sum = cv.map(_._2).reduceLeft(_ += _)
  val beam = new Beam[(Int, Double)](20)(Ordering[Double].on(_._2.abs))
  println("Top Weights:")
  beam foreach { case (i,v) =>
    println(index.get(i) + " " + v)
  }

  val sparsity = cv.last._2.valuesIterator.count(_ == 0.0)
  val sparsityKinda = cv.last._2.valuesIterator.count(_.abs < 1E-6)
  println("Sparsity: " + sparsity + " " + sparsityKinda)

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