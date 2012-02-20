package dlwh.uncanny.hw1

import java.io.File
import io.Source
import scalanlp.data.Example
import scalanlp.util.{Encoder, Index}
import scalala.tensor.dense.{DenseVector, DenseMatrix}
import scalala.library.Library
import scalala.tensor.sparse.SparseVector
import scalanlp.classify.{Classifier, NaiveBayes}
import scalanlp.stats.ContingencyStats
import scalala.tensor.{Counter2, Counter, ::}
import collection.immutable.BitSet

/**
 * 
 * @author dlwh
 */

object HW1 extends App {
  val path = new File(args(0))

  // read data:
  val negative = for {
    file <- new File(path,"neg").listFiles
    toks = Source.fromFile(file).mkString.split("\\s") // data's already tokenized!
  } yield Example(0,toks.toIndexedSeq,file.getName)

  val positive = for {
    file <- new File(path,"pos").listFiles
    toks = Source.fromFile(file).mkString.split("\\s") // data's already tokenized!
  } yield Example(1,toks.toIndexedSeq,file.getName)

  val all = positive ++ negative
  val asCounts:IndexedSeq[Example[Int,Counter[String,Double]]] = for(ex <- all) yield ex.map(Counter.count(_:_*) * 1.0)
  println(asCounts.map(_.features("horizon")).sum)
  val index = Index{for(ex <- asCounts; tok <- ex.features.keysIterator) yield tok}
  val encoder = Encoder.fromIndex(index)
  val asVectors = for(ex <- asCounts) yield ex.map(encoder.encodeSparse _)
  val sections = asVectors.groupBy(_.id.take(3)) // cvX<id>.txt

  // cvs
  for(testID <- sections.keys) {
    println("CV with " + testID + " as test.")
    val training = (sections - testID).values.flatten
    // I'd just use my own NB classifier here, but meh
    val counts = DenseMatrix.zeros[Double](2,index.size) + 0.1 // smoothing
    val classCounts = DenseVector.zeros[Double](2)
    for (ex <- training) {
      classCounts(ex.label) += 1
      counts(ex.label,::) += ex.features
    }

    val mi = FeatureSelection.mutualInformation(counts)
    val toZero = BitSet() ++ Library.sum(counts,Library.Axis.Horizontal).argsort.take(2500)
    println(mi.argsort.reverse.take(50).map(index.get _))

    val normed = Library.logAndNormalizeRows(counts)
    val normClass = Library.log(classCounts)
    val classifier = new Classifier[Int,SparseVector[Double]] {
      def scores(o: SparseVector[Double]) = {
        // yuck, i need to get the matrix multply here working
         (normed * DenseVector.tabulate(index.size)(i => if(toZero.contains(i)) 0.0 else o(i)) + normClass)
      }
    }

    println(ContingencyStats(classifier,sections(testID)))

  }


}

object FeatureSelection {
  def mutualInformation(jointCounts: DenseMatrix[Double]) = {
    import Library._
    val classMarginals:DenseVector[Double] = logAndNormalize(sum(jointCounts,Axis.Vertical))
    val featureMarginals:DenseVector[Double] = logAndNormalize(sum(jointCounts,Axis.Horizontal))
    val jointProb:DenseMatrix[Double] = (jointCounts) / jointCounts.sum
    val mi = DenseVector.zeros[Double](jointCounts.numCols)
    for( (k1,k2,v) <- jointCounts.triplesIterator) {
      mi(k2) += jointProb(k1,k2) * (math.log(jointProb(k1,k2)) - classMarginals(k1) - featureMarginals(k2))
    }
    mi

  }
}