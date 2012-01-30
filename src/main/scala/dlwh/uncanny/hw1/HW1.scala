package dlwh.uncanny.hw1

import java.io.File
import io.Source
import scalanlp.data.Example
import scalala.tensor.Counter
import scalanlp.util.{Encoder, Index}
import scalala.tensor.::
import scalala.tensor.dense.{DenseVector, DenseMatrix}
import scalala.library.Library
import scalala.tensor.sparse.SparseVector
import scalanlp.classify.{Classifier, NaiveBayes}
import scalanlp.stats.ContingencyStats

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

  println(positive(0),negative(0))

  val all = positive ++ negative
  val asCounts:IndexedSeq[Example[Int,Counter[String,Double]]] = for(ex <- all) yield ex.map(Counter.count(_:_*) * 1.0)
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

    val normed = Library.logAndNormalizeRows(counts)
    val normClass = Library.log(classCounts)
    val classifier = new Classifier[Int,SparseVector[Double]] {
      def scores(o: SparseVector[Double]) = {
        // yuck, i need to get the matrix multply here working
         (normed * DenseVector.tabulate(index.size)(o) + normClass)
      }
    }

    println(ContingencyStats(classifier,sections(testID)))

  }


}