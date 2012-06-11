package dlwh.uncanny.lj



/*
 Copyright 2009 David Hall, Daniel Ramage

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied/
 See the License for the specific language governing permissions and
 limitations under the License.
*/

import math._
import scalala.tensor.mutable._
import scalanlp.data._
import scalala.tensor.::
import scalala.tensor.sparse.SparseVector
import scalanlp.classify.{LFMatrix, Classifier}


/**Implements a Naive-Bayes Classifer over bags of words.
 * It automatically trains itself given the collection c of
 * learning examples.
 *
 * @author dlwh
 * @param c: a collection of example documents
 * @param wordSmoothing: how much smoothing for each word
 * @param classSmoothing: how much smoothing for the class.
 */
@SerialVersionUID(1L)
class NaiveBayes[L, W](c: Iterable[Example[L, SparseVector[Double]]],
                       val wordSmoothing: Double = 0.05,
                       val classSmoothing: Double = 0.00) extends Classifier[L, SparseVector[Double]] with Serializable {


  val wordCounts = new LFMatrix[L,SparseVector[Double]](SparseVector.zeros(c.head.features.size))
  val classCounts= Counter[L, Double]()
  val vocabSize = c.head.features.size
    for (e <- c) {
      classCounts(e.label) += 1
      wordCounts(e.label) += e.features
    }

  val wordTotals = {
    for (k <- classCounts.keysIterator) yield k -> wordCounts(k).sum
  } toMap

  /**Returns the unnormalized log probability of each class for the given document. */
  def scores(o: SparseVector[ Double]) = {
    val res = Counter[L, Double]()
    for (l <- classCounts.domain) {
      val prior = classCounts(l)
      res(l) += log(prior + classSmoothing)
      val probWC = wordCounts(l)
      val logDenom = log(wordTotals(l) + vocabSize * wordSmoothing)
      val logWordProbabilities = o.pairsIterator.map {
        case (k, v) => v * (log(probWC(k) + wordSmoothing) - logDenom)
      }
      res(l) += logWordProbabilities.sum
    }
    res
  }
}


