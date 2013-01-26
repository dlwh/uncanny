package dlwh.uncanny

import breeze.data.Example
import breeze.classify.LogisticClassifier
import breeze.linalg._
import breeze.numerics._
import breeze.optimize.FirstOrderMinimizer.OptParams

/**
 * 
 * @author dlwh
 */
object PropensityWeighting {
  def estimate(features: Array[DenseVector[Double]],
               treatment: Array[Boolean],
               outcome: Array[Boolean]) = {
    val treatment_examples = for( (t,f) <- treatment zip features) yield Example(t, f)
    val propClass = new LogisticClassifier.Trainer[Boolean, DenseVector[Double]](OptParams(regularization=0.0)).train(treatment_examples)
    val propensities = features.map(f => exp(logNormalize(propClass.scores(f))) apply (true))

    val treated_examples = for( ((t,f),o) <- treatment zip features zip outcome if t) yield Example(o, f)
    val treatedEffClass = new LogisticClassifier.Trainer[Boolean, DenseVector[Double]](OptParams(regularization=0.0)).train(treated_examples)
    val untreated_examples = for( ((t,f),o) <- treatment zip features zip outcome if !t) yield Example(o, f)
    val untreatedEffClass = new LogisticClassifier.Trainer[Boolean, DenseVector[Double]](OptParams(regularization=0.0)).train(untreated_examples)
    
    val treated_outcomes = features.map(f => exp(logNormalize(treatedEffClass.scores(f))) apply (true))
    val untreated_outcomes = features.map(f => exp(logNormalize(untreatedEffClass.scores(f))) apply (true))

    val treated = for(i <- Array.range(0, features.length)) yield {
      if(propensities(i) == 0.0) 0.0
     else {
        val score= (I(treatment(i) && outcome(i)) - (I(treatment(i)) - propensities(i)) * treated_outcomes(i)) / propensities(i)
        assert(!score.isNaN,propensities(i) + " " + treated_outcomes(i) + " "+ propClass.scores(features(i)))
        score
      }
    }

    val untreated = for(i <- Array.range(0, features.length) ) yield {
      if(propensities(i) == 1.0) 0.0
      else {
        val score = (I(!treatment(i) && outcome(i)) - (I(treatment(i)) - propensities(i)) * untreated_outcomes(i)) / (1 - propensities(i))
        assert(!score.isNaN,propensities(i) + " " + treated_outcomes(i))
        score
      }
    }

    mean(treated) - mean(untreated)
  }

}
