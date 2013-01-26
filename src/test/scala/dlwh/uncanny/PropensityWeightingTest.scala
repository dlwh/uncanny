package dlwh.uncanny

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import breeze.linalg.DenseVector
import breeze.numerics._

/**
 * 
 * @author dlwh
 */
class PropensityWeightingTest extends FunSuite with Checkers {
  test("Random data with the same outcome always should not depend on treatment") {
    val features:Array[DenseVector[Double]] = Array.fill(6000)(DenseVector.rand(10) * 2.0 - 1.0)
    val outcomes = Array.fill(features.size)(true)
    val treated = Array.fill(features.size)(math.random > 0.5)

    val propensity = PropensityWeighting.estimate(features,  treated, outcomes)
    println("same outcome" + propensity)
    assert(propensity.abs < 3E-2, propensity)

  }

  test("Random data where treatment is perfectly predictive of outcome yields high score") {
    val features:Array[DenseVector[Double]] = Array.fill(6000)(DenseVector.rand(10) * 2.0 - 1.0)
    val weights:DenseVector[Double] = DenseVector.rand(10) * 2.0 - 1.0
    val treated = features.map(f => sigmoid(-(weights dot f)) > 0.5)

    val propensity = PropensityWeighting.estimate(features,  treated, treated)
    println("perfectly predictive" + propensity)
    assert(propensity > 0.45, propensity)
  }

  test("Random data where treatment is perfectly predictive of outcome yields high score, round 2") {
    val features:Array[DenseVector[Double]] = Array.fill(6000)(DenseVector.rand(1) * 2.0 - 1.0)
    val treated = Array.fill(features.size)(math.random > 0.5)
    val propensity = PropensityWeighting.estimate(features,  treated, treated)
    println("perfectly predictive, 2" + propensity)
    assert(propensity > 0.99, propensity)
  }

}
