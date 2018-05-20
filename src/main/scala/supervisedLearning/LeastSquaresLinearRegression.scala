package supervisedLearning

import breeze.linalg.{DenseMatrix, DenseVector, Transpose, inv}

import scala.util.Try

/**
  * Given a set of observations and a label associated with each, compute a hyperplane through the observation
  * dimensions such that the sum of the squares of the difference between each observation and its closest point on the
  * hyperplane is minimized.  This works best if there are many more observations than dimensions to the problem.
  *
  * @param observations - observed data points from some problem domain
  * @param labels - labels associated with each observation
  */
class LeastSquaresLinearRegression(val observations: DenseMatrix[Double], val labels: DenseVector[Double]) {
	
	// Public Methods
	/**
	  * Given an observation gathered in the same way as the observations generated in the training set, compute a
	  * label.
	  *
	  * @param observation - some observed datapoint
	  * @return a label for the observation
	  */
	def getLabel(observation: DenseVector[Double]): Option[Double] = {
		
		if (weights.isEmpty) {
			return None
		}
		
		if (observation.length != weights.get.length - 1) {
			return None
		}
		
		Some(computeObservationWithOffset(observation.t, LeastSquaresLinearRegression.OFFSET) dot weights.get)
	}
	
	// Private Methods
	private def computeWeights(): Option[DenseVector[Double]] = {
		
		if (observationsWithOffsets.isEmpty) {
			return None
		}
		
		val observationsWithOffsetsTranspose = observationsWithOffsets.get.t
		val invertedObservations = invertMatrix(observationsWithOffsetsTranspose * observationsWithOffsets.get) getOrElse (return None)
		val wls = invertedObservations * observationsWithOffsetsTranspose * labels
		
		Some(wls)
	}
	
	private def invertMatrix(matrix: DenseMatrix[Double]): Try[DenseMatrix[Double]] = {
		Try(inv(matrix))
	}
	
	private def computeObservationsWithOffsets(): Option[DenseMatrix[Double]] = {
		
		if (observations.rows < observations.cols + 1) {
			return None
		}
		
		val owo = DenseMatrix.zeros[Double](observations.rows, observations.cols + 1)
		
		for (i <- 0 until observations.rows) {
			owo(i, ::) := computeObservationWithOffset(observations(i,::), LeastSquaresLinearRegression.OFFSET).t
		}
		
		Some(owo)
	}
	
	private def computeObservationWithOffset(observation: Transpose[DenseVector[Double]], offset: Double): DenseVector[Double] = {
		
		val row = new Array[Double](observation.t.length + 1)
		row(0) = offset
		
		for (i <- 0 until observation.t.length) {
			row(i + 1) = observation(i)
		}
		
		DenseVector(row)
	}
	
	// Private Fields
	private val observationsWithOffsets: Option[DenseMatrix[Double]] = computeObservationsWithOffsets()
	private val weights: Option[DenseVector[Double]] = computeWeights()
}

object LeastSquaresLinearRegression
{
	val OFFSET: Double = 1
}