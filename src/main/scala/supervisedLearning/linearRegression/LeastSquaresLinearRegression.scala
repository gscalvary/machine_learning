package supervisedLearning.linearRegression

import breeze.linalg.{DenseMatrix, DenseVector, inv}
import supervisedLearning.linearRegression.observationBroadener.ObservationDimensionsExtender

import scala.util.Try

/**
  * Given a set of observations and a label associated with each, compute a hyperplane through the observation
  * dimensions such that the sum of the squares of the difference between each observation and its closest point on the
  * hyperplane is minimized.  This works best if there are many more observations than dimensions to the problem.
  *
  * @param observations - observed data points from some problem domain
  * @param labels - labels associated with each observation
  * @param broadeners - zero or more instances of classes that extend observations
  */
class LeastSquaresLinearRegression(val observations: DenseMatrix[Double], val labels: DenseVector[Double], val broadeners: List[ObservationDimensionsExtender]) {
	
	//Auxiliary Constructor
	def this(observations: DenseMatrix[Double], labels: DenseVector[Double]) {
		this(observations, labels, List.empty)
	}
	
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
		
		val broadenedObservation = broadenObservation(observation)
		
		if (broadenedObservation.length != weights.get.length - 1) {
			return None
		}
		
		Some(computeObservationWithOffset(broadenedObservation) dot weights.get)
	}
	
	// Private Methods
	/**
	  * Use the magic of linear algebra to create weighting factors for each dimension of the augmented observations
	  * plus some offset.
	  *
	  * @return the weights for our dimensions
	  */
	private def computeWeights(): Option[DenseVector[Double]] = {
		
		if (observationsWithOffsets.isEmpty) {
			return None
		}
		
		val observationsWithOffsetsTranspose = observationsWithOffsets.get.t
		val invertedObservations = invertMatrix(observationsWithOffsetsTranspose * observationsWithOffsets.get) getOrElse (return None)
		val wls = invertedObservations * observationsWithOffsetsTranspose * labels
		
		Some(wls)
	}
	
	/**
	  * Try to invert a matrix (it may be singular).
	  *
	  * @param matrix the matrix to be inverted
	  * @return a matrix or None depending upon whether or not the matrix is invertible
	  */
	private def invertMatrix(matrix: DenseMatrix[Double]): Try[DenseMatrix[Double]] = {
		Try(inv(matrix))
	}
	
	/**
	  * Broaden the raw observations using any available broadening functions.
	  *
	  * @return a matrix of broadened observations
	  */
	private def broadenObservations(): DenseMatrix[Double] = {
		
		if (broadeners.isEmpty) {
			return this.observations
		}
		
		val broadenedObservations = DenseMatrix.zeros[Double](this.observations.rows, this.observations.cols + 1)
		
		for (i <- 0 until this.observations.rows) {
			broadenedObservations(i, ::) := broadenObservation(this.observations(i, ::).t).t
		}
		
		broadenedObservations
	}
	
	/**
	  * Broaden a raw observation using any available broadening functions.
	  *
	  * @param observation the observation to be broadened
	  * @return the broadened observation
	  */
	private def broadenObservation(observation: DenseVector[Double]): DenseVector[Double] = {
		
		if (broadeners.isEmpty) {
			return observation
		}
		
		var broadenedObservation = observation
		
		for (i <- broadeners.indices) {
			broadenedObservation = broadeners(i).getExtendedObservation(broadenedObservation)
		}
		
		broadenedObservation
	}
	
	/**
	  * Add a static offset to the beginning of each observation.
	  *
	  * @param observations observations to be offset
	  * @return the offset observations
	  */
	private def computeObservationsWithOffsets(observations: DenseMatrix[Double]): Option[DenseMatrix[Double]] = {
		
		if (observations.rows < observations.cols + 1) {
			return None
		}
		
		val owo = DenseMatrix.zeros[Double](observations.rows, observations.cols + 1)
		
		for (i <- 0 until observations.rows) {
			owo(i, ::) := computeObservationWithOffset(observations(i,::).t).t
		}
		
		Some(owo)
	}
	
	/**
	  * Add a static offset to an observation.
	  *
	  * @param observation observation to be offset
	  * @return the offset observation
	  */
	private def computeObservationWithOffset(observation: DenseVector[Double]): DenseVector[Double] = {
		
		val row = new Array[Double](observation.length + 1)
		row(0) = LeastSquaresLinearRegression.OFFSET
		
		for (i <- 0 until observation.length) {
			row(i + 1) = observation(i)
		}
		
		DenseVector(row)
	}
	
	// Private Fields
	private val observationsWithOffsets: Option[DenseMatrix[Double]] = computeObservationsWithOffsets(broadenObservations())
	private val weights: Option[DenseVector[Double]] = computeWeights()
}

object LeastSquaresLinearRegression {
	/**
	  * Initial value of the offset or intercept.
	  */
	val OFFSET: Double = 1
}
