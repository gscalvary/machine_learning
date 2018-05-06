package supervisedLearning

import breeze.linalg.{DenseMatrix, DenseVector, inv}

class LeastSquaresLinearRegression(val observations: DenseMatrix[Double], val labels: DenseVector[Double], val offset: Double = 0) {
	
	// Public Methods
	def getLabel(observation: DenseVector[Double]): Option[Double] = {
		
		if (freeParameters.isEmpty) {
			return None
		}
		
		if (observation.length != freeParameters.get.length - 1) {
			return None
		}
		
		Some(computeObservationWithOffset(observation, 1) dot freeParameters.get)
	}
	
	// Private Methods
	private def computeFreeParameters(): Option[DenseVector[Double]] = {
		
		if (observationsWithOffsets.isEmpty) {
			return None
		}
		
		val observationsWithOffsetsTraspose = observationsWithOffsets.get.t
		val wls = inv(observationsWithOffsetsTraspose *:* observationsWithOffsets.get) *:* observationsWithOffsetsTraspose * labels
		
		Some(wls)
	}
	
	private def computeObservationsWithOffsets(): Option[DenseMatrix[Double]] = {
		
		if (observations.cols < observations.rows) {
			return None
		}
		
		val owo = DenseMatrix.zeros[Double](observations.rows + 1, observations.cols)
		
		for (i <- 0 to observations.cols) {
			owo(::, i) := computeObservationWithOffset(observations(::,i), offset)
		}
		
		Some(owo)
	}
	
	private def computeObservationWithOffset(observation: DenseVector[Double], firstElement: Double): DenseVector[Double] = {
		
		val columnArray = new Array[Double](observation.length + 1)
		columnArray(0) = firstElement
		
		for (i <- 0 to observation.length) {
			columnArray(i + 1) = observation(i)
		}
		
		DenseVector(columnArray)
	}
	
	// Private Fields
	private val observationsWithOffsets: Option[DenseMatrix[Double]] = computeObservationsWithOffsets()
	private val freeParameters: Option[DenseVector[Double]] = computeFreeParameters()
}
