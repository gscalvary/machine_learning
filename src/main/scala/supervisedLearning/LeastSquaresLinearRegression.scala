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
		
		try {
			val wls = inv(observationsWithOffsetsTraspose * observationsWithOffsets.get) * observationsWithOffsetsTraspose * labels
			return Some(wls)
		} catch {
			case singular: breeze.linalg.MatrixSingularException => singular.printStackTrace()
		}
		
		None
	}
	
	private def computeObservationsWithOffsets(): Option[DenseMatrix[Double]] = {
		
		if (observations.cols < observations.rows + 1) {
			return None
		}
		
		val owo = DenseMatrix.zeros[Double](observations.rows + 1, observations.cols)
		
		for (i <- 0 until observations.cols) {
			owo(::, i) := computeObservationWithOffset(observations(::,i), offset)
		}
		
		Some(owo)
	}
	
	private def computeObservationWithOffset(observation: DenseVector[Double], offset: Double): DenseVector[Double] = {
		
		val columnArray = new Array[Double](observation.length + 1)
		columnArray(0) = offset
		
		for (i <- 0 until observation.length) {
			columnArray(i + 1) = observation(i)
		}
		
		DenseVector(columnArray)
	}
	
	// Private Fields
	private val observationsWithOffsets: Option[DenseMatrix[Double]] = computeObservationsWithOffsets()
	private val freeParameters: Option[DenseVector[Double]] = computeFreeParameters()
}
