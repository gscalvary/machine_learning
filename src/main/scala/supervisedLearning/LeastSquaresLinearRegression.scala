package supervisedLearning

import breeze.linalg.{DenseMatrix, DenseVector, inv}

class LeastSquaresLinearRegression(val observations: DenseMatrix[Double], val labels: DenseVector[Double], val offset: Double = 0) {
	
	// Public Methods
	def getFreeParameters: DenseVector[Double] = {
		
		if (freeParameters == null) {
			observationsWithOffsets = computeObservationsWithOffsets()
			freeParameters = computeFreeParameters()
		}
		
		freeParameters
	}
	
	// Private Methods
	private def computeFreeParameters(): DenseVector[Double] = {
		
		val observationsWithOffsetsTraspose = observationsWithOffsets.t
		val wls = inv(observationsWithOffsetsTraspose *:* observationsWithOffsets) *:* observationsWithOffsetsTraspose * labels
		wls
	}
	
	private def computeObservationsWithOffsets(): DenseMatrix[Double] = {
		
		val owo = DenseMatrix.zeros[Double](observations.rows + 1, observations.cols)
		
		for (i <- 0 to observations.cols) {
			//TODO: fix me!
			owo(::, i) := DenseVector(offset, observations(::, i).toArray)
		}
		
		owo
	}
	
	// Private Fields
	private var observationsWithOffsets: DenseMatrix[Double] = _
	private var freeParameters: DenseVector[Double] = _
}
