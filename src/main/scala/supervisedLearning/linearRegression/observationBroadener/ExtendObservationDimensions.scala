package supervisedLearning.linearRegression.observationBroadener

import breeze.linalg.DenseVector

abstract class ExtendObservationDimensions(val index: Int) {
	
	/**
	  * The lone abstract method that needs to be implemented by an extending class.
	  * Given an observation add a new dimension to the observation created from the application
	  * of some function to the dimension of the input observation identified by the index.
	  *
	  * @param observation an observation to which we would like to add a dimension
	  * @return an observation extended by the addition of a new dimension
	  */
	def getExtendedObservation(observation: DenseVector[Double]): DenseVector[Double]
	
	/**
	  * Given an observation determine if the index is out of bounds.
	  *
	  * @param observation an observation to which we would like to add a dimension
	  * @return whether or not the index is out of bounds for the given observation
	  */
	def isIndexOutOfBounds(observation: DenseVector[Double]): Boolean = {
		index < 0 || index >= observation.length
	}
	
	/**
	  * Given an observation return the same observation extended by one dimension.
	  *
	  * @param observation to extend
	  * @param newDimensionValue the value of the new dimension
	  * @return extended observation
	  */
	def extendObservation(observation: DenseVector[Double], newDimensionValue: Double): DenseVector[Double] = {
		val dimensions = new Array[Double](observation.length + 1)
		dimensions(observation.length) = newDimensionValue
		
		for (i <- 0 until observation.length) {
			dimensions(i) = observation(i)
		}
		
		DenseVector(dimensions)
	}
}
