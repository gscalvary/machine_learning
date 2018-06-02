package supervisedLearning.linearRegression.observationBroadener

import breeze.linalg.DenseVector

trait CreatesObservationDimension {
	/**
	  * The lone abstract method that needs to be implemented by an extending class.
	  * Given an observation and a 0-based index add a new dimension to the observation created from the application
	  * of some function to the dimension of the observation identified by the index.
	  *
	  * @param index a 0-based index used to determine which dimension of the observation should be used as a basis for
	  *              the new dimension
	  * @param observation an observation to which we would like to add a dimension
	  * @param numbers a variable argument that provides the implementing class or object a mechanism to pass function
	  *                variables for new dimension generation
	  * @return an observation
	  */
	def getExtendedObservation(index: Int, observation: DenseVector[Double], numbers:Double*): DenseVector[Double]
	
	/**
	  * Given an observation and a 0-based index determine if the index is out of bounds.
	  *
	  * @param index a 0-based index used to determine which dimension of the observation should be used as a basis for
	  *              the new dimension
	  * @param observation an observation to which we would like to add a dimension
	  * @return whether or not the index is out of bounds
	  */
	def isIndexOutOfBounds(index: Int, observation: DenseVector[Double]): Boolean = {
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
