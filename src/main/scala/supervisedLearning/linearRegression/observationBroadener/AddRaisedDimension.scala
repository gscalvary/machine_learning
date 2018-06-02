package supervisedLearning.linearRegression.observationBroadener
import breeze.linalg.DenseVector

object AddRaisedDimension extends CreatesObservationDimension {
	
	/**
	  *
	  * @param index a 0-based index used to determine which dimension of the observation should be used as a basis for
	  *              the new dimension
	  * @param observation an observation to which we would like to add a dimension
	  * @param power the power to which we would like to raise a given dimension to create a new dimension
	  * @return an observation
	  */
	override def getExtendedObservation(index: Int, observation: DenseVector[Double], power:Double*): DenseVector[Double] = {
		
		if (super.isIndexOutOfBounds(index, observation)) {
			return observation
		}
		
		if (!power.length.equals(1)) {
			return observation
		}
		
		val newDimensionValue = scala.math.pow(observation(index), power(0))
		super.extendObservation(observation, newDimensionValue)
	}
}
