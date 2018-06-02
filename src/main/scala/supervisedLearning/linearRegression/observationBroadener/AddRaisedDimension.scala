package supervisedLearning.linearRegression.observationBroadener
import breeze.linalg.DenseVector

object AddRaisedDimension extends CreatesObservationDimension {
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
