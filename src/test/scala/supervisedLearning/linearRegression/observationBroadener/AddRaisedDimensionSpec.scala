package supervisedLearning.linearRegression.observationBroadener

import breeze.linalg.DenseVector
import org.scalatest.FunSuite

class AddRaisedDimensionSpec extends FunSuite {
	
	val power: Double = 2.0
	
	test("If the index argument is less than zero the observation is not modified.") {
		
		val length: Int = 3
		val observation: DenseVector[Double] = DenseVector.zeros(length)
		val index: Int = -1
		val addRaisedDimension = new AddRaisedDimension(index, power)
		val newObservation: DenseVector[Double] = addRaisedDimension.getExtendedObservation(observation)
		
		assert(newObservation.length == length)
	}
	
	test("If the index argument is greater than or equal to the length of the observation then the observation is not modified.") {
		
		val length: Int = 3
		val observation: DenseVector[Double] = DenseVector.zeros(length)
		val index: Int = 3
		val addRaisedDimension = new AddRaisedDimension(index, power)
		val newObservation: DenseVector[Double] = addRaisedDimension.getExtendedObservation(observation)
		
		assert(newObservation.length == length)
	}
	
	test("Observations are extended correctly.") {
		
		val length: Int = 1
		val observation: DenseVector[Double] = DenseVector.fill(length){2.0}
		val index: Int = 0
		val addRaisedDimension = new AddRaisedDimension(index, power)
		val newObservation: DenseVector[Double] = addRaisedDimension.getExtendedObservation(observation)
		
		assert(newObservation.length == length + 1)
		assert(newObservation(0) == 2.0)
		assert(newObservation(1) == 4.0)
	}
}
