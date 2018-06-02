package supervisedLearning.linearRegression

import breeze.linalg.{DenseMatrix, DenseVector}
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.FunSuite
import supervisedLearning.linearRegression.observationBroadener.{AddRaisedDimension, ObservationDimensionsExtender}

class LeastSquaresLinearRegressionSpec extends FunSuite {
	
	val tolerance = 1e-10f
	implicit val doubleEq: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(tolerance)
	
	test("Labels are not returned if there are less observations than features.") {
		
		val observations: DenseMatrix[Double] = DenseMatrix.zeros(5, 6)
		val labels: DenseVector[Double] = DenseVector.zeros(5)
		val linearLeastSquaresRegressor: LeastSquaresLinearRegression = new LeastSquaresLinearRegression(observations, labels)
		
		assert(linearLeastSquaresRegressor.getLabel(DenseVector.zeros(5)).isEmpty)
	}
	
	test("Labels are not returned if the observations with offsets matrix is not full rank.") {
		
		val observations: DenseMatrix[Double] = DenseMatrix.zeros(6, 5)
		val labels: DenseVector[Double] = DenseVector.zeros(5)
		val linearLeastSquaresRegressor: LeastSquaresLinearRegression = new LeastSquaresLinearRegression(observations, labels)
		
		assert(linearLeastSquaresRegressor.getLabel(DenseVector.zeros(5)).isEmpty)
	}
	
	test("Labels are not returned if the new observation is not of the same dimensions as the training set.") {
		
		val observations: DenseMatrix[Double] = DenseMatrix(1.0, 2.0, 3.0, 4.0, 5.0)
		val labels: DenseVector[Double] = DenseVector(2.0, 4.0, 5.0, 4.0, 5.0)
		val linearLeastSquaresRegressor: LeastSquaresLinearRegression = new LeastSquaresLinearRegression(observations, labels)
		
		assert(linearLeastSquaresRegressor.getLabel(DenseVector.zeros(6)).isEmpty)
	}
	
	
	test("Labels are computed correctly for single dimension input.") {
		
		val observations: DenseMatrix[Double] = DenseMatrix(1.0, 2.0, 3.0, 4.0, 5.0)
		val labels: DenseVector[Double] = DenseVector(2.0, 4.0, 5.0, 4.0, 5.0)
		val linearLeastSquaresRegressor: LeastSquaresLinearRegression = new LeastSquaresLinearRegression(observations, labels)
		
		assert(linearLeastSquaresRegressor.getLabel(DenseVector(3.0)).get === 4.0)
	}
	
	test("Labels are computed correctly for multi-dimensional input.") {

		val observations: DenseMatrix[Double] = DenseMatrix((12.0, 7.0),(6.0, 2.0),(5.0, 1.0))
		val labels: DenseVector[Double] = DenseVector(10.5, 4.0, 3.0)
		val linearLeastSquaresRegressor: LeastSquaresLinearRegression = new LeastSquaresLinearRegression(observations, labels)

		assert(linearLeastSquaresRegressor.getLabel(DenseVector(8.0, 2.0)).get === 7.0)
	}
	
	test("Labels are computed correctly for input with a broadener.") {
		
		val observations: DenseMatrix[Double] = DenseMatrix(0.0, 1.0, 2.0, 3.0, 4.0)
		val labels: DenseVector[Double] = DenseVector(0.0, 1.0, 4.0, 9.0, 16.0)
		val broadeners: List[ObservationDimensionsExtender] = List(new AddRaisedDimension(0, 2))
		val linearLeastSquaresRegressor: LeastSquaresLinearRegression = new LeastSquaresLinearRegression(observations, labels, broadeners)
		
		assert(linearLeastSquaresRegressor.getLabel(DenseVector(3.0)).get === 9.0)
	}
}
