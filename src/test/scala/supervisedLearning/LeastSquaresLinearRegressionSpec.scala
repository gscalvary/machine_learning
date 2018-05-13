package supervisedLearning

import breeze.linalg.{DenseMatrix, DenseVector}
import org.scalatest.FunSuite

class LeastSquaresLinearRegressionSpec extends FunSuite {
	
	test("Labels are not returned if there are less observations than features.") {
		
		val observations: DenseMatrix[Double] = DenseMatrix.zeros(6, 5)
		val labels: DenseVector[Double] = DenseVector.zeros(5)
		val linearLeastSquaresRegressor: LeastSquaresLinearRegression = new LeastSquaresLinearRegression(observations, labels)
		
		assert(linearLeastSquaresRegressor.getLabel(DenseVector.zeros(5)).isEmpty)
	}
	
	test("Labels are not returned if the observations with offsets matrix is singular.") {
		
		val observations: DenseMatrix[Double] = DenseMatrix.zeros(5, 6)
		val labels: DenseVector[Double] = DenseVector.zeros(5)
		val linearLeastSquaresRegressor: LeastSquaresLinearRegression = new LeastSquaresLinearRegression(observations, labels)
		
		assert(linearLeastSquaresRegressor.getLabel(DenseVector.zeros(5)).isEmpty)
	}
	
	test("Labels are computed correctly.") {

		val observations: DenseMatrix[Double] = DenseMatrix((12.0, 6.0, 1.0),(7.0, 2.0, 5.0))
		val labels: DenseVector[Double] = DenseVector(10.5, 4.0, 3.0)
		val linearLeastSquaresRegressor: LeastSquaresLinearRegression = new LeastSquaresLinearRegression(observations, labels, 0.0)

		assert(linearLeastSquaresRegressor.getLabel(DenseVector(8.0, 2.0)).get == 46.0)
	}
}
