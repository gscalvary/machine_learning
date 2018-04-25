package example

import org.scalatest.FunSuite

class CubeCalculatorSpec extends FunSuite{
	test("Cubing a number returns the cube of that number.") {
		assert(CubeCalculator.cube(3) === 27)
	}
}
