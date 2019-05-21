package ComputorV1

import org.scalatest.FunSuite

class ComputorV1Test extends FunSuite {

  val eqP = new EquationParser

  test("Equation String Parser") {

    // Quadratic equation : a . x ^ 2  + b . x + c = 0

    // Equation Params (a, b, c)

    // Equation Map (0 -> a, 1 -> b, 2 -> c, 3 -> d, ...)

    val testInputOutput = List(
//      ("3 + 2 * X ^ 1 + 3 * X ^ 2 = 0", (3d, 2d, 3d)),
//      ("545 -43 * X ^ 1 + 43 * X ^ 2 = 43 -123 * X ^ 2 - 50", (166d, -43d, 595d)),
      ("1 + 5 - 2 + 89 = 1 + 5 - 2", (0d, 0d, 89d)),
//      ("3 + 2 * X ^ 1 + 3 * X ^ 2 = 0", (3d, 2d, 3d)),
//      ("3 + 2 * X ^ 1 + 3 * X ^ 2 = 0", (3d, 2d, 3d)),
//      ("3 + 2 * X ^ 1 + 3 * X ^ 2 = 0", (3d, 2d, 3d)),
//      ("1 * X ^ 2", (1d, 0d, 0d)),
    )

    testInputOutput.foreach(test => {
      assert(eqP.getEquationParams(eqP.getEquationMap(test._1)) == test._2)
    })

  }

}
