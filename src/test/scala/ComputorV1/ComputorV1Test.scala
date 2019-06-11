//  # **************************************************************************** #
//  #                                                                              #
//  #                                                         :::      ::::::::    #
//  #    ComputorV1Test.scala                               :+:      :+:    :+:    #
//  #                                                     +:+ +:+         +:+      #
//  #    By: tmervin <marvin@42.fr>                     +#+  +:+       +#+         #
//  #                                                 +#+#+#+#+#+   +#+            #
//  #    Created: 2019/05/23 10:49:48 by tmervin           #+#    #+#              #
//  #    Updated: 2019/05/23 10:49:54 by tmervin          ###   ########.fr        #
//  #                                                                              #
//  # **************************************************************************** #

package ComputorV1

import org.scalatest.FunSuite

// Quadratic equation : a . x ^ 2  + b . x + c = 0
// Equation Params (a, b, c)
// Equation Map (0 -> a, 1 -> b, 2 -> c, 3 -> d, ...)

class ComputorV1Test extends FunSuite {

  case class ReducedFormAndSolutionClass(inputString: String,
                                          reducedForm: String,
                                          solution: Solution)

  case class InputErrorClass(inputString: String,
                             errorTypes: String)


  val eqP = new EquationParser

  val testReducedFormAndResult = List(
    // LINEAR EQUATIONS
    ReducedFormAndSolutionClass(
      "1 * X^0 + 2 * X^1 = - 1 * X^0 + 4 * X^1",
      "2 * X^0 - 2 * X^1 = 0",
      Solution(SolutionTypes.LINEAR_EQUATION, Array("1"))
    ),
    ReducedFormAndSolutionClass(
      "-1 * X^0 - 2 * X^1 = 1 * X^0 + 2 * X^1",
      "- 2 * X^0 - 4 * X^1 = 0",
      Solution(SolutionTypes.LINEAR_EQUATION, Array("-0.5"))
    ),
    ReducedFormAndSolutionClass(
      "1 * X^0 + 2 * X^1 + 3 * X^2 = - 1 * X^0 + 4 * X^1 + 3 * X^2",
      "2 * X^0 - 2 * X^1 = 0",
      Solution(SolutionTypes.LINEAR_EQUATION, Array("1"))
    ),
    // Quadratic equations
    ReducedFormAndSolutionClass(
      "1 * X^0 + 2 * X^1 + 2 * X^2 = - 1 * X^0 + 4 * X^1 + 3 * X^2",
      "2 * X^0 - 2 * X^1 - 1 * X^2 = 0",
      Solution(SolutionTypes.QUADRATIC_REAL, Array("-2.73205", "0.73205"))
    ),
    ReducedFormAndSolutionClass(
      "1 * X^0 + 2 * X^1 + 4 * X^2 = - 1 * X^0 + 4 * X^1 + 3 * X^2",
      "2 * X^0 - 2 * X^1 + 1 * X^2 = 0",
      Solution(SolutionTypes.QUADRATIC_COMPLEX, Array("1 + i", "1 - i"))
    ),
    ReducedFormAndSolutionClass(
      "2 * X^0 + 2 * X^1 + 4 * X^2 = - 1 * X^0 + 4 * X^1 + 3 * X^2",
      "3 * X^0 - 2 * X^1 + 1 * X^2 = 0",
      Solution(SolutionTypes.QUADRATIC_COMPLEX, Array("1 + 1.41421i", "1 - 1.41421i"))
    ),
    ReducedFormAndSolutionClass(
      "1 * X^0 + 2 * X^1 + 4 * X^2 = 0 * X^0 + 4 * X^1 + 3 * X^2",
      "1 * X^0 - 2 * X^1 + 1 * X^2 = 0",
      Solution(SolutionTypes.QUADRATIC_SINGLE, Array("1"))
    ),
    ReducedFormAndSolutionClass(
      "1 * X^0 + 2 * X^1 + 4 * X^2 = 0 * X^0 + 4 * X^1 + 3 * X^2 + 0 * X^3 + 0 * X^4 + 0 * X^5",
      "1 * X^0 - 2 * X^1 + 1 * X^2 = 0",
      Solution(SolutionTypes.QUADRATIC_SINGLE, Array("1"))
    ),
    // Equations with floats
    ReducedFormAndSolutionClass(
      "1 * X^0 + 2.5 * X^1 = - 1.561151 * X^0 + 4.000 * X^1",
      "2.56115 * X^0 - 1.5 * X^1 = 0",
      Solution(SolutionTypes.LINEAR_EQUATION, Array("1.70743"))
    ),
    ReducedFormAndSolutionClass(
      "1.8526 * X^0 + 2.989 * X^1 + 2.16 * X^2 = - 1.122241 * X^0 + 4.999 * X^1 + 3.25 * X^2",
      "2.97484 * X^0 - 2.01 * X^1 - 1.09 * X^2 = 0",
      Solution(SolutionTypes.QUADRATIC_REAL, Array("-2.81393", "0.96989"))
    ),
    // Errors
    ReducedFormAndSolutionClass(
      "1 * X^0 = 2 * X^0",
      "- 1 * X^0 = 0",
      Solution(SolutionTypes.NO_SOLUTION, Array())
    ),
    ReducedFormAndSolutionClass(
      "1 * X^0 = 1 * X^0",
      "0 * X^0 = 0",
      Solution(SolutionTypes.INFINITE_SOLUTIONS, Array())
    ),
  )

  val testAlternativeWriting = List(
    ReducedFormAndSolutionClass(
      "1 * X^0 + 2 * X^1 = - 6 * X^0 + 4 * X^1 + 1 * X ^ 2",
      "7 * X^0 - 2 * X^1 - 1 * X^2 = 0",
      Solution(SolutionTypes.QUADRATIC_REAL, Array("-2.73205", "0.73205"))
    ),
    ReducedFormAndSolutionClass(
      "1 + 2 * X^1 = - 6 + 4 * X^1 + 1 * X ^ 2",
      "7 * X^0 - 2 * X^1 - 1 * X^2 = 0",
      Solution(SolutionTypes.QUADRATIC_REAL, Array("-2.73205", "0.73205"))
    ),
     ReducedFormAndSolutionClass(
      "1 + 2 X = - 6 + 4 X + 1 * X ^ 2",
      "7 * X^0 - 2 * X^1 - 1 * X^2 = 0",
      Solution(SolutionTypes.QUADRATIC_REAL, Array("-2.73205", "0.73205"))
    ),
    ReducedFormAndSolutionClass(
      "1 + 2X = - 6 + 4X + X ^ 2",
      "7 * X^0 - 2 * X^1 - 1 * X^2 = 0",
      Solution(SolutionTypes.QUADRATIC_REAL, Array("-2.73205", "0.73205"))
    ),
  )

  val inputErrors = List(
    InputErrorClass(inputString = "5 * X ^ 0=5 * X ^ 0", errorTypes = ErrorTypes.INFINITE_SOLUTIONS),
    InputErrorClass(inputString = "", errorTypes = ErrorTypes.NO_EQUATION),
    InputErrorClass(inputString = "x = a", errorTypes = ErrorTypes.WRONG_FORMAT),
    InputErrorClass(inputString = "i", errorTypes = ErrorTypes.WRONG_FORMAT),
    InputErrorClass(inputString = "===", errorTypes = ErrorTypes.WRONG_FORMAT),
    InputErrorClass(inputString = "==", errorTypes = ErrorTypes.WRONG_FORMAT),
    InputErrorClass(inputString = "=", errorTypes = ErrorTypes.WRONG_FORMAT),
    InputErrorClass(inputString = "1 = 2 = 3", errorTypes = ErrorTypes.WRONG_FORMAT),
  )

  test("Reduced Form") {
    testReducedFormAndResult.foreach(iTest => {
      val expectedReducedForm = iTest.reducedForm
      val equationMap = eqP.getEquationMap(iTest.inputString)
      val reducedForm = eqP.printSimplifiedEquation(equationMap)
      assert(expectedReducedForm == reducedForm)
    })
  }

  test("Results") {
    testReducedFormAndResult.foreach(iTest => {
      val expectedSolution = iTest.solution
      val equationMap = eqP.getEquationMap(iTest.inputString)
      val (a, b, c) = eqP.getEquationParams(equationMap)
      val solution = ComputorV1.getSolutions(a, b, c)
      assert(expectedSolution.message == solution.message)
      assert(expectedSolution.solutions.deep == solution.solutions.deep)
    })
  }

  test("InputErrors") {
    inputErrors.foreach(iTest => {
      val expectedError = iTest.errorTypes
      val error = eqP.getInputErrors(Array(iTest.inputString))
      assert(error.get == expectedError)
    })
  }

  test("Alternative simplified Writing of Equation") {
    testAlternativeWriting.foreach(iTest => {
      val expectedReducedForm = iTest.reducedForm
      val equationMap = eqP.getEquationMap(iTest.inputString)
      val reducedForm = eqP.printSimplifiedEquation(equationMap)
      assert(expectedReducedForm == reducedForm)
    })
  }

}
