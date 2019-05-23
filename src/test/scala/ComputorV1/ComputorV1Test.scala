package ComputorV1

import org.scalatest.FunSuite
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

class ComputorV1Test extends FunSuite {

  case class TestClass(
      inputString: String,
      reducedForm: String,
      solution: Array[String]
  )

  val eqP = new EquationParser

  val testInputOutput = List(
    // Simple equations
    TestClass("1 * X^0 + 2 * X^1 = - 1 * X^0 + 4 * X^1",
              "2 * X^0 - 2 * X^1 = 0",
              Array("1")),
    TestClass("-1 * X^0 - 2 * X^1 = 1 * X^0 + 2 * X^1",
              "- 2 * X^0 - 4 * X^1 = 0",
              Array("-0.5")),
    TestClass("1 * X^0 + 2 * X^1 + 3 * X^2 = - 1 * X^0 + 4 * X^1 + 3 * X^2",
              "2 * X^0 - 2 * X^1 + 0 * X^2 = 0",
              Array("1")),
    // Quadratic equations
    TestClass("1 * X^0 + 2 * X^1 + 2 * X^2 = - 1 * X^0 + 4 * X^1 + 3 * X^2",
              "2 * X^0 - 2 * X^1 - 1 * X^2 = 0",
              Array("-2.73205", "0.732051")),
    TestClass("1 * X^0 + 2 * X^1 + 4 * X^2 = - 1 * X^0 + 4 * X^1 + 3 * X^2",
              "2 * X^0 - 2 * X^1 + 1 * X^2 = 0",
              Array("1 + i", "1 - i")),
    TestClass("2 * X^0 + 2 * X^1 + 4 * X^2 = - 1 * X^0 + 4 * X^1 + 3 * X^2",
              "3 * X^0 - 2 * X^1 + 1 * X^2 = 0",
              Array("2 + 2.82843i", "2 - 2.82843i")),
    TestClass("1 * X^0 + 2 * X^1 + 4 * X^2 = 0 * X^0 + 4 * X^1 + 3 * X^2",
              "1 * X^0 - 2 * X^1 + 1 * X^2 = 0",
              Array("1")),
    TestClass(
      "1 * X^0 + 2 * X^1 + 4 * X^2 = 0 * X^0 + 4 * X^1 + 3 * X^2 + 0 * X^3 + 0 * X^4 + 0 * X^5",
      "1 * X^0 - 2 * X^1 + 1 * X^2 + 0 * X^3 + 0 * X^4 + 0 * X^5 = 0",
      Array("1")
    ),
    // Equations with floats
    TestClass("1 * X^0 + 2.5 * X^1 = - 1.561151 * X^0 + 4.000 * X^1",
              "2.56115 * X^0 - 1.5 * X^1 = 0",
              Array("1.70743")),
    TestClass(
      "1.8526 * X^0 + 2.989 * X^1 + 2.16 * X^2 = - 1.122241 * X^0 + 4.999 * X^1 + 3.25 * X^2",
      "2.97484 * X^0 - 2.01 * X^1 - 1.09 * X^2 = 0",
      Array("-2.81393", "0.969893")
    ),
    // Errors
    TestClass("1 * X^0 = 2 * X^0", "- 1 * X^0 = 0", Array("No solution")),
    TestClass("1 * X^0 = 1 * X^0",
              "0 * X^0 = 0",
              Array("Infinite number of solutions")),
    // Other cases
    TestClass(
      "1 * X^0 + 2 * X^1 + 4 * X^2 = 0 * X^0 + 4 * X^1 + 3 * X^2 + 0 * X^3 + 0 * X^4 + 2 * X^5",
      "1 * X^0 - 2 * X^1 + 1 * X^2 + 0 * X^3 + 0 * X^4 - 2 * X^5 = 0",
      Array(
        "La puissance étant supérieure a 2, le programme ne peut donc pas le résoudre.")
    ),
    // Parsing Errors
  )

  test("Equation String Parser") {

    // Quadratic equation : a . x ^ 2  + b . x + c = 0

    // Equation Params (a, b, c)

    // Equation Map (0 -> a, 1 -> b, 2 -> c, 3 -> d, ...)

    testInputOutput.foreach(iTest => {

//      val a = iTest.reducedForm
//      val b = eqP.printSimplifiedEquation(eqP.getEquationMap(iTest.inputString))
//
//      if (a != b) {
//        println(iTest.inputString)
//        println(a)
//        println(b)
//      }

      assert(
        iTest.reducedForm == eqP.printSimplifiedEquation(
          eqP.getEquationMap(iTest.inputString)))
    })

  }

}
