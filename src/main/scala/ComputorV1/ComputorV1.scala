//  # **************************************************************************** #
//  #                                                                              #
//  #                                                         :::      ::::::::    #
//  #    ComputorV1.scala                                   :+:      :+:    :+:    #
//  #                                                     +:+ +:+         +:+      #
//  #    By: tmervin <marvin@42.fr>                     +#+  +:+       +#+         #
//  #                                                 +#+#+#+#+#+   +#+            #
//  #    Created: 2019/05/23 10:49:48 by tmervin           #+#    #+#              #
//  #    Updated: 2019/05/23 10:49:54 by tmervin          ###   ########.fr        #
//  #                                                                              #
//  # **************************************************************************** #

package ComputorV1

import scala.collection.mutable.ListBuffer

object SolutionTypes {
  val QUADRATIC_SINGLE = "Discriminant is equal to zero, the solution is:"
  val LINEAR_EQUATION = "Linear equation, the solution is:"
  val QUADRATIC_REAL = "Discriminant is strictly positive, the solutions are:"
  val QUADRATIC_COMPLEX = "Discriminant is strictly negative, the two complex solutions are:"
  val NO_SOLUTION = "No solution."
  val INFINITE_SOLUTIONS = "Infinite number of solutions."
}

object ErrorTypes {
  val NO_EQUATION = "Input Error -> No equation is given"
  val WRONG_FORMAT = "Input Error -> Wrong equation format"
  val TOO_HIGH_DEGREE = "Input Error -> The polynomial degree is strictly greater than 2, I can't solve."
  val INFINITE_SOLUTIONS = "Input Error -> Infinite number of solutions."
}

case class Solution(message: String, solutions: Array[String])

object ComputorV1 {

  val u = new Utils
  val p = new EquationParser

  private def complexSolutions(delta: Double, a: Double, b: Double): Solution = {
    val sqrtDelta = u.sqrt(-delta)
    val realPart = -b / (2 * a)
    val complexPart = sqrtDelta / (2 * a)
    val realS = if (realPart != 0d) u.printFraction(-b, 2 * a) else s"0"
    val complexS = if (complexPart == 1d) s"i" else s"${u.printFraction(sqrtDelta, 2 * a)}i"
    Solution(SolutionTypes.QUADRATIC_COMPLEX,
      Array(
        s"$realS + $complexS",
        s"$realS - $complexS",
      )
    )
  }

  private def naturalSolutions(delta: Double, a: Double, b: Double): Solution = {
    val sqrtDelta = u.sqrt(delta)
    Solution(SolutionTypes.QUADRATIC_REAL,
      Array(s"${u.printFraction(-b + sqrtDelta, 2 * a)}",
        s"${u.printFraction(-b - sqrtDelta, 2 * a)}"
      )
    )
  }

  private def uniqueSolution(a: Double, b: Double): Solution =
    Solution(SolutionTypes.QUADRATIC_SINGLE, Array(s"${u.printIntOrDouble(-b / (2 * a))}"))

  private def linearSolution(b: Double, c: Double): Solution =
    Solution(SolutionTypes.LINEAR_EQUATION, Array(s"${u.printIntOrDouble(-c / b)}"))

  def getSolutions(a: Double, b: Double, c: Double): Solution = {
    (a, b, c) match {
      case (0d, 0d, 0d) => Solution(SolutionTypes.INFINITE_SOLUTIONS, Array())
      case (0d, 0d, _)  => Solution(SolutionTypes.NO_SOLUTION, Array())
      case (0d, _, _)   => linearSolution(b, c)
      case (_, _, _) => {
        val delta = b * b - 4 * a * c
        delta match {
          case 0f         => uniqueSolution(a, b)
          case x if x < 0 => complexSolutions(delta, a, b)
          case _          => naturalSolutions(delta, a, b)
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val errors = p.getInputErrors(args)
    if (errors.isEmpty) {
      val equationMap = p.getEquationMap(args(0))
      val (a, b, c) = p.getEquationParams(equationMap)
      val solution = getSolutions(a, b, c)
      val polyD = p.polynDegree(equationMap)
      println(s"Reduced form: ${p.printSimplifiedEquation(equationMap)}")
      println(s"Polynomial degree: ${polyD.get}")
      println(solution.message)
      solution.solutions.foreach(println)
    } else {
      errors.foreach(println)
    }
  }
}
