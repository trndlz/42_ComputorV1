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

object ComputorV1 {

  val u = new Utils
  val p = new EquationParser

  case class Solution(message: String, solutions: Array[String])

  private def complexSolutions(delta: Double, a: Double, b: Double): Solution = {
    val sqrtDelta = u.sqrt(-delta)
    Solution(
      "Discriminant is strictly negative, the two complex solutions are:",
      Array(
        s"${u.printFraction(-b, 2 * a)} + i * ${u.printFraction(sqrtDelta, 2 * a)}",
        s"${u.printFraction(-b, 2 * a)} - i * ${u.printFraction(sqrtDelta, 2 * a)}"
      )
    )
  }

  private def naturalSolutions(delta: Double, a: Double, b: Double): Solution = {
    val sqrtDelta = u.sqrt(delta)
    Solution(
      "Discriminant is strictly positive, the solutions are:",
      Array(s"${u.printFraction(-b + sqrtDelta, 2 * a)}",
        s"${u.printFraction(-b - sqrtDelta, 2 * a)}"
      )
    )
  }

  private def uniqueSolution(a: Double, b: Double): Solution = {
    Solution(
      "Discriminant is equal to zero, the solution is",
      Array(
        s"${-b / (2 * a)}"
      )
    )
  }

  private def linearSolution(b: Double, c: Double): Solution = {
    Solution(
      "The solution is:",
      Array(
        s"${-c / b}"
      )
    )
  }

  def getSolutions(a: Double, b: Double, c: Double): Solution = {
    (a, b, c) match {
      case (0d, 0d, 0d) => Solution("All values are solution", Array())
      case (0d, 0d, _)  => Solution("It does not make any sense", Array())
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

    val equationMap = p.getEquationMap(args(0))
    val polyDegree = p.polynDegree(equationMap)
    if (polyDegree > 2) {
      println(
        "The polynomial degree is strictly greater than 2, I can't solve.")
      System.exit(1)
    }
    val (a, b, c) = p.getEquationParams(equationMap)
    val solution = getSolutions(a, b, c)
    println(s"Polynomial degree = $polyDegree")
    println(solution.message)
    solution.solutions.map(println)
  }
}
