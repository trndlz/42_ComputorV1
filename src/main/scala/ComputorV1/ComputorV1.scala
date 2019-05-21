package ComputorV1

object ComputorV1 {

  val u = new Utils
  val p = new EquationParser

  def complexSolutions(delta: Double, a: Double, b: Double): Unit = {
    println("Discriminant is strictly negative, the two complex solutions are:")
    val sqrtDelta = u.sqrt(-delta)
    println(s"${u.printFractionOrDecimal(-b,2 * a)} + i * ${u.printFractionOrDecimal(sqrtDelta,2 * a)}")
    println(s"${u.printFractionOrDecimal(-b,2 * a)} - i * ${u.printFractionOrDecimal(sqrtDelta,2 * a)}")
  }

  def naturalSolutions(delta: Double, a: Double, b: Double): Unit = {
    println("Discriminant is strictly positive, the solutions are:")
    val sqrtDelta = u.sqrt(delta)
    println(s"${u.printFractionOrDecimal(-b + sqrtDelta,2 * a)}")
    println(s"${u.printFractionOrDecimal(-b - sqrtDelta,2 * a)}")
  }

  def uniqueSolution(a: Double, b: Double): Unit = {
    println("Discriminant is equal to zero, the solution is")
    println(-b / (2 * a))
  }

  def linearSolution(b: Double, c: Double): Unit = {
    println("The solution is:")
    println(-c / b)
  }

  def main(args: Array[String]): Unit = {

    val equationMap = p.getEquationMap(args(0))
    val polyDegree = p.polynomialDegree(equationMap)
    if (polyDegree > 2) {
      println("The polynomial degree is strictly greater than 2, I can't solve.")
      System.exit(1)
    }
    println(s"Polynomial degree = $polyDegree")
    val (a, b, c) = p.getEquationParams(equationMap)
    (a, b, c) match {
      case (0d, 0d, 0d) => println("All values are solution")
      case (0d, 0d, _) => println("It does not make any sense")
      case (0d, _, _) => linearSolution(b, c)
      case (_, _, _) => {
        val delta = b * b - 4 * a * c
        delta match {
          case 0f => uniqueSolution(a, b)
          case x if x < 0 => complexSolutions(delta, a, b)
          case _ => naturalSolutions(delta, a, b)
        }
      }
    }
  }
}
