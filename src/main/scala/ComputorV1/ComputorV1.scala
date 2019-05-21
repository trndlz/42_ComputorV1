package ComputorV1
import scala.io
import scala.util.Try
import scala.util.matching.Regex
import cats.implicits._
import scala.math.sqrt

object ComputorV1 {

  type coefAndExp = Map[Int, Float]

  def splitEquation(s: String): List[String] = s.split("=").toList

  def tryToInt(s: String): Option[Int] = Try(s.toInt).toOption

  def removeAllWhiteSpaces(s: String): String = s.replaceAll("\\s", "")

  def parseCoef(s: String): coefAndExp = {

    val r = new Regex("""([-+]?(?:\d+\.)?\d+)(?:[\*]?[Xx][\^]?(\d+))?""")
    val matches = r.findAllMatchIn(removeAllWhiteSpaces(s)) map (m => (m.group(1), m.group(2))) toList

    matches.map {
      case (a, null) => (0, a.toFloat)
      case (a, b) => (if (tryToInt(b).contains(0)) 0 else tryToInt(b).get, a.toFloat)
    }.toMap
  }

  def invertMap(m: coefAndExp): coefAndExp = {
    m map {
      case (exp, value) => (exp, -value)
    }
  }

  def polynomialDegree(eq: coefAndExp): Int = eq.keysIterator.reduceLeft((a, b) => if ((a > b) && (a > 0.0f)) a else b)

  def main(args: Array[String]): Unit = {

    val eqTest = removeAllWhiteSpaces("5+4*X^1+1*X^2=1*X^2")

    val splitAr = splitEquation(eqTest)

    val coef: coefAndExp = splitAr.length match {
      case 1 => parseCoef(splitAr.head)
      case 2 => parseCoef(splitAr.head) combine invertMap(parseCoef(splitAr(1)))
      case _ => throw new Exception("ERROR")
    }

    val coefClean = coef.filter(_._2 != 0.0f)

    println(coef)
    println(coefClean)

    // a x 2 + b x + c = 0
    val a = coefClean.getOrElse(2, 0f)
    val b = coefClean.getOrElse(1, 0f)
    val c = coefClean.getOrElse(0, 0f)

    println(s"A = $a")
    println(s"B = $b")
    println(s"C = $c")

    val polyDegree = polynomialDegree(coefClean)
    println(s"Polynomial degree = $polyDegree")
    if (polyDegree > 2) {
      println("The polynomial degree is strictly greater than 2, I can't solve.")
      System.exit(1)
    }

    (a, b, c) match {
      case (0f, 0f, 0f) => println("All values are solution")
      case (0f, 0f, _) => println("It does not make any sense")
      case (0f, _, _) => {
        println("The solution is:")
        println(-c / b)
      }
      case (_, _, _) => {
        val delta = b * b - 4 * a * c
        println(s"DELTA = $delta")
        delta match {
          case 0f => {
            println("Discriminant is equal to zero, the solution is")
            println(sqrt(delta))
          }
          case x if x > 0 => {
            println("Discriminant is strictly positive, the two solutions are:")
            val sqrtDelta = sqrt(delta)
            println((-b - sqrtDelta) / (2 * a))
            println((-b + sqrtDelta) / (2 * a))
          }
          case _ => {
            println("Discriminant is strictly negative, the two complex solutions are:")
            val sqrtDelta = sqrt(-delta)
            println((-b - sqrtDelta) / (2 * a))
            println((-b + sqrtDelta) / (2 * a))
          }
        }
      }
    }
  }
}
