package ComputorV1
import scala.io
import scala.util.Try
import scala.util.matching.Regex
import cats.implicits._

object ComputorV1 {

  type coefAndExp = Map[Option[Int], Int]

  def splitEquation(s: String): List[String] = s.split("=").toList

  def tryToInt(s: String): Option[Int] = Try(s.toInt).toOption

  def removeAllWhiteSpaces(s: String): String = s.replaceAll("\\s", "")

  def parseCoef(s: String): coefAndExp = {
    val r = new Regex("""([-+]?(?:\d+\.)?\d+)(?:[\*]?[Xx][\^]?(\d+))?""")

    println(s)
    val matches = r.findAllMatchIn(removeAllWhiteSpaces(s)) map (m => (m.group(1), m.group(2))) toList

    println(matches)

    val newMatches = matches.map {
      case (a, null) => (None, a.toInt)
      case (a, b) => (Option(b.toInt), a.toInt)
    }.toMap

    println(newMatches)

    r.findAllIn(removeAllWhiteSpaces(s)).map {
      case r(a, null) => (None, a.toInt)
      case r(a, b) => (Option(b.toInt), a.toInt)
    }.toMap
  }

  def invertMap(m: coefAndExp): coefAndExp = {
    m map {
      case (exp, value) => (exp, -value)
    }
  }

  def main(args: Array[String]): Unit = {

    val eqTest = removeAllWhiteSpaces("5 * X^0 + 4 * X^1 - 9 * X^2 + 5 = 1 * X^0")

    val splitAr = splitEquation(eqTest)

    val coef: coefAndExp = splitAr.length match {
      case 1 => parseCoef(splitAr.head)
      case 2 => invertMap(parseCoef(splitAr(1))) combine parseCoef(splitAr.head)
      case _ => throw new Exception("ERROR")
    }

    println(coef)

//    if (stringSplit.length )

//    println(parseCoefficients(eqTest))

//    println("Type an equation ")
//    "hello world".split(" ").foreach(println)
//    val input = scala.io.StdIn.readLine()
//    val returnValue = splitEquation(removeAllWhiteSpaces(input))
//    returnValue.foreach(println)
  }
}
