package ComputorV1

import cats.implicits._
import scala.util.Try
import scala.util.matching.Regex

class EquationParser {

  type coefAndExp = Map[Int, Double]

  private def invertMap(m: coefAndExp): coefAndExp = m map {
    case (exp, value) => (exp, -value)
  }

  private def splitEquation(s: String): List[String] = s.split("=").toList

  private def tryToInt(s: String): Option[Int] = Try(s.toInt).toOption

  private def removeAllWhiteSpaces(s: String): String = s.replaceAll("\\s", "")

  private def parseCoef(s: String): coefAndExp = {
    println(s)
      val r = new Regex("""([-+]?(?:\d+\.)?\d+)(?:[\*]?[Xx][\^]?(\d+))?""")
      val matches = r.findAllMatchIn(removeAllWhiteSpaces(s)) map (m => {
        println(s"${m.group(2)} / ${m.group(1)} ")
        val exp = if (m.group(2) == null) 0 else m.group(2).toInt
        val coe = if (m.group(1) == null) 0 else m.group(1).toDouble
        Map(exp -> coe)
      }) toList


    println("Q")
    println(matches)
    println(matches.flatten)
    println(matches.flatten.toMap)
    println("A")

//    val matches2 = r.findAllMatchIn(removeAllWhiteSpaces(s))
//
//    val testttt = for (m <- matches2); i <- 0 until end yield (m.group())
//
//    println("POIPOIPOI")
//    println(testttt)

//    map (m => {
//      (m.group(1), m.group(2))
//    }) toL

//    val test = matches.map {
//      case (a, null) => (0, a.toDouble)
//      case (a, b) => (b.toDouble, a.toDouble)
//    }

//    println("WWW")
//
//    println(test)
//    println("EEEE")
//    println(test.toMap)



      val a = matches.flatten.toMap

    println(a)
    a
  }

  def polynomialDegree(coef: coefAndExp): Int = {
    coef.keysIterator.reduceLeft((a, b) => if ((a > b) && (a > 0.0f)) a else b)
  }

  def getEquationParams(coef: coefAndExp): (Double, Double, Double) = {
    (coef.getOrElse(2, 0d), coef.getOrElse(1, 0d),coef.getOrElse(0, 0d))
  }

  def getEquationMap(s: String): coefAndExp = {
    val eqTest = removeAllWhiteSpaces(s)
    val splitAr = splitEquation(eqTest)
    splitAr.length match {
      case 1 => parseCoef(splitAr.head)
      case 2 => parseCoef(splitAr.head) combine invertMap(parseCoef(splitAr(1)))
      case _ => throw new Exception("Wrong expressions")
    }
  }







}
