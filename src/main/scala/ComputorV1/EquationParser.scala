//  # **************************************************************************** #
//  #                                                                              #
//  #                                                         :::      ::::::::    #
//  #    EquationParser.scala                               :+:      :+:    :+:    #
//  #                                                     +:+ +:+         +:+      #
//  #    By: tmervin <marvin@42.fr>                     +#+  +:+       +#+         #
//  #                                                 +#+#+#+#+#+   +#+            #
//  #    Created: 2019/05/23 10:49:48 by tmervin           #+#    #+#              #
//  #    Updated: 2019/05/23 10:49:54 by tmervin          ###   ########.fr        #
//  #                                                                              #
//  # **************************************************************************** #

package ComputorV1

import scala.util.Try
import scala.util.matching.Regex


class EquationParser {

  val u = new Utils

  object ParsingErrors {
    val NOT_INT_EXP = "Exposant should be an integer"
  }

  case class EqParameters(
                           coefficients: Double,
                           degree: Int,
                         )

  private def invertMap(m: List[EqParameters]): List[EqParameters] = {
    m.map(a => EqParameters(-a.coefficients, a.degree))
  }

  private def splitEquation(s: String): List[String] = s.split("=").toList

  private def tryToInt(s: String): Option[Int] = Try(s.toInt).toOption

  private def removeAllWhiteSpaces(s: String): String = s.replaceAll("\\s", "")

  private def groupToEqParam(c: String, d: String): EqParameters = {
    val coef = Try(c.toDouble).toOption.getOrElse(0d)
    val deg = Try(d.toInt).toOption.getOrElse(0)
    EqParameters(coef, deg)
  }


  private def paramConversion(a: Option[String], b: Option[String]): EqParameters = {
    val aDouble: Double = a.flatMap(parseDouble).getOrElse(0f)
    val bInt: Int = b.flatMap(parseInt).getOrElse(1)
    EqParameters(aDouble, bInt)
  }

  private def parseInt(i: String): Option[Int] = Try(i.toInt).toOption
  private def parseDouble(i: String): Option[Double] = Try(i.toDouble).toOption

  private def mergeCoefficients(l: List[EqParameters]): List[EqParameters] = {
    l.groupBy(_.degree) map { _._2 reduce { (a, b) => EqParameters(a.coefficients + b.coefficients, a.degree)}} toList
  }

  private def parseEachCase(i: String): Option[EqParameters] = {
    // EQUATION A * X ^ B
    val aXExpB = new Regex("""^([-+]?[0-9]*\.?[0-9]+)\*X\^([-+]?[0-9]+)*$""")  // A != 0 && B != 0
    val aX = new Regex("""^([-+]?[0-9]*\.?[0-9]+)\*X$""")                      // A != 0 && B == 0
    val xExpB = new Regex("""^([-+])?X\^([-+]?[0-9]+)*$""")                           // A = 1 && B != 0
    val xOnly = new Regex("""^([-+]?)X$""")                                    // A = 1 && B = 1
    val const = new Regex("""^([-+]?[0-9]*\.?[0-9]+)*$""")                     // A != 0 && B = 1

    i match {
      case aXExpB(a, b) => Some(paramConversion(Some(a), Some(b)))
      case aX(a) => Some(paramConversion(Some(a), Some("1")))
      case xExpB(s, b) => {
        val sign = s match {
          case "-" => "-1"
          case _ => "1"
        }
        Some(paramConversion(Some(sign), Some(b)))
      }
      case xOnly(a) => Some(paramConversion(if (a == s"-") Some("-1") else Some("1"), None))
      case const(a) => Some(paramConversion(Some(a), Some("0")))
      case _ => None
    }
  }

  private def getParamsList(s: String): Option[List[EqParameters]] = {
    val splitElements = s.split("""(?=[+-])""")
    val parameters: List[Option[EqParameters]] = splitElements.map(parseEachCase).toList
    if (parameters.contains(None)) None else Some(mergeCoefficients(parameters.flatten))
  }

  def highestDegree(coef: List[EqParameters]): Int = coef.map(_.degree).max

  def polynDegree(coef: List[EqParameters]): Option[Int] = {
    val removedBlanks = coef.filter(_.coefficients != 0d).map(_.degree)
    if (removedBlanks.isEmpty) None else Some(removedBlanks.max)
  }

  private def getCoef(d: Int, eq: List[EqParameters]): Option[Double] = eq.find(_.degree == d).map(_.coefficients)

  def getEquationParams(eq: List[EqParameters]): (Double, Double, Double) = {
    (getCoef(2, eq).getOrElse(0), getCoef(1, eq).getOrElse(0), getCoef(0, eq).getOrElse(0))
  }

  private def numberSign(n: Double): String = if (n < 0) s"-" else s"+"

  def printSimplifiedEquation(eq: List[EqParameters]): String = {
    val p = polynDegree(eq).getOrElse(highestDegree(eq))
    val generator = for (i <- 0 to p if getCoef(i, eq).isDefined) yield (i, getCoef(i, eq).get)
    generator.map(e => {
      val number = (e._1, e._2) match {
        case (0, x) if x < 0 => s"- ${u.printIntOrDouble(u.absD(e._2))}"
        case (0, x) if x >= 0 => s"${u.printIntOrDouble(u.absD(e._2))}"
        case (_, x) if x < 0 => s" - ${u.printIntOrDouble(u.absD(e._2))}"
        case (_, x) if x >= 0 => s" + ${u.printIntOrDouble(u.absD(e._2))}"
      }
      s"$number * X^${e._1}"
    }).mkString.concat(" = 0")
  }

  def getEquationMap(s: String): List[EqParameters] = {
    val eqTest = removeAllWhiteSpaces(s)
    val splitAr = splitEquation(eqTest)
    splitAr.length match {
      case 1 =>
        val left = getParamsList(splitAr.head)
        if (left.isEmpty) Nil else left.get
      case 2 =>
        val left = getParamsList(splitAr.head)
        val right = getParamsList(splitAr(1))
        if (left.isEmpty || right.isEmpty) Nil else mergeCoefficients(left.get ++ invertMap(right.get))
      case _ => Nil
    }
  }
}
