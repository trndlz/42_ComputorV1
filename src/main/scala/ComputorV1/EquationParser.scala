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

import cats.implicits._
import scala.util.Try
import scala.util.matching.Regex

class EquationParser {

  val u = new Utils

  case class EqParameters(coefficients: Double, degree: Int)

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

  private def mergeCoefficients(l: List[EqParameters]): List[EqParameters] = {
    l.groupBy(_.degree) map { _._2 reduce { (a, b) => EqParameters(a.coefficients + b.coefficients, a.degree)}} toList
  }

  private def getParamsList(s: String): List[EqParameters] = {
    val r = new Regex("""([-+]?(?:\d+\.)?\d+)(?:[\*]?[Xx][\^]?(\d+))?""")
    val matches = r.findAllMatchIn(removeAllWhiteSpaces(s)) map (m => {
      groupToEqParam(m.group(1), m.group(2))
    }) toList

    mergeCoefficients(matches)
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
      case 1 => getParamsList(splitAr.head)
      case 2 => mergeCoefficients(getParamsList(splitAr.head) ++ invertMap(getParamsList(splitAr(1))))
      case _ => Nil
    }
//    if (eqMap.forall(_.coefficients != 0d)) throw new Exception("Djigebenw") else eqMap
  }







}
