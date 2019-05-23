//  # **************************************************************************** #
//  #                                                                              #
//  #                                                         :::      ::::::::    #
//  #    Utils.scala                                        :+:      :+:    :+:    #
//  #                                                     +:+ +:+         +:+      #
//  #    By: tmervin <marvin@42.fr>                     +#+  +:+       +#+         #
//  #                                                 +#+#+#+#+#+   +#+            #
//  #    Created: 2019/05/23 10:49:48 by tmervin           #+#    #+#              #
//  #    Updated: 2019/05/23 10:49:54 by tmervin          ###   ########.fr        #
//  #                                                                              #
//  # **************************************************************************** #

package ComputorV1

class Utils {

  private def isGoodEnough(guess: Double, x: Double) =
    absD(guess * guess - x) / x < 0.000000000000001

  private def improve(guess: Double, x: Double) = (guess + x / guess) / 2

  private def newtonsMethod(guess: Double, x: Double): Double = {
    if (isGoodEnough(guess, x)) guess
    else newtonsMethod(improve(guess, x), x)
  }

  def sqrt(x: Double): Double = newtonsMethod(1, x)

  def abs(x: Int): Int = if (x < 0) -x else x

  def absD(x: Double): Double = if (x < 0) -x else x

  def printDouble(d: Double) = new java.text.DecimalFormat("#.#####").format(d)

  def printIntOrDouble(n: Double): String = if (n % 1 == 0d) s"${n.toInt}" else printDouble(n)

  private def gcd(n: Int, d: Int): Int =
    if (d == 0) n else gcd(d, n % d)

  def printFraction(numerator: Double, denominator: Double): String = {
      if ((numerator % 1 == 0f) && (denominator % 1 == 0f)) {
        val sign = if (numerator * denominator < 0) s"- " else s""
        val n = abs(numerator.toInt)
        val d = abs(denominator.toInt)
        if (n == d) s"1" else {
          val g = gcd(n, d)
          s"$sign${n / g} / ${d / g}"
        }
      } else {
        s"${printDouble(numerator / denominator)}"
      }
  }
}
