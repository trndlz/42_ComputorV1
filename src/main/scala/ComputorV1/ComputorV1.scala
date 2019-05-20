package ComputorV1
import scala.io
import scala.util.matching.Regex

object ComputorV1 {


  def splitEquation(input: String): List[String] = {
    input.split("=").toList
  }

  def removeAllWhiteSpaces(input: String): String = {
    input.replaceAll("\\s", "")
  }

  def main(args: Array[String]): Unit = {

    val regTest = new Regex("([-+]?(?:\\d+\\.)?\\d+)(?:[\\*]?[Xx][\\^]?(\\d+))?")
    val eqTest = removeAllWhiteSpaces("5 * X^0 + 4 * X^1 - 9.3 * X^2 + 5 = 1 * X^0")
    val matchTest = regTest.findAllIn(eqTest).map {
      case regTest(a, b) => (a, b)
    }.toList

    println(matchTest)

//    println("Type an equation ")
//    "hello world".split(" ").foreach(println)
//    val input = scala.io.StdIn.readLine()
//    val returnValue = splitEquation(removeAllWhiteSpaces(input))
//    returnValue.foreach(println)
  }
}
