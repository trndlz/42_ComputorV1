package ComputorV1
import io

object ComputorV1 {



  def removeAllWhiteSpaces(input: String): String = {
    input.replaceAll("\\s", "")
  }

  def main(args: Array[String]): Unit = {
    println("Type an equation ")
    val input = scala.io.StdIn.readLine()
    println("Did you type this ? " + removeAllWhiteSpaces(input))
  }
}
