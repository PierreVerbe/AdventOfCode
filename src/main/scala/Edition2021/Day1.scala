package Edition2021

import scala.io.Source

object Day1 {
  def enigma1(input: List[Int]): Int = {
    val slidingWindow = input.sliding(2, 1).toList

    val measurements = slidingWindow.map(listItem => {
      if (listItem.head < listItem(1)) "increased"
      else if (listItem.head == listItem(1)) "no change"
      else "decreased"
    })
    measurements.count(_ == "increased")
  }

  def enigma2(input: List[Int]): Int = {
    val slidingWindow = input.sliding(3, 1).toList.map(_.sum)
    enigma1(slidingWindow)
  }

  def main(args: Array[String]) {
    val filename = "src/main/resources/Edition2021/Day1.txt"

    val source = Source.fromFile(filename)
    val lineList = source.getLines.toList.map(_.toInt)
    source.close()

    // Enigma 1
    val result1 = enigma1(lineList)
    println(s"$result1")

    // Enigma 2
    val result2 = enigma2(lineList)
    println(s"$result2")
  }
}
