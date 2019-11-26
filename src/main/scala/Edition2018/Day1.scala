package Edition2018

import scala.io.Source

object Day1 {

  def main(args: Array[String]): Unit = {
    val filename = "src/main/resources/Edition2018/Day1.txt";

    val lineList = Source.fromFile(filename).getLines.toList
    val lineListInt = lineList.map(_.toInt)

    val resultSum = lineListInt.sum
    val resultFrequencyTwice = frequencyTwice(lineListInt)

    println(resultSum)
    println("twice " + resultFrequencyTwice)
  }

  def frequencyTwice(listInt : List[Int]): Int ={

    def frequencyTwiceInner (theList: List[Int], sum: Int, countTwice: Int): Int ={
      if (theList.isEmpty) return countTwice
      else {
        val actual = sum + theList.head
        println(actual)

        if (actual == 394) frequencyTwiceInner(theList.tail, actual, countTwice + 1)
        else frequencyTwiceInner(theList.tail, actual, countTwice)
      }
    }

    val nbTwice = frequencyTwiceInner(listInt, 0, 0)
    return nbTwice
  }

}