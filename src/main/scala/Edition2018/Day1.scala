package Edition2018

import scala.io.Source

object Day1 {

  def main(args: Array[String]): Unit = {
    val filename = "src/main/resources/Edition2018/Day1.txt";

    val lineList = Source.fromFile(filename).getLines.toList
    val lineListInt = lineList.map(_.toInt)

    val resultSum = lineListInt.sum
    val resultFrequencyTwice = frequencyTwice(lineListInt)

    println("Part 1, result = " + resultSum)
    println("Part 2, result =  " + resultFrequencyTwice)
  }

  def frequencyTwice(listInt : List[Int]): Int ={

    def frequencyTwiceInner (theList: List[Int], listOfResult: List[Int], sum: Int): Int ={
      if (theList.isEmpty) frequencyTwiceInner(listInt, listOfResult, sum)
      else {
        val actual = sum + theList.head

        if (!listOfResult.contains(actual)) frequencyTwiceInner(theList.tail, (actual :: listOfResult), actual)
        else actual
      }
    }

    val nbTwice = frequencyTwiceInner(listInt, List[Int](), 0)
    nbTwice
  }

}