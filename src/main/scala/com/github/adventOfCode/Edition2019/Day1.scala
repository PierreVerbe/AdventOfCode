package com.github.adventOfCode.Edition2019

import scala.io.Source

object Day1 {

  def main(args: Array[String]): Unit = {

    def requiredFuel(fuel: Int): Int = {
      val preCalculation = (fuel/3)-2
      if (preCalculation <= 0) fuel
      else fuel + requiredFuel(preCalculation)
    }

    val filename = "src/main/resources/Edition2019/Day1.txt"

    /*for (line <- Source.fromFile(filename).getLines) {
     val lineInt = line
   }*/

    val lineList = Source.fromFile(filename).getLines.toList
    val lineListInt = lineList.map(_.toInt)

    val lineListResult1 = lineListInt.map(x => (x/3)-2)
    val finalResult1 = lineListResult1.sum
    println("Part 1, result = " + finalResult1)

    val lineListResult2 = lineListResult1.map(x => requiredFuel(x))
    val finalResult2 = lineListResult2.sum
    println("Part 2, result = " + finalResult2)

  }
}