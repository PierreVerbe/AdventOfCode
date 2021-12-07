package com.github.adventOfCode.Edition2021

import com.github.adventOfCode.ToolAOC

object Day7 {
  def enigma1(input: List[Int]): Int = {
    val positionList = (input.min to input.max).toList
    val fuelList = positionList.map(position =>
      input.map(crab => Math.abs(crab - position)).sum
    )

    fuelList.min
  }

  def enigma2(input: List[Int], numberDay: Int): BigInt = {
    val days = List.range(0, numberDay)
    val fishesStart = input.groupBy(_.toInt).mapValues(_.length)
    val allFishesStart = List.range(0, 9).groupBy(_.toInt).transform((k,v) => (BigInt(fishesStart.getOrElse(k, 0)), BigInt(0)))

    val fishesEnd = days.foldLeft(allFishesStart) {
      (acc, num) => {
        acc.transform((k, v) => {
          if (k == 6) {
            val map0 = acc.getOrElse(0, (BigInt(0), BigInt(0)))
            val map7 = acc.getOrElse(7, (BigInt(0), BigInt(0)))
            (v._1, map7._1 + map0._1)
          }
          else if (k == 8) {
            val map0 = acc.getOrElse(0, (BigInt(0), BigInt(0)))
            (v._1, map0._1)
          }
          else {
            val mapPrevious = acc.getOrElse(k + 1, (BigInt(0),BigInt(0)))
            (v._1, v._2 + mapPrevious._1)
          }
        }).mapValues(item => (item._2, BigInt(0)))
      }
    }

    fishesEnd.foldLeft(BigInt(0))(
      (acc, num) => acc + num._2._1
    )
  }

  def main(args: Array[String]) {
    val toolAOC = new ToolAOC("src/main/resources/Edition2021/Day7.txt")
    val input = toolAOC.getInputfile().head.split(",").toList.map(_.toInt)

    // Enigma 1
    val result1 = enigma1(input)
    println(s"$result1")

    // Enigma 2
    //val result2 = enigma2(input,256)
    //println(s"$result2")
  }

}
