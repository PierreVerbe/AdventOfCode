package com.github.adventOfCode.Edition2021

import com.github.adventOfCode.ToolAOC

object Day7 {
  def enigma1(input: List[Int]): Int = {
    val positionList = (input.min to input.max).toList
    val fuelList = positionList.map(position =>
      input.map(crab =>
        Math.abs(crab - position)
      ).sum
    )

    fuelList.min
  }

  def enigma2(input: List[Int]): Int = {
    val positionList = (input.min to input.max).toList
    val fuelList = positionList.map(position =>
      input.map(crab =>
        (0 to Math.abs(crab - position)).toList.sum
      ).sum
    )

    fuelList.min
  }

  def main(args: Array[String]) {
    val toolAOC = new ToolAOC("src/main/resources/Edition2021/Day7.txt")
    val input = toolAOC.getInputfile().head.split(",").toList.map(_.toInt)

    // Enigma 1
    val result1 = enigma1(input)
    println(s"$result1")

    // Enigma 2
    val result2 = enigma2(input)
    println(s"$result2")
  }

}
