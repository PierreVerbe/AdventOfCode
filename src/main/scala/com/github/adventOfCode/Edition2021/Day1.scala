package com.github.adventOfCode.Edition2021

import com.github.adventOfCode.ToolAOC

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
    val toolAOC = new ToolAOC("src/main/resources/Edition2021/Day1.txt")
    val input = toolAOC.getInputfile().map(_.toInt)

    // Enigma 1
    val result1 = enigma1(input)
    println(s"$result1")

    // Enigma 2
    val result2 = enigma2(input)
    println(s"$result2")
  }

}
