package com.github.adventOfCode.Edition2021

import com.github.adventOfCode.ToolAOC

object Day3 {

  def enigma1(input: List[List[Int]]): Int = {
    val length = input.length
    val sumNList = input.transpose.map(_.sum)

    val gammaRateBinary = sumNList.map(item => if (item < length-item) 0 else 1)
    val epsilonRateBinary = gammaRateBinary.map(item => if(item == 1) 0 else 1)

    val gammaRate = Integer.parseInt(gammaRateBinary.mkString, 2)
    val epsilonRate = Integer.parseInt(epsilonRateBinary.mkString, 2)
    gammaRate * epsilonRate
  }

  def enigma2(input: List[List[Int]]): Int = {
    def filterListFromColumn(nColumn: Int, input: List[List[Int]]): List[List[Int]] = {
      val length = input.length
      val sumNList = input.transpose.map(_.sum)
      val gammaRateBinary = sumNList.map(item => if (item < length-item) 0 else 1)

      if (gammaRateBinary(nColumn) == 0) input.filter(item => item(nColumn) == 0).take(length-sumNList(nColumn))
      else input.filter(item => item(nColumn) == 1).take(sumNList(nColumn))
    }

    val listRange = List.range(0, input.head.length)
    val oxygenGeneratorRateBinary = listRange.foldLeft(input) {
      (acc, num) => filterListFromColumn(num, acc, false)
    }
    val co2ScrubberRateBinary = listRange.foldLeft(input) {
      (acc, num) => filterListFromColumn(num, acc, true)
    }

    oxygenGeneratorRateBinary.foreach(println)

    val oxygenGeneratorRate = Integer.parseInt(oxygenGeneratorRateBinary.mkString, 2)
    val co2ScrubberRate = Integer.parseInt(co2ScrubberRateBinary.mkString, 2)

    oxygenGeneratorRate * co2ScrubberRate
  }

  def parseLine(line: String): List[Int] = {
    line.split("").toList.map(_.toInt)
  }

  def main(args: Array[String]) {
    val toolAOC = new ToolAOC("src/main/resources/Edition2021/Day3.txt")
    val input = toolAOC.getInputfile().map(parseLine)

    // Enigma 1
    val result1 = enigma1(input)
    println(s"$result1")

    // Enigma 2
    val result2 = enigma2(input)
    println(s"$result2")
  }

}
