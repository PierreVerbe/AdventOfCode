package com.github.adventOfCode.Edition2021

import com.github.adventOfCode.ToolAOC

object Day8 {
  case class Display(signal1: List[String], signal2: List[String])

  def enigma1(input: List[Display]): Int = {
    val easySegments = List(2, 4, 3, 7)
    val result = input.map(display => {
      val filteredEasySegments = display.signal2.filter(segments => easySegments.contains(segments.length))
      filteredEasySegments.length
    })

    result.sum
  }

  def enigma2(input: List[Display]): Int = {
    val result = input.map( display => {
      val patterns = display.signal1.map(pattern => (pattern, pattern.length))
      val mapPatterns = patterns.flatMap(pattern => {
        pattern._2 match {
          case 2 => List((1, pattern._1 ))
          case 3 => List((7, pattern._1 ))
          case 4 => List((4, pattern._1 ))
          case 7 => List((8, pattern._1 ))
          case _ => List()
        }
      }).toMap

      //println(mapPatterns)
      val mapPatternsLength6 = patterns.filter(_._2 == 6).map( pattern => {
        val splitedPattern = pattern._1.split("").toList
        val isPattern0 = mapPatterns.getOrElse(1, "")
          .split("").toList
          .map(splitedPattern.contains(_))
          .reduce((x, y) => x && y)
        val isPattern9 = mapPatterns.getOrElse(4, "")
          .split("").toList
          .map(splitedPattern.contains(_))
          .reduce((x, y) => x && y)

        if (isPattern9) (9, pattern._1)
        else if (isPattern0) (0, pattern._1)
        else (6, pattern._1)
      }).toMap

      val patternsLength5 = patterns.filter(_._2 == 5).map( pattern => {
        //println(pattern._1)
        val splitedPattern = pattern._1.split("").toList
        val isPattern3 = mapPatterns.getOrElse(1, "")
          .split("").toList
          .map(splitedPattern.contains(_))
          .reduce((x, y) => x && y)
        val isPattern2 = {
          val pattern8 = mapPatterns.getOrElse(8, "").split("").toList
          val pattern9 = mapPatterns.getOrElse(9, "").split("").toList
          val bottomLeft = pattern8.flatMap(item => if (pattern9.contains(item)) List() else List(item)).head
          //println(bottomLeft)
          splitedPattern.contains(bottomLeft)
        }

        //println(s"$isPattern3 $isPattern2")

        if (isPattern3) (3, pattern._1)
        else if (isPattern2) (2, pattern._1)
        else (5, pattern._1)
      }).toMap

      val merged = mapPatterns.toSeq ++ mapPatternsLength6.toSeq ++ patternsLength5.toSeq
      val grouped = merged.groupBy(_._1)
      val cleaned = grouped.mapValues(_.map(_._2).toList.head)
      //println(patterns)
      //println(mapPatternsLength6)
      println(cleaned)

    })

    0
  }

  def parseLine(line: String): Display = {
    val signals = line.split(" \\| ").toList
    val signal1 = signals.head.split(" ").toList
    val signal2 = signals(1).split(" ").toList

    Display(signal1, signal2)
  }

  def main(args: Array[String]) {
    val toolAOC = new ToolAOC("src/main/resources/Edition2021/Day8.txt")
    val input = toolAOC.getInputfile().map(parseLine)

    // Enigma 1
    val result1 = enigma1(input)
    println(s"$result1")

    // Enigma 2
    val result2 = enigma2(input)
    println(s"$result2")
  }

}
