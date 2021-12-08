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
    val listDecodedNumber = input.map( display => {
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

        //println(s"$isPattern0 $isPattern9")
        (isPattern0, isPattern9) match {
          case (true, true) => (9, pattern._1)
          case (false, false) => (6, pattern._1)
          case _ => (0, pattern._1)
        }
      }).toMap

      val merged = mapPatterns.toSeq ++ mapPatternsLength6.toSeq
      val grouped = merged.groupBy(_._1)
      val addedPatterns = grouped.mapValues(_.map(_._2).toList.head)

      val mapPatternsLength5 = patterns.filter(_._2 == 5).map( pattern => {
        //println(pattern._1)
        val splitedPattern = pattern._1.split("").toList
        val isPattern3 = mapPatterns.getOrElse(1, "")
          .split("").toList
          .map(splitedPattern.contains(_))
          .reduce((x, y) => x && y)
        val isPattern2 = {
          val pattern8 = mapPatterns.getOrElse(8, "").split("").toList
          val pattern9 = addedPatterns.getOrElse(9, "").split("").toList
          val bottomLeft = pattern8.flatMap(item => if (pattern9.contains(item)) List() else List(item))

          splitedPattern.contains(bottomLeft.head)
        }

        println(s"$isPattern2 $isPattern3")

        (isPattern2, isPattern3) match {
          case (false, true) => (3, pattern._1)
          case (true, false) => (2, pattern._1)
          case _ => (5, pattern._1)
        }
      }).toMap

      println(mapPatternsLength5)

      val finalMerged = addedPatterns.toSeq ++ mapPatternsLength5.toSeq
      val finalGrouped = finalMerged.groupBy(_._1)

      val finalMap = finalGrouped.mapValues(_.map(_._2).toList.head.split("").sorted.mkString).map(_.swap)
      println(finalMap)

      val decodedNumber = display.signal2.map(item => {
        val sortedString = item.split("").toList.sorted.mkString
        finalMap.getOrElse(sortedString, 0).toString
      }).mkString.toInt



     decodedNumber
      //println(mapPatternsLength6)
      //println(cleaned)

    })

    listDecodedNumber.sum
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
