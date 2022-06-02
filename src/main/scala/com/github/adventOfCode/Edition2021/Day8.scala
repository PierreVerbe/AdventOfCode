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
      val listPatternLength = display.signal1.map(pattern => (pattern, pattern.length))
      val mapEasyPattern = listPatternLength.flatMap(pattern => {
        pattern._2 match {
          case 2 => List((1, pattern._1 ))
          case 3 => List((7, pattern._1 ))
          case 4 => List((4, pattern._1 ))
          case 7 => List((8, pattern._1 ))
          case _ => List()
        }
      }).toMap

      val mapPatternsLength6 = listPatternLength.filter(_._2 == 6).map( pattern => {
        val splitedPattern = pattern._1.split("").toList
        val isPattern0 = mapEasyPattern.getOrElse(1, "")
          .split("").toList
          .map(splitedPattern.contains(_))
          .reduce((x, y) => x && y)
        val isPattern9 = mapEasyPattern.getOrElse(4, "")
          .split("").toList
          .map(splitedPattern.contains(_))
          .reduce((x, y) => x && y)

        (isPattern0, isPattern9) match {
          case (true, true) => (9, pattern._1)
          case (false, false) => (6, pattern._1)
          case _ => (0, pattern._1)
        }
      }).toMap

      val seqPatternAppended = mapEasyPattern.toSeq ++ mapPatternsLength6.toSeq
      val mapPatternCompletedGrouped = seqPatternAppended.groupBy(_._1)
      val mapPatternCompleted = mapPatternCompletedGrouped.mapValues(_.map(_._2).toList.head)

      val mapPatternsLength5 = listPatternLength.filter(_._2 == 5).map( pattern => {
        val splitedPattern = pattern._1.split("").toList
        val isPattern3 = mapEasyPattern.getOrElse(1, "")
          .split("").toList
          .map(splitedPattern.contains(_))
          .reduce((x, y) => x && y)
        val isPattern2 = {
          val pattern8 = mapEasyPattern.getOrElse(8, "").split("").toList
          val pattern9 = mapPatternCompleted.getOrElse(9, "").split("").toList
          val bottomLeft = pattern8.flatMap(item => if (pattern9.contains(item)) List() else List(item))
          splitedPattern.contains(bottomLeft.head)
        }

        (isPattern2, isPattern3) match {
          case (false, true) => (3, pattern._1)
          case (true, false) => (2, pattern._1)
          case _ => (5, pattern._1)
        }
      }).toMap

      val seqPatternFinalMerged = mapPatternCompleted.toSeq ++ mapPatternsLength5.toSeq
      val mapPatternFinalGrouped = seqPatternFinalMerged.groupBy(_._1)
      val mapPatternFinal = mapPatternFinalGrouped.mapValues( value =>
        value.map(_._2).head.split("").sorted.mkString
      ).map(_.swap)

      display.signal2.map(item => {
        val sortedString = item.split("").sorted.mkString
        mapPatternFinal.getOrElse(sortedString, 0).toString
      }).mkString.toInt
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
