package com.github.adventOfCode.Edition2021

import com.github.adventOfCode.ToolAOC

object Day9 {

  case class Position(x: Int, y: Int)

  def enigma1(input: Map[Position, Int]): Int = {
    val lowestPoints = input.filter(point => {
      val leftPoint = input.getOrElse(Position(point._1.x + 1, point._1.y), 10)
      val rightPoint = input.getOrElse(Position(point._1.x - 1, point._1.y), 10)
      val upPoint = input.getOrElse(Position(point._1.x, point._1.y + 1), 10)
      val downPoint = input.getOrElse(Position(point._1.x, point._1.y - 1), 10)

      (leftPoint > point._2, rightPoint > point._2, upPoint > point._2, downPoint > point._2) match {
        case (true, true, true, true) => true
        case _ => false
      }
    })

    lowestPoints.toList.map(_._2 + 1).sum
  }

  /*
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

   */

  def parseLine(line: (String, Int)): List[(Position, Int)] = {
    val listItems = line._1.split("").toList.map(_.toInt)
    val listItemsZipped = listItems.zipWithIndex

    listItemsZipped.map(item => (Position(item._2, line._2), item._1))
  }

  def main(args: Array[String]) {
    val toolAOC = new ToolAOC("src/main/resources/Edition2021/Day9.txt")
    val input = toolAOC.getInputfile()
      .zipWithIndex
      .flatMap(parseLine)
      .toMap

    // Enigma 1
    val result1 = enigma1(input)
    println(s"$result1")

    // Enigma 2
    //val result2 = enigma2(input)
    //println(s"$result2")
  }

}
