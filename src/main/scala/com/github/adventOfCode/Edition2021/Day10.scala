package com.github.adventOfCode.Edition2021

import com.github.adventOfCode.ToolAOC

object Day10 {

  case class Position(x: Int, y: Int)

  def getLowestPoints(input: Map[Position, Int]): Map[Position, Int] = {
    input.filter(point => {
      val leftPoint = input.getOrElse(Position(point._1.x + 1, point._1.y), 10)
      val rightPoint = input.getOrElse(Position(point._1.x - 1, point._1.y), 10)
      val upPoint = input.getOrElse(Position(point._1.x, point._1.y + 1), 10)
      val downPoint = input.getOrElse(Position(point._1.x, point._1.y - 1), 10)

      (leftPoint > point._2, rightPoint > point._2, upPoint > point._2, downPoint > point._2) match {
        case (true, true, true, true) => true
        case _ => false
      }
    })
  }

  def enigma1(input: List[List[String]]): Int = {
    val evenInput = input.filter(line => {
      val result = line.map(item => if (item == "[" || item == "(" || item == "{" || item == "<") 1 else -1).sum
      println(result)
      result != 0
    })
    println(evenInput.map(_.mkString))



    0
  }

  def enigma2(input: Map[Position, Int]): Int = {
    def recSearchBasin(position: Position, acc: List[Position]): List[Position] = {
      val leftPoint = input.getOrElse(Position(position.x + 1, position.y), 10)
      val rightPoint = input.getOrElse(Position(position.x - 1, position.y), 10)
      val upPoint = input.getOrElse(Position(position.x, position.y + 1), 10)
      val downPoint = input.getOrElse(Position(position.x, position.y - 1), 10)

      val isSearchLeft = ! acc.contains(Position(position.x + 1, position.y)) && leftPoint != 9 && leftPoint != 10
      val listLeftPoint = if (isSearchLeft) recSearchBasin(Position(position.x + 1, position.y), acc :+ Position(position.x + 1, position.y)) else acc

      val isSearchRight = ! listLeftPoint.contains(Position(position.x - 1, position.y)) && rightPoint != 9 && rightPoint != 10
      val listRightPoint = if (isSearchRight) recSearchBasin(Position(position.x - 1, position.y), listLeftPoint :+ Position(position.x - 1, position.y)) else listLeftPoint

      val isSearchUp = ! listRightPoint.contains(Position(position.x, position.y + 1)) && upPoint != 9 && upPoint != 10
      val listUpPoint = if (isSearchUp) recSearchBasin(Position(position.x, position.y + 1), listRightPoint :+ Position(position.x, position.y + 1)) else listRightPoint

      val isSearchDown = ! listUpPoint.contains(Position(position.x, position.y - 1)) && downPoint != 9 && downPoint != 10
      val listDownPoint = if (isSearchDown) recSearchBasin(Position(position.x, position.y - 1), listUpPoint :+ Position(position.x, position.y - 1)) else listUpPoint

      listDownPoint
    }

    val lowestPoints = getLowestPoints(input).toList
    val listBasinLength = lowestPoints.map(lowestPoint => {
      val basinMixed = recSearchBasin(lowestPoint._1, List())
      val basin = basinMixed.distinct

      basin.length
    })

    val top3Basin = listBasinLength.sorted.reverse.take(3)
    top3Basin.product
  }

  def main(args: Array[String]) {
    val toolAOC = new ToolAOC("src/main/resources/Edition2021/Day10.txt")
    val input = toolAOC.getInputfile().map(_.split("").toList)

    println(input)

    // Enigma 1
    val result1 = enigma1(input)
    println(s"$result1")

    // Enigma 2
    //val result2 = enigma2(input)
    //println(s"$result2")
  }


}
