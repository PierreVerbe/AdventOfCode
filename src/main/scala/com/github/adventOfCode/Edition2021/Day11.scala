package com.github.adventOfCode.Edition2021

import com.github.adventOfCode.ToolAOC

object Day11 {
  case class Position(x: Int, y: Int)
  case class Octopus(value: Int, numberFlashes: Int)

  def recFlashedOctopus(input: Map[Position, Octopus], flashes: Int): (Map[Position, Octopus], Int) = {
    val filteredFlashed = input.filter( item => item._2.value >= 9).toList
      .flatMap(item => {
        val leftItem = if (item._1.x - 1 > 0) List((Position(item._1.x - 1, item._1.y), Octopus(0, 0))) else List()
        val rightItem = if (item._1.x + 1 < 10) List((Position(item._1.x + 1, item._1.y), Octopus(0, 0))) else List()
        val upItem = if (item._1.y - 1 > 0) List((Position(item._1.x, item._1.y - 1), Octopus(0, 0))) else List()
        val downItem = if (item._1.y + 1 < 10) List((Position(item._1.x, item._1.y + 1), Octopus(0, 0))) else List()

        // suite
        val leftUpItem = if (item._1.x - 1 > 0 && item._1.y - 1 > 0) List((Position(item._1.x - 1, item._1.y - 1), Octopus(0, 0))) else List()
        val rightUpItem = if (item._1.x + 1 < 10 && item._1.y - 1 > 0) List((Position(item._1.x + 1, item._1.y - 1), Octopus(0, 0))) else List()
        val leftDownItem = if (item._1.x - 1 > 0 && item._1.y + 1 < 10) List((Position(item._1.x - 1, item._1.y + 1), Octopus(0, 0))) else List()
        val rightDownItem = if (item._1.x + 1 < 10 && item._1.y + 1 < 10) List((Position(item._1.x + 1, item._1.y + 1), Octopus(0, 0))) else List()

        List(item) ::: leftItem ::: rightItem ::: upItem ::: downItem ::: leftUpItem ::: rightUpItem ::: leftDownItem ::: rightDownItem
      })


    if (filteredFlashed.isEmpty) (input, flashes)
    else {
      val passesOctopusTo0 = input.map{ case(key, value) =>
        if (filteredFlashed.contains((key, value))) {
          if (value.value >= 9 && value.numberFlashes != 1) (key, Octopus(0, 1))
          else (key, Octopus(value.value + 1, value.numberFlashes))
        }
        else (key, value)
      }

      println(passesOctopusTo0)

      val numFlashes = passesOctopusTo0.toList.map(_._2.numberFlashes).sum
      recFlashedOctopus(passesOctopusTo0, numFlashes)
    }
  }

  def enigma1(input: Map[Position, Octopus]): Int = {
    val steps = (1 to 2).toList
    val result = steps.foldLeft((input, 0)) {
      (acc, num) => {
        val increasedEnergy = acc._1.mapValues(octopus => {
          Octopus(octopus.value + 1, octopus.numberFlashes)
        })

       recFlashedOctopus(increasedEnergy, acc._2)
      }
    }

    println(result._2) // == 35



    0
  }

    /*
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

     */

  def parseLine(line: (String, Int)): List[(Position, Octopus)] = {
    val octopuses = line._1.split("").zipWithIndex.toList

    octopuses.map(item => (Position(line._2, item._2), Octopus(item._1.toInt, 0)))
  }

  def main(args: Array[String]) {
    val toolAOC = new ToolAOC("src/main/resources/Edition2021/Day11.txt")
    val input = toolAOC.getInputfile().zipWithIndex.flatMap(parseLine).toMap

    // Enigma 1
    val result1 = enigma1(input)
    println(s"$result1")

    // Enigma 2
    //val result2 = enigma2(input)
    //println(s"$result2")
  }

}
