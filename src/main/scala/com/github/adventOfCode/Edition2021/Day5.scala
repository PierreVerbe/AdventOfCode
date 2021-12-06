package com.github.adventOfCode.Edition2021

import com.github.adventOfCode.ToolAOC

object Day5 {
  case class Position(x: Int, y: Int)
  case class Direction(position1: Position, position2: Position)

  def enigma1(input: List[Direction]): Int = {
    val filteredInput = input.filter(item => item.position1.x == item.position2.x || item.position1.y == item.position2.y)

    val result = filteredInput.foldLeft(List[(Position, Int)]()) {
        (acc, num) => {
          if (num.position1.x == num.position2.x) {
            val pos1y = num.position1.y
            val pos2y = num.position2.y

            val rangeList = if (pos1y < pos2y) (pos1y to pos2y).toList else (pos2y to pos1y).toList
            val positions = rangeList.map(item => (Position(num.position1.x, item), 1))
            acc ::: positions
          }
          else {
            val pos1x = num.position1.x
            val pos2x = num.position2.x

            val rangeList = if (pos1x < pos2x) (pos1x to pos2x).toList else (pos2x to pos1x).toList
            val positions = rangeList.map(item => (Position(item, num.position1.y), 1))
            acc ::: positions
          }
        }
      }

    val resultGrouped = result.groupBy(_._1).mapValues(item => item.map(_._2).sum)
    resultGrouped.count(item => item._2 > 1)
  }

  def enigma2(input: List[Direction]): Int = {
    val filteredInput = input.filter(item => item.position1.x == item.position2.x
      || item.position1.y == item.position2.y
      || Math.abs(item.position1.x - item.position2.x) == Math.abs(item.position1.y - item.position2.y))

    println(filteredInput)
    val result = filteredInput.foldLeft(List[(Position, Int)]()) {
      (acc, num) => {
        val pos1x = num.position1.x
        val pos2x = num.position2.x
        val pos1y = num.position1.y
        val pos2y = num.position2.y

        if (pos1x == pos2x) {
          val rangeList = if (pos1y < pos2y) (pos1y to pos2y).toList else (pos2y to pos1y).toList
          val positions = rangeList.map(item => (Position(pos1x, item), 1))
          acc ::: positions
        }
        else if (pos1y == pos2y) {
          val rangeList = if (pos1x < pos2x) (pos1x to pos2x).toList else (pos2x to pos1x).toList
          val positions = rangeList.map(item => (Position(item, pos1y), 1))
          acc ::: positions
        }
        else {
          val listX = if(pos1x < pos2x) (pos1x to pos2x).toList else (pos2x to pos1x).toList
          val listY = if(pos1y < pos2y) (pos1y to pos2y).toList else (pos2y to pos1y).toList
          val positions = listX.zip(listY).map(item => (Position(item._1, item._2), 1))
          acc ::: positions
        }
      }
    }

    val resultGrouped = result.groupBy(_._1).mapValues(item => item.map(_._2).sum)
    resultGrouped.count(item => item._2 > 1)
  }

  def parseLine(line: String): Direction = {
    val directions = line.split(" -> ").toList
    val direction1 = directions.head.split(",").toList
    val direction2 = directions(1).split(",").toList

    Direction(Position(direction1.head.toInt, direction1(1).toInt), Position(direction2.head.toInt, direction2(1).toInt))
  }

  def main(args: Array[String]) {
    val toolAOC = new ToolAOC("src/main/resources/Edition2021/Day5.txt")
    val input = toolAOC.getInputfile().map(parseLine)

    // Enigma 1
    val result1 = enigma1(input)
    println(s"$result1")

    // Enigma 2
    val result2 = enigma2(input)
    println(s"$result2") //18864
  }

}
