package com.github.adventOfCode.Edition2021

import com.github.adventOfCode.ToolAOC

object Day2 {
  case class PositionSubmarine(horizontal: Int, depth: Int)
  case class PositionSubmarine2(horizontal: Int, depth: Int, aim: Int)
  case class Instruction(direction: String, value: Int)

  def enigma1(input: List[Instruction]): Int = {
    val position = input.foldLeft(PositionSubmarine(0,0))((position, item) =>
      item match {
        case Instruction("forward", _) => PositionSubmarine(position.horizontal + item.value, position.depth)
        case Instruction("up", _) => PositionSubmarine(position.horizontal, position.depth - item.value)
        case Instruction("down", _) => PositionSubmarine(position.horizontal, position.depth + item.value)
        case _ => PositionSubmarine(position.horizontal, position.depth)
      }
    )

    position.horizontal * position.depth
  }

  def enigma2(input: List[Instruction]): Int = {
    val position = input.foldLeft(PositionSubmarine2(0,0,0))((position, item) =>
      item match {
        case Instruction("forward", _) => PositionSubmarine2(position.horizontal + item.value, position.depth + (position.aim * item.value), position.aim)
        case Instruction("up", _) => PositionSubmarine2(position.horizontal, position.depth, position.aim - item.value)
        case Instruction("down", _) => PositionSubmarine2(position.horizontal, position.depth, position.aim + item.value)
        case _ => PositionSubmarine2(position.horizontal, position.depth, position.aim)
      }
    )

    position.horizontal * position.depth
  }

  def parseLine(line: String): Instruction = {
    val fields = line.split(" ")
    Instruction(fields(0), fields(1).toInt)
  }

  def main(args: Array[String]) {
    val toolAOC = new ToolAOC("src/main/resources/Edition2021/Day2.txt")
    val input = toolAOC.getInputfile().map(parseLine)

    // Enigma 1
    val result1 = enigma1(input)
    println(s"$result1")

    // Enigma 2
    val result2 = enigma2(input)
    println(s"$result2")
  }

}
