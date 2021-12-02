package com.github.adventOfCode.Edition2019

import scala.io.Source

object Day16 {

  def main(args: Array[String]): Unit = {



    def generatePattern(phase: Int, size: Int): Vector[Int] = {
      def fillPatern(main: Vector[Int], next: Vector[Int]): Vector[Int] = {
        main ++ Vector.fill(163)(next).flatten
      }

      val start = Vector.fill(phase - 1)(0)
      val inside = Vector.fill(phase)(1) ++ Vector.fill(phase)(0) ++ Vector.fill(phase)(-1) ++ Vector.fill(phase)(0)

      val intermediare = phase match {
        case 1 => Vector(1, 0, -1, 0)
        case _ => start ++ inside
      }

      fillPatern(intermediare, inside).take(size)
    }

    def calculationPhase(inputSignal: Vector[Int], actualPhase: Int = 1): Vector[Int] ={
      def calculation(inputSignal: Vector[Int], pattern: Vector[Int]): Int ={
        val outputSignal = inputSignal.zip(pattern).map { case (a, b) => a * b }.sum.toString
        val outputSignalChar= outputSignal.charAt(outputSignal.length - 1)
        outputSignalChar.asDigit
      }

      def insidePhase(intermediareSignal: Vector[Int], intermediareResult: Vector[Int] = Vector(), nbInsidePhase: Int = 1): Vector[Int] ={
        if (nbInsidePhase > intermediareSignal.size) intermediareResult

        else {
          val pattern = generatePattern(nbInsidePhase, intermediareSignal.size)
          val resultInsidePhase = calculation(intermediareSignal, pattern)
          insidePhase(intermediareSignal, intermediareResult ++ Vector(resultInsidePhase), nbInsidePhase + 1)
        }
      }

      println(actualPhase)
      if (actualPhase > 100) inputSignal

      else {
        val phaseN = insidePhase(inputSignal, Vector())
        calculationPhase(phaseN, actualPhase + 1)
      }
    }

    /*def repeated10000Times(main: Vector[Int], actual: Int): Vector[Int] ={
      main ++
    }*/

    val filename = "src/main/resources/Edition2019/Day16_1.txt";
    val lineString = Source.fromFile(filename).getLines.toList.mkString
    val lineVector = lineString.toVector.map(_.toString.toInt)

    /*val test1 = "98765432109876543210".toVector.map(_.toString.toInt)
    println(test1)
    println(test1.slice(7, 15))*/

    val resultPart1 = calculationPhase(lineVector).take(8).map(_.toString).mkString
    println("Part 1, result = " + resultPart1)

    //val test =

    val lineVector10000 = Vector.fill(10000)(lineVector).flatten
    val offsetResultPart2 = lineVector.take(7).map(_.toString).mkString.toInt
    val interessantResultPart2 = lineVector10000.take(offsetResultPart2 + 8)
    val beforeResultPart2 = calculationPhase(interessantResultPart2)

    val resultPart2 = beforeResultPart2.drop(offsetResultPart2)

    println("Part 2, result = " + resultPart2)
  }
}