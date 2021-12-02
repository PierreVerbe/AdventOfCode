package com.github.adventOfCode.Edition2019

import scala.io.Source

object Day2B extends App {

    def calculationGravityVector(theVector : Vector[Int], position : Int): Int = {
      val operation = theVector(position)

      operation match {
        case 1 =>
          val inputValue = (theVector(position + 1), theVector(position + 2), theVector(position + 3))
          val newValue = theVector(inputValue._1) + theVector(inputValue._2)
          val newVector = theVector.updated(inputValue._3, newValue)
          calculationGravityVector(newVector, position + 4)

        case 2 =>
          val inputValue = (theVector(position + 1), theVector(position + 2), theVector(position + 3))
          val newValue = theVector(inputValue._1) * theVector(inputValue._2)
          val newVector = theVector.updated(inputValue._3, newValue)
          calculationGravityVector(newVector, position + 4)

        case 99 => theVector.head

        case _ =>
          print("error")
          -1
      }
    }

  def calculationGravityVector2(theVector : Vector[Int], input: Int, position : Int): Int = {
    val operation = theVector(position)

    operation match {
      case 1 =>
        val inputValue = (theVector(position + 1), theVector(position + 2), theVector(position + 3))
        val newValue = theVector(inputValue._1) + theVector(inputValue._2)
        val newVector = theVector.updated(inputValue._3, newValue)
        calculationGravityVector2(newVector, input, position + 4)

      case 2 =>
        val inputValue = (theVector(position + 1), theVector(position + 2), theVector(position + 3))
        val newValue = theVector(inputValue._1) * theVector(inputValue._2)
        val newVector = theVector.updated(inputValue._3, newValue)
        calculationGravityVector2(newVector, input, position + 4)

      case 3 =>
        val inputValue = theVector(position + 1)
        val newVector = theVector.updated(inputValue, input)
        calculationGravityVector2(newVector, input, position + 2)

      case 4 =>
        val inputValue = theVector(position + 1)
        calculationGravityVector2(theVector, theVector(inputValue), position + 2)

      case 99 => theVector.head

      case _ =>
        print("error")
        -1
    }
  }

  def diagnosticCode(theVector : Vector[Int], input: Int, position : Int): Int = {
    def convertPosition(number: Int): String = {
      val numberFigure = number.toString.length

      numberFigure match {
        case 1 => "0000" + number
        case 2 => "000" + number
        case 3 => "00" + number
        case 4 => "0" + number
        case 5 => number.toString
        case _ =>
          println("error convert poisiton function")
          number.toString
      }
    }

    def isImmediateMode(number: String): (Boolean, Boolean, Boolean) = {
      val modeParam1 = if (number.substring(2, 3).toInt == 1) true else false
      val modeParam2 = if (number.substring(1, 2).toInt == 1) true else false
      val modeParam3 = if (number.substring(0, 1).toInt == 1) true else false
      (modeParam1, modeParam2, modeParam3)
    }

    val preOperation = convertPosition(theVector(position))
    val modeNumber = isImmediateMode(preOperation)
    val operation = preOperation.substring(3, 5).toInt
    println(operation)
    println(modeNumber)

    operation match {
      case 1 =>
        val inputValue = (if (modeNumber._1) theVector(position + 1) else theVector(theVector(position + 1)) , if (modeNumber._2) theVector(position + 2) else theVector(theVector(position + 2)), theVector(theVector(position + 3)))
        val newValue = inputValue._1 + inputValue._2
        val newVector = theVector.updated(inputValue._3, newValue)
        diagnosticCode(newVector, input, position + 4)

      case 2 =>
        val inputValue = (theVector(position + 1), theVector(position + 2), theVector(position + 3))
        val newValue = theVector(inputValue._1) * theVector(inputValue._2)
        val newVector = theVector.updated(inputValue._3, newValue)
        diagnosticCode(newVector, input, position + 4)

      case 3 =>
        val inputValue = theVector(position + 1)
        val newVector = theVector.updated(inputValue, input)
        diagnosticCode(newVector, input, position + 2)

      case 4 =>
        val inputValue = theVector(position + 1)
        diagnosticCode(theVector, theVector(inputValue), position + 2)

      case 99 => theVector.head

      case _ =>
        print("error")
        -1
    }
  }


  val filename = "src/main/resources/Edition2019/Day2.txt";

  val vectorLine = Source.fromFile(filename).getLines.toVector
  val vectorString = vectorLine.flatMap(_.split(",").toVector)
  val vectorInt = vectorString.map(_.toInt)
  val vectorVerify = vectorInt.updated(1,12).updated(2,2)

  println(vectorVerify)
  println("Part 1, result = " + calculationGravityVector(vectorVerify, 0))
  val input1 = Vector(1101,0,4,0,99)
  println("ft " + diagnosticCode(input1, 5555, 0))

}
