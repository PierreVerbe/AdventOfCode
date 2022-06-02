package com.github.adventOfCode.Edition2019

import scala.io.Source

object Day2 {

  def main(args: Array[String]): Unit = {

    def calculationGravity(theList : List[Int], position : Int): Int = {
      val operation = theList(position)
      val value1 = theList(position + 1)
      val value2 = theList(position + 2)
      val positionReplace = theList(position + 3)

      if (operation == 99) theList.head

      else if (operation == 1) {
        val newValue = theList(value1) + theList(value2)
        val newList = theList.updated(positionReplace, newValue)
        calculationGravity(newList, position + 4)
      }

      else {
        val newValue = theList(value1) * theList(value2)
        val newList = theList.updated(positionReplace, newValue)
        calculationGravity(newList, position + 4)
      }

    }


    def calculationGravityPart2(theList : List[Int], noun : Int, verb : Int): Int = {
      def innerCalculationGravityPart2(theInnerList: List[Int], position: Int): Int = {
        val operation = theInnerList (position)
        val value1 = theInnerList (position + 1)
        val value2 = theInnerList (position + 2)
        val positionReplace = theInnerList (position + 3)

        if (operation == 99) theInnerList.head

        else if (operation == 1) {
        val newValue = theInnerList (value1) + theInnerList (value2)
        val newList = theInnerList.updated (positionReplace, newValue)
        innerCalculationGravityPart2 (newList, position + 4)
       }

        else {
        val newValue = theInnerList (value1) * theInnerList (value2)
        val newList = theInnerList.updated (positionReplace, newValue)
        innerCalculationGravityPart2 (newList, position + 4)
        }
      }

      println("noun = " + noun + " ; " + " verb = " + verb + " ; " + " result = " + (100 * noun + verb))
      val theListUpdated = theList.updated(1, noun).updated(2, verb)

      if (verb >= 100) calculationGravityPart2(theListUpdated, noun + 1, 0)

      else if (innerCalculationGravityPart2(theListUpdated, 0) == 19690720) {
        val result = 100 * noun + verb
        result
      }

      else calculationGravityPart2(theList, noun, verb + 1)

    }

    val filename = "src/main/resources/Edition2019/Day2.txt";

    val lineList = Source.fromFile(filename).getLines.toList
    val listString = lineList.flatMap(_.split(",").toList)
    val listInt = listString.map(_.toInt)
    val verifyList = listInt.updated(1,12).updated(2,2)

    val test1 = List(1,9,10,3,2,3,11,0,99,30,40,50)

    println("Part test, result = " + calculationGravity(test1, 0))
    println("Part 1, result = " + calculationGravity(verifyList, 0))
    println("Part 2, result = " + calculationGravityPart2(verifyList, 0, 0))

  }

}