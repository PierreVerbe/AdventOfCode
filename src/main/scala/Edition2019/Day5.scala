package Edition2019

import scala.io.Source

object Day5 {

  def main(args: Array[String]): Unit = {
    def calculationGravity(theList : List[Int], position : Int, input : Int): Int = {
      val convertOperation = convertPosition(theList(position))
      //println(convertOperation)

      //analyse the instruction
      val operation = convertOperation.substring(3, 5).toInt
      val modeParam1 = convertOperation.substring(2, 3).toInt
      val modeParam2 = convertOperation.substring(1, 2).toInt
      val modeParam3 = convertOperation.substring(0, 1).toInt

      if (operation == 99) theList.head

      else if (operation == 1) {
        val value1 = theList(position + 1)
        val value2 = theList(position + 2)
        val positionReplace = theList(position + 3)

        val res1 = if (modeParam1 == 0) theList(value1) else value1
        val res2 = if (modeParam2 == 0) theList(value2) else value2

        val newValue = res1 + res2
        val newList = theList.updated(positionReplace, newValue)
        calculationGravity(newList, position + 4, input)
      }

      else if (operation == 2){
        val value1 = theList(position + 1)
        val value2 = theList(position + 2)
        val positionReplace = theList(position + 3)

        val res1 = if (modeParam1 == 0) theList(value1) else value1
        val res2 = if (modeParam2 == 0) theList(value2) else value2

        val newValue = res1 * res2
        val newList = theList.updated(positionReplace, newValue)
        calculationGravity(newList, position + 4, input)
      }

      else if (operation == 3){
        val value1 = theList(position + 1)
        val newList = theList.updated(value1, input)
        calculationGravity(newList, position + 2, input)
      }

        //4
      else {
        val value1 = theList(position + 1)
        println(value1)
        calculationGravity(theList, position + 2, input)
      }

    }

    def convertPosition(number: Int): String ={
      val numberFigure = number.toString.length

      numberFigure match {
        case 1 => "0000" + number
        case 2 => "000" + number
        case 3 => "00" + number
        case 4 => "0" + number
        case 5 => number.toString
        case _ => number.toString
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

    val filename = "src/main/resources/Edition2019/Day5.txt";

    val lineList = Source.fromFile(filename).getLines.toList
    val listString = lineList.flatMap(_.split(",").toList)
    val listInt = listString.map(_.toInt)
    //val verifyList = listInt.updated(1,12).updated(2,2)

    val test1 = List(3,0,4,0,99)

    println("Part test, result = " + calculationGravity(test1, 0, 77))
    println(calculationGravity(listInt, 0, 1))
    println("Part 1, result = " + calculationGravity(listInt, 0, 1))
    //println("Part 2, result = " + calculationGravityPart2(listInt, 0, 0))
  }

}