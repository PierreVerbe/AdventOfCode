package com.github.adventOfCode.Edition2019

object Day4 {
  def main(args: Array[String]): Unit = {

    def adjacentDigits(elem: Int, positionChar: Int): Boolean = {
      if (positionChar == 5) false
      else if (elem.toString.charAt(positionChar).equals(elem.toString.charAt(positionChar + 1))) true
      else adjacentDigits(elem, positionChar + 1)
    }

    def neverDecrease(elem: Int, positionChar: Int, min: Int): Boolean = {
      if (positionChar == 6) true
      else if (elem.toString.charAt(positionChar).toInt >= min) neverDecrease(elem, positionChar + 1, elem.toString.charAt(positionChar).toInt)
      else false
    }

    def adjacentDigitsTriple(elem: Int, positionChar: Int): Boolean = {
      if (positionChar == 5) false
      else if (elem.toString.charAt(positionChar).equals(elem.toString.charAt(positionChar + 1))) {
        if (positionChar == 0 && !(elem.toString.charAt(0).equals(elem.toString.charAt(2)))) true
        else if (positionChar == 4 && !(elem.toString.charAt(positionChar).equals(elem.toString.charAt(positionChar - 1)))) true
        else if (positionChar != 0 && !(elem.toString.charAt(positionChar).equals(elem.toString.charAt(positionChar - 1))) && !(elem.toString.charAt(positionChar).equals(elem.toString.charAt(positionChar + 2)))) true
        else adjacentDigitsTriple(elem, positionChar + 1)
      }
      else adjacentDigitsTriple(elem, positionChar + 1)
    }

    val firstRange = 246515
    val secondRange = 739105
    val rangeOfNumber = (firstRange to secondRange by 1).toList

    val rangeOfnumberDouble = rangeOfNumber.filter(x => adjacentDigits(x, 0))
    val result = rangeOfnumberDouble.filter(x => neverDecrease(x, 0, 0))
    println("Part 1, result = " + result.size)

    val result2 = result.filter(x => adjacentDigitsTriple(x, 0))
    println("Part 2, result = " + result2.size)
  }
}