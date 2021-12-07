package com.github.adventOfCode.Edition2021

import com.github.adventOfCode.ToolAOC

object Day4 {
  case class Bingo(randomNumbers: List[Int], Boards: List[List[List[AnyVal]]])

  def parseBingo(line: List[String]): Bingo = {
    val randomNumbers = line.head.split(",").toList.map(_.toInt)
    val boards = line.drop(2).foldLeft(List(List(List[Int]()))) {
      (acc, num) => {
        if(num == "") acc :+ List(List())
        else {
          val listSplit = num.split(" +").toList
          val listNumber = if(listSplit.head == "") listSplit.drop(1).map(_.toInt) else listSplit.map(_.toInt)
          val lastElement = acc.last :+ listNumber
          acc.dropRight(1) :+ lastElement
        }
      }
    }.map(_.filter(_.nonEmpty))

    Bingo(randomNumbers, boards)
  }

  def isBingoWin(board: List[List[AnyVal]]): Boolean = {
    val isBingoLine = board.exists(line => line.forall(_ == line.head))
    val isBingoColumn = board.transpose.exists(line => line.forall(_ == line.head))

    isBingoLine || isBingoColumn
  }

  def enigma1(bingo: Bingo): Int = {
    val result = bingo.randomNumbers.foldLeft((0, bingo.Boards)) {
      (acc, num) => {
        if (acc._2.length == 1) acc
        else {
          val updatedBingoBoards = acc._2.map(board => board.map(line => line.map(number => if (num == number) true else number)))
          val BingoBoard = updatedBingoBoards.filter(isBingoWin)
          if (BingoBoard.nonEmpty) (num, BingoBoard) else (num, updatedBingoBoards)
        }
      }
    }

    val sumUnmarkedNumbers = result._2.head.map(line =>
      line.map(number =>
        if (number == true) 0 else number.asInstanceOf[Int]
      ).sum).sum

    sumUnmarkedNumbers * result._1
  }

  def enigma2(input: List[List[Int]]): Int = {
    def filterListFromColumn(nColumn: Int, input: List[List[Int]]): List[List[Int]] = {
      val length = input.length
      val sumNList = input.transpose.map(_.sum)
      val gammaRateBinary = sumNList.map(item => if (item < length-item) 0 else 1)

      if (gammaRateBinary(nColumn) == 0) input.filter(item => item(nColumn) == 0).take(length-sumNList(nColumn))
      else input.filter(item => item(nColumn) == 1).take(sumNList(nColumn))
    }

    def filterListFromColumnNot(nColumn: Int, input: List[List[Int]]): List[List[Int]] = {
      val length = input.length
      val sumNList = input.transpose.map(_.sum)
      val gammaRateBinary = sumNList.map(item => if (item < length-item) 1 else 0)

      if (gammaRateBinary(nColumn) == 0) input.filter(item => item(nColumn) == 0).take(length-sumNList(nColumn))
      else input.filter(item => item(nColumn) == 1).take(sumNList(nColumn))
    }

    val listRange = List.range(0, input.head.length)
    val oxygenGeneratorRateBinary = listRange.foldLeft(input) {
      (acc, num) => {
        if (acc.length == 1) acc
        else filterListFromColumn(num, acc)
      }
    }
    val co2ScrubberRateBinary = listRange.foldLeft(input) {
      (acc, num) => {
        if (acc.length == 1) acc
        else filterListFromColumnNot(num, acc)
      }
    }

    val oxygenGeneratorRate = Integer.parseInt(oxygenGeneratorRateBinary.head.mkString, 2)
    val co2ScrubberRate = Integer.parseInt(co2ScrubberRateBinary.head.mkString, 2)

    oxygenGeneratorRate * co2ScrubberRate
  }



  def main(args: Array[String]) {
    val toolAOC = new ToolAOC("src/main/resources/Edition2021/Day4.txt")
    val bingo = parseBingo(toolAOC.getInputfile())

    // Enigma 1
    val result1 = enigma1(bingo)
    println(s"$result1")

    // Enigma 2
    val result2 = enigma2(bingo)
    println(s"$result2")
  }

}
