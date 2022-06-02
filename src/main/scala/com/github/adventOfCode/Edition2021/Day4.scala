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
          val bingoBoard = updatedBingoBoards.filter(isBingoWin)
          if (bingoBoard.nonEmpty) (num, bingoBoard) else (num, updatedBingoBoards)
        }
      }
    }

    val sumUnmarkedNumbers = result._2.head.map(line =>
      line.map(number =>
        if (number == true) 0 else number.asInstanceOf[Int]
      ).sum).sum

    sumUnmarkedNumbers * result._1
  }

  def enigma2(bingo: Bingo): Int = {
    val result = bingo.randomNumbers.foldLeft((0, bingo.Boards)) {
      (acc, num) => {
        if (acc._2.length == 1 && isBingoWin(acc._2.head)) acc
        else {
          val updatedBingoBoards = acc._2.map(board => board.map(line => line.map(number => if (num == number) true else number)))
          val remainingBingoBoards = updatedBingoBoards.filter(! isBingoWin(_))
          if (remainingBingoBoards.nonEmpty) (num, remainingBingoBoards) else (num, updatedBingoBoards)
        }
      }
    }

    val sumUnmarkedNumbers = result._2.head.map(line =>
      line.map(number =>
        if (number == true) 0 else number.asInstanceOf[Int]
      ).sum).sum

    sumUnmarkedNumbers * result._1
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
