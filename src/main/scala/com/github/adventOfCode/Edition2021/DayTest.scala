package com.github.adventOfCode.Edition2021

import org.apache.spark.SparkContext

object DayTest {

  def parseLine(line: String): (Int, Double) = {
    val fields = line.split(",")
    (0, 0.0)
  }


  def main(args: Array[String]) {

    val filename = "src/main/resources/Edition2021/Day1.txt"


    val sc = new SparkContext("local[*]", "AdventOfCode Day1")
    val input = sc.textFile("")
    val mappedInput = input.map(parseLine)



  }


}
