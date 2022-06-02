package com.github.adventOfCode

import scala.io.Source

class ToolAOC(filename: String) {

  def getInputfile(): List[String] ={
    val source = Source.fromFile(filename)
    val inputList = source.getLines.toList
    source.close()

    inputList
  }

}
