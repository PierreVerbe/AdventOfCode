package Edition2019

import scala.io.Source

object Day8 {
  def main(args: Array[String]): Unit = {

    def splitString(theString: String, theList: List[String]): List[String] = {
       if (theString.isEmpty)theList
       else {
         val elemSplit = theString.substring(0, 25*6)
         val restString = theString.substring(25*6, theString.length)
         splitString(restString, theList :+ elemSplit)
       }
    }

    def fewest0digits(theList: List[String]): String = {
      def innerFewest0Digits (theListInner: List[String], actualLayer: Int, minLayer: Int, nb0: Int): String = {
        if (theListInner.isEmpty) theList(minLayer)
        else {
          val layerNb0 = theListInner.head.count(_ == '0')
          if (layerNb0 < nb0) innerFewest0Digits(theListInner.tail, actualLayer + 1, actualLayer, layerNb0)
          else innerFewest0Digits(theListInner.tail, actualLayer + 1, minLayer, nb0)
        }
      }
    innerFewest0Digits(theList, 0, 999, 999)
    }

    def render(theList: List[String], resultLayer: String): String = {
      def renderLayer(actualLayer: String, transformLayer: String, result: String): String = {
        def renderPixel(actualPixel: Char, transformPixel: Char): Char = {
          actualPixel match {
            case '2' => transformPixel
            case _ => actualPixel
          }
        }

        if (actualLayer.isEmpty) result
        else {
          val resultChar = renderPixel(transformLayer.charAt(0), actualLayer.charAt(0))
          renderLayer(actualLayer.substring(1), transformLayer.substring(1), result + resultChar)
        }
      }

      if(theList.isEmpty) resultLayer
      else {
        val intermediaireResult = renderLayer(theList.head, resultLayer, "")
        render(theList.tail, intermediaireResult)
      }
    }

    def printpart2(theString: String): Unit = {
      println("Part 2, result = ")
      val resultPart2clean = theString.replace('0', ' ')

      println(resultPart2clean.toString.substring(0, 25))
      println(resultPart2clean.toString.substring(25, 50))
      println(resultPart2clean.toString.substring(50, 75))
      println(resultPart2clean.toString.substring(75, 100))
      println(resultPart2clean.toString.substring(100, 125))
      println(resultPart2clean.toString.substring(125, 150))
    }

    val filename = "src/main/resources/Edition2019/Day8.txt";
    val lineList = Source.fromFile(filename).getLines.toList.head.toString
    val listLayer = splitString(lineList, List())

    val layerFewest0Digits = fewest0digits(listLayer)
    val resultPart1 = layerFewest0Digits.count(_ == '1') * layerFewest0Digits.count(_ == '2')
    println("Part 1, result = " + resultPart1)

    val listTest = List("0222", "1122", "2212", "0000")
    println("result test = " + render(listTest, "2222"))

    val resultPart2 = render(listLayer, "222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222").toString
    printpart2(resultPart2)
  }
}