package Edition2019.IdeaBox

import scala.io.Source

object Day3Tab {
  def main(args: Array[String]): Unit = {

    val filename = "src/main/resources/Edition2019/Day3.txt";

    val lineList = Source.fromFile(filename).getLines.toList
    val wire1 = lineList.head.split(",").toList
    val wire2 = lineList(1).split(",").toList

    println(wire1)
    println(wire2)

    def writeWire(thelist: List[String], tableau: Array[Array[Char]]): Array[Array[Char]] = {

      def innerWriteWire(innerList: List[String], innerTableau: Array[Array[Char]], x: Int, y: Int): (Int, Int, Array[Array[Char]]) = {
        if (innerList.isEmpty) {
          (x, y, innerTableau)
        }


        else {
          val elementList = thelist.head
          val numberMove = elementList.substring(1).toInt

          val fields = if (elementList.startsWith("R")) {
            val innerNewTab = writeRight(innerTableau, x, y, numberMove)
            val newX = x
            val newY = y + numberMove
            (newX, newY, innerNewTab)
            //if ( case != null)
          }

          else if (elementList.startsWith("L")) {
            val innerNewTab = writeLeft(innerTableau, x, y, numberMove)
            val newX = x
            val newY = y - numberMove
            (newX, newY, innerNewTab)
          }

          else if (elementList.startsWith("U")) {
            val innerNewTab = writeUp(innerTableau, x, y, numberMove)
            val newX = x - numberMove
            val newY = y
            (newX, newY, innerNewTab)
          }

          else {
            val innerNewTab = writeDown(innerTableau, x, y, numberMove)
            val newX = x + numberMove
            val newY = y
            (newX, newY, innerNewTab)
          }

          innerWriteWire(innerList.tail, fields._3, fields._1, fields._2)
        }
      }

      val result = innerWriteWire(thelist, tableau, 50000, 50000)
      result._3
    }

    def writeRight(tab: Array[Array[Char]], x: Int, y: Int, nb: Int): Array[Array[Char]] ={
      if (nb == 0) tab

      else if (nb == 1) {
        tab(x)(y+1) = '+'
        val tabnew = tab
        writeRight(tabnew, x, y+1, nb-1)
      }

      else {
        tab(x)(y+1) = '-'
        val tabnew = tab
        writeRight(tabnew, x, y+1, nb-1)
      }
    }

    def writeDown(tab: Array[Array[Char]], x: Int, y: Int, nb: Int): Array[Array[Char]] ={
      if (nb == 0) tab

      else if (nb == 1) {
        tab(x+1)(y) = '+'
        val tabnew = tab
        writeDown(tabnew, x+1, y, nb-1)
      }

      else {
        tab(x+1)(y) = '-'
        val tabnew = tab
        writeDown(tabnew, x+1, y, nb-1)
      }
    }

    def writeUp(tab: Array[Array[Char]], x: Int, y: Int, nb: Int): Array[Array[Char]] ={
      if (nb == 0) tab

      else if (nb == 1) {
        tab(x-1)(y) = '+'
        val tabnew = tab
        writeUp(tabnew, x-1, y, nb-1)
      }

      else {
        tab(x-1)(y) = '-'
        val tabnew = tab
        writeUp(tabnew, x-1, y, nb-1)
      }
    }

    def writeLeft(tab: Array[Array[Char]], x: Int, y: Int, nb: Int): Array[Array[Char]] ={
      if (nb == 0) tab

      else if (nb == 1) {
        tab(x)(y-1) = '+'
        val tabnew = tab
        writeLeft(tabnew, x, y-1, nb-1)
      }

      else {
        tab(x)(y-1) = '-'
        val tabnew = tab
        writeLeft(tabnew, x, y-1, nb-1)
      }
    }

    //writeWire()

    val maMatrice:Array[Array[Char]] = Array.ofDim(100000, 100000)

    val result = writeWire( wire1, maMatrice)
    println(result.deep.mkString("\n"))


    //maMatrice(1)(1) = "g"
    //println(maMatrice(1)(1))
    //println(maMatrice(0)(0))

    //val jteest = writeRight(maMatrice, 2,2, 4)
    //println(jteest.deep.mkString("\n"))
    //println()

    //println(maMatrice.deep.mkString("\n"))
    //println(maMatrice(-1)(-1))

    /*val test = "R12"
    val test2 = "R234567"
    println(test.substring(1))
    println(test2.substring(1))
    */


  }

}
