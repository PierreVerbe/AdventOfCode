package Edition2019

import scala.collection.immutable.HashMap
import scala.io.Source

object Day3 {
  def main(args: Array[String]): Unit = {

    val filename = "src/main/resources/Edition2019/Day3.txt";

    val lineList = Source.fromFile(filename).getLines.toList
    val wire1 = lineList.head.split(",").toList
    val wire2 = lineList(1).split(",").toList

    println(wire1)
    println(wire2)

    def writeWire(thelist: List[String], theMap: HashMap[String, Char]): HashMap[String, Char] = {

      def innerWriteWire(innerList: List[String], theInnerMap: HashMap[String, Char], x: Int, y: Int): (Int, Int, HashMap[String, Char]) = {
        if (innerList.isEmpty) {
          (x, y, theInnerMap)
        }

        else {
          val elementList = innerList.head
          val numberMove = elementList.substring(1).toInt

          val fields = if (elementList.startsWith("R")) {
            val innerNewMap = writeRight(theInnerMap, x, y, numberMove)
            val newX = x + numberMove
            val newY = y
            (newX, newY, innerNewMap)
          }

          else if (elementList.startsWith("L")) {
            val innerNewMap = writeLeft(theInnerMap, x, y, numberMove)
            val newX = x - numberMove
            val newY = y
            (newX, newY, innerNewMap)
          }

          else if (elementList.startsWith("U")) {
            val innerNewMap = writeUp(theInnerMap, x, y, numberMove)
            val newX = x
            val newY = y + numberMove
            (newX, newY, innerNewMap)
          }

          else {
            val innerNewMap = writeDown(theInnerMap, x, y, numberMove)
            val newX = x
            val newY = y - numberMove
            (newX, newY, innerNewMap)
          }

          innerWriteWire(innerList.tail, fields._3, fields._1, fields._2)
        }
      }

      val result = innerWriteWire(thelist, theMap, 0, 0)
      result._3

    }


    def writeRight(theMap: HashMap[String, Char], x: Int, y: Int, nb: Int): HashMap[String, Char] = {
      val newKey = x.toString + "," + y.toString

      if (nb == 0) theMap

      else if (nb == 1) {
        val mapNew = if (theMap.contains(newKey)) theMap + (newKey -> 'X')
        else theMap + (newKey -> '+')
        writeRight(mapNew, x+1, y, nb-1)
      }

      else {
        val mapNew = if (theMap.contains(newKey)) theMap + (newKey -> 'X')
        else theMap + (newKey -> '-')
        writeRight(mapNew, x+1, y, nb-1)
      }
    }

    def writeLeft(theMap: HashMap[String, Char], x: Int, y: Int, nb: Int): HashMap[String, Char] ={
      val newKey = x.toString + "," + y.toString

      if (nb == 0) theMap

      else if (nb == 1) {
        val mapNew = if (theMap.contains(newKey)) theMap + (newKey -> 'X')
        else theMap + (newKey -> '+')
        writeLeft(mapNew, x-1, y, nb - 1)
      }

      else {
        val mapNew = if (theMap.contains(newKey)) theMap + (newKey -> 'X')
        else theMap + (newKey -> '-')
        writeLeft(mapNew, x-1, y, nb - 1)
      }
    }

    def writeUp(theMap: HashMap[String, Char], x: Int, y: Int, nb: Int): HashMap[String, Char] ={
      val newKey = x.toString + "," + y.toString

      if (nb == 0) theMap

      else if (nb == 1) {
        val mapNew = if (theMap.contains(newKey)) theMap + (newKey -> 'X')
        else theMap + (newKey -> '+')
        writeUp(mapNew, x, y + 1, nb - 1)
      }

      else {
        val mapNew = if (theMap.contains(newKey)) theMap + (newKey -> 'X')
        else theMap + (newKey -> '|')
        writeUp(mapNew, x, y + 1, nb - 1)
      }
    }

    def writeDown(theMap: HashMap[String, Char], x: Int, y: Int, nb: Int): HashMap[String, Char] = {
      val newKey = x.toString + "," + y.toString

      if (nb == 0) theMap

      else if (nb == 1) {
        val mapNew = if (theMap.contains(newKey)) theMap + (newKey -> 'X')
        else theMap + (newKey -> '+')
        writeDown(mapNew, x, y - 1, nb - 1)
      }

      else {
        val mapNew = if (theMap.contains(newKey)) theMap + (newKey -> 'X')
        else theMap + (newKey -> '|')
        writeDown(mapNew, x, y - 1, nb - 1)
      }
    }

    val myhashMap : HashMap[String, Char] = HashMap()
    val result =  writeDown(myhashMap,5,5,10)
    val result2 = writeRight(result,2,2,10)
    val result3 = writeLeft(result2,8,4,10)
    val result4 = writeUp(result3, 3,1, 10)

    val mapWire1 = writeWire(wire1, myhashMap)
    val mapWire1And2 = writeWire(wire2, mapWire1)
    val mapWire1And2Without0 = mapWire1And2 - "0,0"
    val mapWire1And2Filter = mapWire1And2Without0.filter( v => v._2 == 'X')
    val mapWire1And2FilterDistanceMin = mapWire1And2Filter.keySet.toList.map(x => (x.split(",")))

    //mapWire1And2Filter.filter( v => v._1.split(",").toList.map(_.toInt)(1) > 0)
      //.minBy(_._1.split(",").toList.map(_.toInt).min)._1

    val mapWire1And2FilterDistance : HashMap[String, Int] = mapWire1And2Filter.map{ case (key, value) => (key, key.split(",").toList.map(_.toInt.abs).sum)}
    val mapWire1And2FilterDistanceMin2 = mapWire1And2Filter.map{ case (key, value) => (key, key.split(",").toList.map(_.toInt))}
    println(mapWire1And2FilterDistanceMin)

    mapWire1And2FilterDistance.foreach
    {
      case (key, value) => println (key + " -> " + value)
    }

    println(mapWire1And2FilterDistanceMin(0)(0).toInt)

    var min = 67890
    var autre = 234567

    for( i <- 0 to mapWire1And2FilterDistanceMin.size - 1  ) {
      if (min > mapWire1And2FilterDistanceMin(i)(1).toInt.abs){
        min = mapWire1And2FilterDistanceMin(i)(1).toInt.abs
        if (min == mapWire1And2FilterDistanceMin(i)(1).toInt.abs && autre > mapWire1And2FilterDistanceMin(i)(0).toInt.abs){
          autre = mapWire1And2FilterDistanceMin(i)(0).toInt
        }

      }
    }

    println(min + ", " + autre)
    //117

  }

}
