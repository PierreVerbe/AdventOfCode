package com.github.adventOfCode.Edition2019

import scala.collection.immutable.HashMap
import scala.io.Source

object Day6 {
  def main(args: Array[String]): Unit = {

    def countOrbits(searchOrbit: String , mapOrbits: Map[String, String], nbOrbits: Int): Int = {
      if (! mapOrbits.contains(searchOrbit)) nbOrbits
      else {
        val directOrbit = mapOrbits(searchOrbit)
        countOrbits(directOrbit, mapOrbits.-(searchOrbit), nbOrbits+1)
      }
    }

    def totalOrbits(mapActualOrbits: Map[String, String], mapTotalOrbits: Map[String, String], totalOrbitsNb: Int): Int = {
      if (mapActualOrbits.isEmpty) totalOrbitsNb
      else {
        val directOrbit = mapActualOrbits.head._1
        val countOrbitsState = countOrbits(directOrbit, mapTotalOrbits, 0)
        val calculTotal = countOrbitsState + totalOrbitsNb
        totalOrbits(mapActualOrbits.tail, mapTotalOrbits, calculTotal)
      }
    }

    def containsSanta(searchOrbit: String , mapOrbits: Map[String, String]): Boolean = {
      val searchOrbitResult = mapOrbits.getOrElse(searchOrbit, "null")
      println(searchOrbitResult)
      if (searchOrbit == "SAN") true
      else if (searchOrbit == "null") false
      else {
        containsSanta(searchOrbitResult.toString, mapOrbits.-(searchOrbit))
      }
    }

    /*def test(mapOrbits: Map[String, String]): Unit ={
      if(mapOrbit.filter(_._2.equals("B")).size == 1 )

    }*/

    val filename = "src/main/resources/Edition2019/Day6_2.txt";
    val lineList = Source.fromFile(filename).getLines.toList
    val mapOrbit : Map[String, String]= lineList.map(x => x.split("\\)").toList.tail.head ->  x.split("\\)").toList.head).toMap
    println(mapOrbit)
    println(totalOrbits(mapOrbit, mapOrbit,0))
    println(containsSanta("L", mapOrbit))

    println(mapOrbit.filter(_._2.equals("B")))
    println(mapOrbit.filter(_._2.equals("B")).size)


  }
}