package com.github.adventOfCode.Edition2019

import scala.io.Source

object Day12 {
  def main(args: Array[String]): Unit = {

    class Galaxy(val moon1: Moon, val moon2: Moon, val moon3: Moon, val moon4: Moon){

      def printGalaxy(): Unit ={
        moon1.printMoon
        moon2.printMoon
        moon3.printMoon
        moon4.printMoon
      }

      def equalsGalaxy(anotherGalaxy: Galaxy): Boolean ={
        if (moon1.equalsMoon(anotherGalaxy.moon1) && moon2.equalsMoon(anotherGalaxy.moon2) && moon3.equalsMoon(anotherGalaxy.moon3) && moon4.equalsMoon(anotherGalaxy.moon4)) true
        else false
      }

      def comparePosition(compare: Int, pos1: Int, pos2: Int, pos3: Int): Int ={
        val preResult1 = if (compare == pos1) 0 else if (compare > pos1) - 1 else 1
        val preResult2 = preResult1 + (if(compare == pos2) 0 else if (compare > pos2) -1 else 1)
        val preResult3 = preResult2 + (if(compare == pos3) 0 else if (compare > pos3) -1 else 1)
        preResult3
      }

      def gravityMoon(positionMoonCompare: Position, positionMoon1: Position, positionMoon2: Position, positionMoon3: Position, velocityMoon: Velocity): Velocity ={
        val resultGravityX = velocityMoon.vX + comparePosition(positionMoonCompare.pX, positionMoon1.pX, positionMoon2.pX, positionMoon3.pX)
        val resultGravityY = velocityMoon.vY + comparePosition(positionMoonCompare.pY, positionMoon1.pY, positionMoon2.pY, positionMoon3.pY)
        val resultGravityZ = velocityMoon.vZ + comparePosition(positionMoonCompare.pZ, positionMoon1.pZ, positionMoon2.pZ, positionMoon3.pZ)

        new Velocity(resultGravityX, resultGravityY, resultGravityZ)
      }

      def gravityGalaxy(): Galaxy ={
        val resultGravityMoon1 = gravityMoon(moon1.moonPosition, moon2.moonPosition, moon3.moonPosition, moon4.moonPosition, moon1.moonVelocity)
        val resultGravityMoon2 = gravityMoon(moon2.moonPosition, moon1.moonPosition, moon3.moonPosition, moon4.moonPosition, moon2.moonVelocity)
        val resultGravityMoon3 = gravityMoon(moon3.moonPosition, moon2.moonPosition, moon1.moonPosition, moon4.moonPosition, moon3.moonVelocity)
        val resultGravityMoon4 = gravityMoon(moon4.moonPosition, moon2.moonPosition, moon3.moonPosition, moon1.moonPosition, moon4.moonVelocity)

        new Galaxy(new Moon(moon1.moonPosition, resultGravityMoon1),
                   new Moon(moon2.moonPosition, resultGravityMoon2),
                   new Moon(moon3.moonPosition, resultGravityMoon3),
                   new Moon(moon4.moonPosition, resultGravityMoon4))
      }

      def velocityMoon(positionMoon: Position, velocityMoon: Velocity): Position ={
        val resultVelocityX = positionMoon.pX + velocityMoon.vX
        val resultVelocityY = positionMoon.pY + velocityMoon.vY
        val resultVelocityZ = positionMoon.pZ + velocityMoon.vZ

        new Position(resultVelocityX, resultVelocityY, resultVelocityZ)
      }

      def velocityGalaxy(): Galaxy = {
        val resultgravityGalaxy = gravityGalaxy()

        val resultVelocityMoon1 = velocityMoon(resultgravityGalaxy.moon1.moonPosition, resultgravityGalaxy.moon1.moonVelocity)
        val resultVelocityMoon2 = velocityMoon(resultgravityGalaxy.moon2.moonPosition, resultgravityGalaxy.moon2.moonVelocity)
        val resultVelocityMoon3 = velocityMoon(resultgravityGalaxy.moon3.moonPosition, resultgravityGalaxy.moon3.moonVelocity)
        val resultVelocityMoon4 = velocityMoon(resultgravityGalaxy.moon4.moonPosition, resultgravityGalaxy.moon4.moonVelocity)

        new Galaxy( new Moon(resultVelocityMoon1, resultgravityGalaxy.moon1.moonVelocity),
                    new Moon(resultVelocityMoon2, resultgravityGalaxy.moon2.moonVelocity),
                    new Moon(resultVelocityMoon3, resultgravityGalaxy.moon3.moonVelocity),
                    new Moon(resultVelocityMoon4, resultgravityGalaxy.moon4.moonVelocity))
      }

    }

    class Moon(val moonPosition: Position, val moonVelocity: Velocity) {

      def printMoon(): Unit = {
        println("pos=< x= " + moonPosition.pX + ", y= " + moonPosition.pY + ", z= " + moonPosition.pZ + " >, vel=< x= " + moonVelocity.vX + ", y= " + moonVelocity.vY + " ,z= " + moonVelocity.vZ + " >")
      }

      def equalsMoon(anotherMoon: Moon): Boolean ={
        if (moonPosition.equalsPosition(anotherMoon.moonPosition)) true
        else false
      }
    }

    class Position(val pX: Int, val pY: Int, val pZ: Int){

      def printPosition(): Unit={
        println("poc=< x= " + pX + ", y= " + pY + " ,z= " + pZ + " >")
      }

      def equalsPosition(anotherPosition: Position): Boolean ={
        if (pX == anotherPosition.pX && pY == anotherPosition.pY && pZ == anotherPosition.pZ) true
        else false
      }

    }

    class Velocity(val vX: Int, val vY: Int, val vZ: Int){

      def printVelocity(): Unit={
        println("vel=< x= " + vX + ", y= " + vY + " ,z= " + vZ + " >")
      }

    }

    def generatePositionMoon(lineMoon: String): Position ={
      val splitMoon = lineMoon.split(",").toList
      val xPositionMoon = splitMoon.head.drop(3).toInt
      val yPositionMoon = splitMoon(1).drop(3).toInt
      val zPositionMoon = splitMoon(2).drop(3).dropRight(1).toInt

      new Position(xPositionMoon, yPositionMoon, zPositionMoon)
    }

    def simulation1000Steps(actualGalaxy: Galaxy, step: Int = 1): Galaxy ={
      if (step == 1001) actualGalaxy
      else {
        println("step = " + step)
        val resultGalaxy = actualGalaxy.velocityGalaxy
        resultGalaxy.printGalaxy()
        println
        simulation1000Steps(resultGalaxy, step + 1)
      }
    }

    def calculEnergy(EnergyGalaxy: Galaxy): Int ={
      val potentialMoon1 = EnergyGalaxy.moon1.moonPosition.pX.abs + EnergyGalaxy.moon1.moonPosition.pY.abs + EnergyGalaxy.moon1.moonPosition.pZ.abs
      val kineticMoon1 = EnergyGalaxy.moon1.moonVelocity.vX.abs + EnergyGalaxy.moon1.moonVelocity.vY.abs + EnergyGalaxy.moon1.moonVelocity.vZ.abs
      val totalMoon1 = potentialMoon1 * kineticMoon1

      val potentialMoon2 = EnergyGalaxy.moon2.moonPosition.pX.abs + EnergyGalaxy.moon2.moonPosition.pY.abs + EnergyGalaxy.moon2.moonPosition.pZ.abs
      val kineticMoon2 = EnergyGalaxy.moon2.moonVelocity.vX.abs + EnergyGalaxy.moon2.moonVelocity.vY.abs + EnergyGalaxy.moon2.moonVelocity.vZ.abs
      val totalMoon2 = potentialMoon2 * kineticMoon2

      val potentialMoon3 = EnergyGalaxy.moon3.moonPosition.pX.abs + EnergyGalaxy.moon3.moonPosition.pY.abs + EnergyGalaxy.moon3.moonPosition.pZ.abs
      val kineticMoon3 = EnergyGalaxy.moon3.moonVelocity.vX.abs + EnergyGalaxy.moon3.moonVelocity.vY.abs + EnergyGalaxy.moon3.moonVelocity.vZ.abs
      val totalMoon3 = potentialMoon3 * kineticMoon3

      val potentialMoon4 = EnergyGalaxy.moon4.moonPosition.pX.abs + EnergyGalaxy.moon4.moonPosition.pY.abs + EnergyGalaxy.moon4.moonPosition.pZ.abs
      val kineticMoon4 = EnergyGalaxy.moon4.moonVelocity.vX.abs + EnergyGalaxy.moon4.moonVelocity.vY.abs + EnergyGalaxy.moon4.moonVelocity.vZ.abs
      val totalMoon4 = potentialMoon4 * kineticMoon4

      totalMoon1 + totalMoon2 + totalMoon3 + totalMoon4
    }


    val filename = "src/main/resources/Edition2019/Day12.txt"
    val lineList = Source.fromFile(filename).getLines.toList

    val positionLine1 = generatePositionMoon(lineList.head)
    val positionLine2 = generatePositionMoon(lineList(1))
    val positionLine3 = generatePositionMoon(lineList(2))
    val positionLine4 = generatePositionMoon(lineList(3))

    val theMoon1 = new Moon(positionLine1, new Velocity(0, 0, 0))
    val theMoon2 = new Moon(positionLine2, new Velocity(0, 0, 0))
    val theMoon3 = new Moon(positionLine3, new Velocity(0, 0, 0))
    val theMoon4 = new Moon(positionLine4, new Velocity(0, 0, 0))

    val theGalaxy = new Galaxy(theMoon1, theMoon2, theMoon3, theMoon4)

    /* val simulationGalaxy = simulation1000Steps(theGalaxy)
    val resultPart1 = calculEnergy(simulationGalaxy)
    println("Part 1, result = " + resultPart1) */

  }
}