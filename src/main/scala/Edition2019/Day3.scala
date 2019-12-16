package Edition2019

import scala.io.Source

object Day3 {

  def main(args: Array[String]): Unit = {

    def closestIntersection(firstLine: String, secondLine: String): Int = {
      var firstPos: Vector[(Int,Int)] = Vector()
      var secondPos: Vector[(Int,Int)] = Vector()
      var currentPos = (0,0)

      for (instruction <- firstLine.split(',')) {
        instruction.head match {
          case 'R' => for(i <- 1 to instruction.tail.toInt) firstPos = firstPos :+ (currentPos._1 + i, currentPos._2)
          case 'L' => for(i <- 1 to instruction.tail.toInt) firstPos = firstPos :+ (currentPos._1 - i, currentPos._2)
          case 'U' => for(i <- 1 to instruction.tail.toInt) firstPos = firstPos :+ (currentPos._1, currentPos._2 + i)
          case 'D' => for(i <- 1 to instruction.tail.toInt) firstPos = firstPos :+ (currentPos._1, currentPos._2 - i)
          case _ => println("error")
        }
        currentPos = firstPos.last
      }

      currentPos = (0,0)
      for (instruction <- secondLine.split(',')) {
        instruction.head match {
          case 'R' => for(i <- 1 to instruction.tail.toInt) secondPos = secondPos :+ (currentPos._1 + i, currentPos._2)
          case 'L' => for(i <- 1 to instruction.tail.toInt) secondPos = secondPos :+ (currentPos._1 - i, currentPos._2)
          case 'U' => for(i <- 1 to instruction.tail.toInt) secondPos = secondPos :+ (currentPos._1, currentPos._2 + i)
          case 'D' => for(i <- 1 to instruction.tail.toInt) secondPos = secondPos :+ (currentPos._1 , currentPos._2- i)
          case _ => println("error")
        }
        currentPos = secondPos.last
      }

      val intersectionsWires = firstPos.intersect(secondPos).sortBy(x => math.abs(x._1) + math.abs(x._2))
      println("X = " + intersectionsWires(0)._1 + ", Y = " + intersectionsWires(0)._2)
      math.abs(intersectionsWires(0)._1) + math.abs(intersectionsWires(0)._2)
    }

    def fewestSteps(firstLine: String, secondLine: String): Int = {
      var firstPos: Vector[(Int,Int)] = Vector.empty
      var firstStep: scala.collection.mutable.Map[(Int,Int), Int] = scala.collection.mutable.Map.empty
      var secondPos: Vector[(Int,Int)] = Vector.empty
      var secondStep: scala.collection.mutable.Map[(Int,Int), Int] = scala.collection.mutable.Map.empty
      var currentPos = (0,0)
      var steps = 0

      for (instruction <- firstLine.split(',')) {
        instruction.head match {
          case 'R' => for(i <- 1 to instruction.tail.toInt) {
            steps += 1
            firstPos = firstPos :+ (currentPos._1 + i, currentPos._2)
            if (!firstStep.contains(firstPos.last)) firstStep(firstPos.last) = steps
          }
          case 'L' => for(i <- 1 to instruction.tail.toInt) {
            steps += 1
            firstPos = firstPos :+ (currentPos._1 - i, currentPos._2)
            if (!firstStep.contains(firstPos.last)) firstStep(firstPos.last) = steps
          }
          case 'U' => for(i <- 1 to instruction.tail.toInt) {
            steps += 1
            firstPos = firstPos :+ (currentPos._1, currentPos._2 + i)
            if (!firstStep.contains(firstPos.last)) firstStep(firstPos.last) = steps
          }
          case 'D' => for(i <- 1 to instruction.tail.toInt) {
            steps += 1
            firstPos = firstPos :+ (currentPos._1, currentPos._2 - i)
            if (!firstStep.contains(firstPos.last)) firstStep(firstPos.last) = steps
          }
          case _ => println("error")
        }
        currentPos = firstPos.last
      }
      currentPos = (0,0)
      steps = 0

      for (instruction <- secondLine.split(',')) {
        instruction.head match {
          case 'R' => for(i <- 1 to instruction.tail.toInt) {
            steps += 1
            secondPos = secondPos :+ (currentPos._1 + i, currentPos._2)
            if (!secondStep.contains(secondPos.last)) secondStep(secondPos.last) = steps
          }
          case 'L' => for(i <- 1 to instruction.tail.toInt) {
            steps += 1
            secondPos = secondPos :+ (currentPos._1 - i, currentPos._2)
            if (!secondStep.contains(secondPos.last)) secondStep(secondPos.last) = steps
          }
          case 'U' => for(i <- 1 to instruction.tail.toInt) {
            steps += 1
            secondPos = secondPos :+ (currentPos._1, currentPos._2 + i)
            if (!secondStep.contains(secondPos.last)) secondStep(secondPos.last) = steps
          }
          case 'D' => for(i <- 1 to instruction.tail.toInt) {
            steps += 1
            secondPos = secondPos :+ (currentPos._1 , currentPos._2- i)
            if (!secondStep.contains(secondPos.last)) secondStep(secondPos.last) = steps
          }
          case _ => println("error")
        }
        currentPos = secondPos.last
      }
      val intersectionsWires = firstPos.intersect(secondPos).map(x => firstStep(x) + secondStep(x)).sorted
      intersectionsWires(0)
    }

    val filename = "src/main/resources/Edition2019/Day3.txt";
    val lineList = Source.fromFile(filename).getLines.toList

    println("Part 1, result = " + closestIntersection(lineList(0), lineList(1)))
    println("Part 2, result = " + fewestSteps(lineList(0), lineList(1)))
  }
}