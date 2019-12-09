package Edition2019

import scala.io.Source

object Day8 {
  def main(args: Array[String]): Unit = {

    val filename = "src/main/resources/Edition2019/Day8.txt";
    val lineList = Source.fromFile(filename).getLines.toList
  }

}
