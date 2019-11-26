package Edition2018

import scala.io.Source

object Day1 {

  def main(args: Array[String]): Unit = {
    var somme = 0;

    val filename = "src/main/resources/Edition2018/Day1.txt";

    for (ligne <- Source.fromFile(filename).getLines) {
      val ligne_int = ligne.toInt;
      somme = somme + ligne_int;
      println(ligne_int);
    }

    println("Le résultat est :");
    println(somme);
  }

}