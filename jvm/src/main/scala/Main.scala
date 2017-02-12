package edu.holycross.shot.gsphone
import edu.holycross.shot.greek._

object Main {
  def main(args: Array[String]): Unit = {

    val iliad1_1 = "Μῆνιν ἄειδε θεὰ Πηληϊάδεω Ἀχιλῆος"
    println("\nIliad 1.1:")
    println(iliad1_1)

    val iliad1_1_vector = iliad1_1.split(" ").toVector.map(LiteraryGreekString(_))

    println("\nSyllabified:")
    val sylls = LGSyllable.syllabify(iliad1_1_vector)
    println(sylls.map(_.ucode).mkString("-"))
    println("\nRise and fall in pitch by syllable:")
    println(sylls.map(_.accent.getOrElse("-")).mkString + "\n")

  }
}
