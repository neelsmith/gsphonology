import edu.holycross.shot.greek._
import edu.holycross.shot.gsphone._

/*
val lns = """Μῆνιν ἄειδε θεὰ Πηληϊάδεω Ἀχιλῆος
οὐλομένην· ἡ μυρί᾽ Ἀχαιοῖς ἄλγε᾽ ἔθηκεν·
πολλὰς δ᾽ ἰφθίμους ψυχὰς Ἄϊδι προΐαψεν
ἡρώων· αὐτοὺς δὲ ἑλώρια τεῦχε κύνεσσιν
οἰωνοῖσί τε πᾶσι· Διὸς δ᾽ ἐτελείετο βουλή·
ἐξ οὗ δὴ τὰ πρῶτα διαστήτην ἐρίσαντε
Ἀτρείδης τε ἄναξ ἀνδρῶν καὶ δῖος Ἀχιλλεύς""".split("\n")
*/


def fullGS(gs: LiteraryGreekString): String = {
  gs.ucode + "=" + gs.ascii
}
def demo {
// Split a String into a Vector of Greek Stings:
  println("Iliad 1.1 as a String:")
  val iliad1_1 = "Μῆνιν ἄειδε θεὰ Πηληϊάδεω Ἀχιλῆος"
  println("\t" + iliad1_1)
  println("Split on white space and make Greek String objects:")
  val iliad1_1_vector = iliad1_1.split(" ").toVector.map(LiteraryGreekString(_))
  println("\t" + iliad1_1_vector.map(fullGS(_)).mkString(" "))
  println("Syllabify:")
  val sylls = LGSyllable.syllabify(iliad1_1_vector)
  println("\t" + sylls.map(_.ucode).mkString("-"))
  println("Rise and fall in pitch by syllable:")
  println("\t" + sylls.map(_.accent.getOrElse("-")).mkString)

}





val iliad1_1 = LiteraryGreekString("Μῆνιν ἄειδε θεὰ Πηληϊάδεω Ἀχιλῆος")
