package edu.holycross.shot.gsphone
import org.scalatest.FlatSpec
import edu.holycross.shot.greek._



class LiquidConsonantSpec extends FlatSpec {

  "Syllabification of a literary Greek string"  should  "preserve leading and trailing content when recursively splitting on liquid+consonant" in {
    val gsVector = Vector(LiteraryGreekString("σμινθεῦ"))
    val sylls = LGSyllable.splitOnLiqCons(gsVector)
    val syllsUcode = sylls.map(_.ucode)
    assert(syllsUcode == Vector("σμιν","θεῦ"))
  }

  it should "split correctly on liquid+consonant combination" in {
    val bad = Vector(LiteraryGreekString("ἄλγε"))
    val sylls = LGSyllable.splitOnLiqCons(bad)
    assert(sylls.size == 2)
  }


}
