package edu.holycross.shot.gsphone
import edu.holycross.shot.greek._
import org.scalatest._

class ExportTest extends FlatSpec {

  "The gsphone library"  should "expose methods for syllabification" in {
    val gsVector = Vector(LiteraryGreekString("a)ntio/wsan"))
    val sylls = LGSyllable.splitOnShortVowelVowel(gsVector)
    val syllsUcode = sylls.map(_.ucode)
    assert(syllsUcode == Vector("ἀντι","ό","ωσαν"))
  }

}
