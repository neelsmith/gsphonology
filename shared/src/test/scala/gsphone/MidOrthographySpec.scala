package edu.holycross.shot.gsphone
import org.scalatest.FlatSpec
import edu.holycross.shot.greek._
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._

class MidOrthographySpec extends FlatSpec {

  "The LGSyllable object" should "tokenize a string into syllables" in {
    val wrath = "μῆνιν"
    val urn = CtsUrn("urn:cts:greekLit:tlg0012.tlg001:1.1")
    val cn = CitableNode(urn, wrath)
    val sylls = LGSyllable.tokenizeNode(cn)
    val expectedSize = 2
    assert (sylls.size == expectedSize)
    val expectedText2 = "νιν"
    assert(sylls(1).string == expectedText2)
  }
}
