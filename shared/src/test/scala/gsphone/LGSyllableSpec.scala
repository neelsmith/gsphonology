package edu.holycross.shot.gsphone
import org.scalatest.FlatSpec
import edu.holycross.shot.greek._



class LGSyllableSpec extends FlatSpec {

  "Syllabification of a literary Greek string"  should "pass a single syllable unchagined" in {
    val sylls = LGSyllable.syllabify(Vector(LiteraryGreekString("w)=")))
    val syllsAscii = sylls.map(_.ascii)
    assert (syllsAscii == Vector("w)="))
  }

  it should "preserve leading and trailing content when recursively splitting on diaeresis" in {
    val strVector = "Τρῳαὶ ἐϋπλόκαμοι".split(" ").toVector
    val gsVector = strVector.map(LiteraryGreekString(_))
    val sylls = LGSyllable.splitOnDiaeresis(gsVector)
    val syllsUcode = sylls.map(_.ucode)
    assert(syllsUcode == Vector("Τρῳαὶ","ἐ","ϋ","πλόκαμοι"))
  }


  it should "preserve leading and trailing content when recursively splitting on mn" in {
    val strVector = "τῶν μιμνησκόμενος  ".split(" ").toVector
    val gsVector = strVector.map(LiteraryGreekString(_))
    val sylls = LGSyllable.splitOnMuNu(gsVector)
    val syllsUcode = sylls.map(_.ucode)
    assert(syllsUcode == Vector("τῶν","μι","μνησκόμενοσ"))
  }
  it should "preserve leading and trailing content when recursively splitting on liquid+consonant" in pending
  it should "preserve leading and trailing content when recursively splitting on diphthong+vowel" in pending
  it should "preserve leading and trailing content when recursively splitting on vowel+diphthong" in pending
  it should "preserve leading and trailing content when recursively splitting on short vowel+vowel" in pending
  it should "preserve leading and trailing content when recursively splitting on long vowel+vowel" in pending
  it should "preserve leading and trailing content when recursively splitting on upsilon+vowel" in pending
  it should "preserve leading and trailing content when recursively splitting on double consonant" in pending
  it should "preserve leading and trailing content when recursively splitting on consonant cluster" in pending
  it should "preserve leading and trailing content when recursively splitting on  vowel+=consonantvowel" in pending

/*
def testMap = [
"poios"  : "poi#os",
"o)i+w" : "o)#i+#w",
"pwu+" : "pw#u+",
"oi)w" : "oi)#w",
"limnh" : "li#mnh",
"a)nqos" : "a)n#qos",
"e)lpis" : "e)l#pis",
"e)rgma" : "e)r#gma",
"a)ei" : "a)#ei",
"dia" : "di#a",
"die" : "di#e",
"eu)+" : "e#u)+",
"r(ea" : "r(e#a",
"pragma" : "pra#gma",
"sui+" : "su#i+",
"tiw" :  "ti#w",
"r(a" : "r(a",
"oi(o" : "oi(#o",
"a)asamhn": "a)#a#sa#mhn",
"e)u+" : "e)#u+",
"ou(tos" : "ou(#tos",
"dw|h" : "dw|#h",
"eu+n" : "e#u+n",
"a)ll'": "a)ll'",
"a)mf'" : "a)mf'",
"e)aa|" : "e)#a#a|",
"h)u+s" : "h)#u+s",
"h)i+e" : "h)#i+#e",
"kien" : "ki#en",
"kion" : "ki#on",
"ui(ei+" : "ui(#e#i+",
"xiwn"  : "xi#wn",
"a)u+th" : "a)#u+#th",
"lu_e" : "lu_#e",
"a)nalu_w": "a)#na#lu_#w",
"is" : "is",
"ios" : "i#os",
"ni_ke": "ni_#ke",
"e)gegra^pto" : "e)#ge#gra^#pto"
]
*/
}
