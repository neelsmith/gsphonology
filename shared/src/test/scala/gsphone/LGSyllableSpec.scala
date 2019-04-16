package edu.holycross.shot.gsphone
import org.scalatest.FlatSpec
import edu.holycross.shot.greek._



class LGSyllableSpec extends FlatSpec {

  "Syllabification of a literary Greek string"  should "pass a single syllable unchanged when accept is kept" in {
    val sylls = LGSyllable.syllabify(Vector(LiteraryGreekString("w)=")), true)
    val syllsAscii = sylls.map(_.ascii)
    assert (syllsAscii == Vector("w)="))
  }

  it should "produce the same result without accents and breathings when that is requested" in {
    val sylls = LGSyllable.syllabify(Vector(LiteraryGreekString("w)=")), false)
    val syllsAscii = sylls.map(_.ascii)
    assert (syllsAscii == Vector("w"))
  }




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
