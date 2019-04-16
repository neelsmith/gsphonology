package edu.holycross.shot.gsphone
import edu.holycross.shot.greek._
import scala.scalajs.js
import scala.scalajs.js.annotation._

import edu.holycross.shot.ohco2._
import edu.holycross.shot.cite._
import edu.holycross.shot.mid.validator._


@JSExportAll   case class LGSyllable (syllable: LiteraryGreekString) {

}


@JSExportAll object LGSyllable extends MidOrthography {


  // required by MidOrthography trait
  def orthography = "Tokenization of literary Greek orthography by syllable"

  // required by MidOrthography trait
  def tokenCategories = Vector(TextSyllable)

  // required by MidOrthography trait
  def validCP(cp: Int): Boolean = LiteraryGreekString.validAsciiCP(cp)

  // required by MidOrthography trait
  def tokenizeNode (n: CitableNode) : Vector[MidToken]  = {
    tokenizeNodeWithLGS(n, true)
  }



  /** Tokenize the text of a `CitableNode` into a Vector of `MidToken`s.
  *
  * @param n Node to tokenize.
  * @param ucodeString True if text strings of resulting `MidToken`s should
  * be in `LiteraryGreekString`'s Unicode representation;  false if ASCII.
  */
  def tokenizeNodeWithLGS(n: CitableNode, ucodeString: Boolean = true) : Vector[MidToken] = {
    val syllables =  syllabify(n.text)
    val urn = n.urn

    val pairs = for (unit <- syllables.zipWithIndex) yield {
      val newPassage = urn.passageComponent + "." + unit._2
      val newVersion = urn.addVersion(urn.versionOption.getOrElse("") + "_sylls")
      val newUrn = CtsUrn(newVersion.dropPassage.toString + newPassage)

      val trimmed = if (ucodeString) unit._1.ucode.trim else unit._1.ascii.trim

      MidToken(newUrn, trimmed, Some(TextSyllable))

    }
    pairs.toVector
  }


  /** Given a string in either the ASCII or Unicode representation
  * recognized by the `LiteraryGreekString` class, tokenize the string
  * into `LiteraryGreekString`s representing syllables.
  *
  * @param s String to syllabify.
  *

  def syllabify(s: String): Vector[LiteraryGreekString] = {
    syllabify(s, false)
  }
  */

  /** Given a string in either the ASCII or Unicode representation
  * recognized by the `LiteraryGreekString` class, tokenize the string
  * into `LiteraryGreekString`s representing syllables.
  *
  * @param s String to syllabify.
  * @param keepAccent True if accents should be retained in
  * strings of individual syllables.
  *
  */
  def syllabify(s: String, keepAccent:  Boolean = false): Vector[LiteraryGreekString] = {
    val vect = s.split(" ").filter(_.nonEmpty).toVector
    val gsVect = vect.map(LiteraryGreekString(_))
    syllabify(gsVect, keepAccent)
  }

/*
  def syllabify(v : Vector[LiteraryGreekString]) :  Vector[LiteraryGreekString] = {
    syllabify(v, false)

  }*/

  /** Given a Vector of  `LiteraryGreekString`s, tokenize each item into
  * into `LiteraryGreekString`s representing syllables.
  * The algorithm directly mimics the description of syllabic division
  * in Smyth's *Greek Grammar*.
  *
  * @param v Vector of `LiteraryGreekString`s to syllabify.
  *
  */
  def syllabify(v : Vector[LiteraryGreekString], keepAccent: Boolean): Vector[LiteraryGreekString] = {
    val strVector = keepAccent match {
      case true => v
      case false => v.map(_.stripBreathingAccent)
    }
    val dia = splitOnDiaeresis(strVector)
    val mn = splitOnMuNu(dia)
    val lc = splitOnLiqCons(mn)
    val dv = splitOnDiphthVowel(lc)
    val vd = splitOnVowelDiphth(dv)
    val shrtVwl = splitOnShortVowelVowel(vd)
    val lngVwl = splitOnLongVowelVowel(shrtVwl)
    val uVwl = splitOnUpsilonVowel(lngVwl)

    val dblCons = splitOnDoubleCons(uVwl)
    // Make consonant clusters smarter?
    val conss = splitOnConsCluster(dblCons)
    splitOnVCV(conss)
  }



  def splitOnDiaeresis(v : Vector[LiteraryGreekString]): Vector[LiteraryGreekString] = {
    val diaPattern = "(.*[aeiouhw][\\)\\(]?)([iu][\\)\\(]?\\+)(.*)".r
    v.flatMap (gs => {
      gs.ascii match {
        case diaPattern(lead, vwl, trail) => {
          val matchingContent = Vector(lead,vwl,trail).filter(_.nonEmpty)
          val gsVector = matchingContent.map(LiteraryGreekString(_))
          splitOnDiaeresis(gsVector)
        }
        case _ => Vector(gs)
      }
    })
  }



  def splitOnMuNu(v : Vector[LiteraryGreekString]): Vector[LiteraryGreekString] = {
    val mnPattern = "(.*[aeiouhw\\|\\+])mn(.*)".r
    v.flatMap { gs => {
      gs.ascii match {
        case mnPattern(leadCluster,trail) => {
          val matchingContent = Vector(leadCluster,"mn" + trail).filter(_.nonEmpty)
          val gsVector = matchingContent.map(LiteraryGreekString(_))
          splitOnMuNu(gsVector)
        }

        case _ => Vector(gs)
        }
      }
    }
  }


  /** Split on combination of liquid+consonant into a Vector of strings.
  *
  * @param v Vector of strings to split further.
  */
  def splitOnLiqCons(v : Vector[LiteraryGreekString]): Vector[LiteraryGreekString] = {
    //val liqConsPattern = "(.+[\\)\\(aeiouhw\\|\\+])([lmnr])([bgdzqkcprstfxy]+.*)".r
    val liqConsPattern = "(.+)([lmnr])([bgdzqkcprstfxy]+.*)".r
    v.flatMap { gs => {
      //println("SPLIT STRING: " + gs.ascii)
      gs.ascii match {
        case liqConsPattern(leadCluster,liquid,trail) => {
          val matchingContent = Vector(leadCluster + liquid, trail).filter(_.nonEmpty)
          val gsVector = matchingContent.map(LiteraryGreekString(_))
          splitOnLiqCons(gsVector)
        }
        case _ => Vector(gs)
        }
      }
    }
  }

  def splitOnDiphthVowel(v : Vector[LiteraryGreekString]): Vector[LiteraryGreekString] = {
    val diphthVowelPattern = "(.*)(ai[\\)\\(=\\/]*|oi[\\)\\(=\\/]*|ei[\\)\\(=\\/]*|au[\\)\\(=\\/]*|eu[\\)\\(=\\/]*|ou[\\)\\(=\\/]*|hu[\\)\\(=\\/]*|wu[\\)\\(=\\/]*|ui[\\)\\(=\\/]*)([aeiouhw].*)".r
    v.flatMap (gs => {
      gs.ascii match {
        case diphthVowelPattern(lead, vwl, trail) => {
          val matchingContent = Vector(lead + vwl,trail).filter(_.nonEmpty)
          val gsVector = matchingContent.map(LiteraryGreekString(_))
          splitOnDiphthVowel(gsVector)
        }
        case _ => Vector(gs)
      }
    })
  }



  // short break out long vs short to insist on proper
  // accet ...
  def splitOnVowelDiphth(v : Vector[LiteraryGreekString]): Vector[LiteraryGreekString] = {
    val vowelDiphPattern = "(.*[aeiouhw][\\(\\)/=]*)(ai|oi|ei|au|eu|ou|hu|wu|ui)(.*)".r

    v.flatMap (gs => {
      gs.ascii match {
          case vowelDiphPattern(lead, diph, trail) => {
            if (trail.isEmpty) {
              val matchVector = Vector(lead,diph).map(LiteraryGreekString(_))
              splitOnVowelDiphth(matchVector)
            } else {
              val matchVector = Vector(lead,diph + trail).map(LiteraryGreekString(_))
              splitOnVowelDiphth(matchVector)
            }
        }
        case _ => Vector(gs)
      }
    })
  }

  def splitOnShortVowelVowel(v : Vector[LiteraryGreekString]): Vector[LiteraryGreekString] = {
    val shortPlusVowelPattern = "(.*[aeio][\\)\\(\\+/]*)([aehow].*)".r
    v.flatMap (gs => {
      gs.ascii match {
          case shortPlusVowelPattern(lead, trail) => {
            splitOnShortVowelVowel(
              Vector(LiteraryGreekString(lead), LiteraryGreekString(trail)))
          }
          case _ => Vector(gs)
        }
      })
  }
  def splitOnLongVowelVowel(v : Vector[LiteraryGreekString]): Vector[LiteraryGreekString] = {
    // ([hw][\)\(\|]?)([aehiow])
    val longPlusVowelPattern = "(.*[hw][\\)\\(\\|\\+=/]*)([aehow].*)".r
    v.flatMap (gs => {
      gs.ascii match {
        case longPlusVowelPattern(lead, trail) => {
          splitOnLongVowelVowel(
            Vector(LiteraryGreekString(lead), LiteraryGreekString(trail)))
        }
        case _ => Vector(gs)
        }
      })
  }
  def splitOnUpsilonVowel(v : Vector[LiteraryGreekString]): Vector[LiteraryGreekString] = {
    val upsVwlpattern = "(.*u[\\)\\(=/])([aehouw].*)".r
    v.flatMap (gs => {
      gs.ascii match {
        case upsVwlpattern(lead, trail) => {
          splitOnUpsilonVowel(
            Vector(LiteraryGreekString(lead), LiteraryGreekString(trail)))
        }
        case _ => Vector(gs)
        }
      })
  }
  def splitOnDoubleCons(v : Vector[LiteraryGreekString]): Vector[LiteraryGreekString] = {
    val doubleConsPattern = "(.*)(b{2}|g{2}|d{2}|z{2}|q{2}|k{2}|l{2}|m{2}|n{2}|p{2}|r{2}|s{2}|t{2}|f{2}|x{2})(.*)".r
    v.flatMap (gs => {
      gs.ascii match {
        case doubleConsPattern(lead, dubble, trail) => {
          splitOnDoubleCons(
            Vector(LiteraryGreekString(lead + dubble(0)),

             LiteraryGreekString(dubble(1) + trail)))
        }
        case _ => Vector(gs)
        }
      })
  }
  def splitOnConsCluster(v : Vector[LiteraryGreekString]): Vector[LiteraryGreekString] = {
    val consClustPattern = "(.*[\\)\\(aeiouhw\\|\\+])([bgdzqkpcstfxy][mnbgdzqklcprstfxy]+.*)".r
    v.flatMap (gs => {
      gs.ascii match {
        case consClustPattern(lead,  trail) => {
          splitOnConsCluster(
            Vector(LiteraryGreekString(lead),
             LiteraryGreekString(trail)))
        }
        case _ => Vector(gs)
        }
      })

  }
  def splitOnVCV(v : Vector[LiteraryGreekString]): Vector[LiteraryGreekString] = {
    val vcvPattern = "(.*[\\)\\(aeiouhw\\|\\+][\\)\\(/=]*)([bgdzqklmncprstfxy][aeiouhw].*)".r

    v.flatMap (gs => {
      gs.ascii match {
        case vcvPattern(lead,  trail) => {
          splitOnVCV(
            Vector(LiteraryGreekString(lead),
             LiteraryGreekString(trail)))
        }
        case _ => Vector(gs)
        }
      })
  }


}
