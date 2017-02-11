package edu.holycross.shot.gsphone
import edu.holycross.shot.greek._

object LGSyllable {

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


  def splitOnLiqCons(v : Vector[LiteraryGreekString]): Vector[LiteraryGreekString] = {
    val liqConsPattern = "(.+[\\)\\(aeiouhw\\|\\+])([lmnr])([bgdzqkcprstfxy]+.*)".r

    v.flatMap { gs => {
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

  def syllabify(v : Vector[LiteraryGreekString]): Vector[LiteraryGreekString] = {
    val dia = splitOnDiaeresis (v)
    val mn = splitOnMuNu(dia)
    mn

/*

diphthong_vowel
vowel_diphthong
shortv_vowel
longv_vowel
upsilon_vowel
double_consonant
consonant_cluster
vowel_consonantvowel

    // dipthong-vowel splits
    syllabic = syllabic.replaceAll(diphthong_vowel) { fullMatch, dipth, vow ->
      dipth + "#" + vow
    }
    // vowel-dipthong splits
    syllabic = syllabic.replaceAll(vowel_diphthong) {fullMatch, vow, dipth ->
      vow + "#" + dipth
    }
    // split short vowel followed by non-diphthong
    syllabic = syllabic.replaceAll(shortv_vowel) {fullMatch, v1, v2 ->
      v1 + "#" + v2
    }
    // apply twice because regex matches overlap if there are 3 successive vowels.
    syllabic = syllabic.replaceAll(shortv_vowel) {fullMatch, v1, v2 ->
      v1 + "#" + v2
    }

    // split long vowel by nature followed by non-diphthong
    syllabic = syllabic.replaceAll(longv_vowel) {fullMatch, v1, v2 ->
    v1 + "#" + v2
    }
    // split long vowel marked with macron followed by non-diphthong
    syllabic = syllabic.replaceAll(vowelwmacron_vowel) {fullMatch, v1, v2 ->
    v1 + "#" + v2
    }
    // split upsilon followed by non-diphthong
    syllabic = syllabic.replaceAll(upsilon_vowel) {fullMatch, u, v ->
      u + "#" + v
    }
    // double consonants split
    syllabic = syllabic.replaceAll(double_consonant) { fullMatch, doubled, trail ->
      doubled[0] + "#" + doubled[1] + trail
    }

    // Otherwise, consonant clusters start a syllable.
    // Must apply  pattern twice, because regex
    // matches can overlap.
    syllabic = syllabic.replaceAll(consonant_cluster) { fullMatch, v, cons, trail ->
      v + "#" + cons + trail
    }
    syllabic = syllabic.replaceAll(consonant_cluster) { fullMatch, v, cons, trail ->
      v + "#" + cons + trail
    }

    // Must apply vowel-consonant-vowel pattern twice, because regex
    // matches can overlap.
    syllabic = syllabic.replaceAll(vowel_consonantvowel) { fullMatch, v, cv ->
      v + "#" + cv
    }
    syllabic = syllabic.replaceAll(vowel_consonantvowel) { fullMatch, v, cv ->
      v + "#" + cv
    }
    */
  }
}
