package edu.holycross.shot.gsphone
import edu.holycross.shot.greek._

object LGSyllable {

  def splitOnDiaeresis(v : Vector[LiteraryGreekString]): Vector[LiteraryGreekString] = {
    val diaPattern = "([aeiouhw][\\)\\(]?)([iu][\\)\\(]?\\+)([aeiouhw]?)".r
    v.flatMap (gs => {
      gs.ascii match {
        case diaPattern(vwl1, vwl2, trail) =>
          if (trail.nonEmpty) {
            Vector(LiteraryGreekString(vwl1), LiteraryGreekString(vwl2), LiteraryGreekString(trail))
          } else {
            Vector(LiteraryGreekString(vwl1), LiteraryGreekString(vwl2))
          }
        case _ => Vector(gs)
      }
    })
  }

  def splitOnMuNu(v : Vector[LiteraryGreekString]): Vector[LiteraryGreekString] = {
    val mnPattern = "([\\)\\(aeiouhw\\|\\+])mn".r
    v.flatMap { gs => {
      gs.ascii match {
        case mnPattern(vwlCluster) =>
          Vector(LiteraryGreekString(vwlCluster), LiteraryGreekString("mn"))
        case _ => Vector(gs)
        }
      }
    }
  }
//~//
  def syllabify(v : Vector[LiteraryGreekString]): Vector[LiteraryGreekString] = {
    val dia = splitOnDiaeresis (v)
    val mn = splitOnMuNu(dia)
    mn
  }
}
