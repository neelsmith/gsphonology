package edu.holycross.shot.gsphone
import edu.holycross.shot.greek._

object LGSyllable {

  def splitOnDiaeresis(v : Vector[LiteraryGreekString]): Vector[GreekString] = {
    val diaPattern = "([aeiouhw][\\)\\(]?)([iu][\\)\\(]?\\+)([aeiouhw]?)".r
    v.flatMap (gs => {
      val diaPattern(vwl1, vwl2, trail) = gs.ascii
      if (trail.nonEmpty) {
        Vector(LiteraryGreekString(vwl1), LiteraryGreekString(vwl2), LiteraryGreekString(trail))
      } else {
        Vector(LiteraryGreekString(vwl1), LiteraryGreekString(vwl2))
      }
    })
  }

  def syllabify(v : Vector[LiteraryGreekString]): Vector[GreekString] = {
    val dia = splitOnDiaeresis (v)
    dia
  }
}
