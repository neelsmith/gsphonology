
package edu.holycross.shot.gsphone

import edu.holycross.shot.mid.validator._
import scala.scalajs.js.annotation._

/** A phonetic syllable token.*/
@JSExportTopLevel("TextSyllable") case object TextSyllable extends MidTokenCategory {
  def name = "syllable"
}
