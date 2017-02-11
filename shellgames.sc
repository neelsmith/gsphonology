import edu.holycross.shot.greek._
import edu.holycross.shot.gsphone._


val lns = """Μῆνιν ἄειδε θεὰ Πηληϊάδεω Ἀχιλῆος
οὐλομένην· ἡ μυρί᾽ Ἀχαιοῖς ἄλγε᾽ ἔθηκεν·
πολλὰς δ᾽ ἰφθίμους ψυχὰς Ἄϊδι προΐαψεν
ἡρώων· αὐτοὺς δὲ ἑλώρια τεῦχε κύνεσσιν
οἰωνοῖσί τε πᾶσι· Διὸς δ᾽ ἐτελείετο βουλή·
ἐξ οὗ δὴ τὰ πρῶτα διαστήτην ἐρίσαντε
Ἀτρείδης τε ἄναξ ἀνδρῶν καὶ δῖος Ἀχιλλεύς""".split("\n")


val line1sylls = LGSyllable.syllabify(lns(0).split(" ").toVector.map(LiteraryGreekString(_)))
