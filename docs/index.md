---
title: Greek phonology library
layout: page
---



## Some preliminary notes

The core class is the `LGSyllable` (for "Literary Greek Syllable").  It understands the phonology of Greek written in the orthographic system of the `edu.holycross.shot.greek.LiteraryGreekString`.

The `LGSyllable`



## Short examples

Imports:

```scala
import edu.holycross.shot.gsphone._
import edu.holycross.shot.greek._
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
```


Syllabify a `CitableNode`:

```scala
val urn = CtsUrn("urn:cts:greekLit:tlg0012.tlg001.msA:1.1")
val node = CitableNode(urn, "Μῆνιν ἄειδε θεὰ Πηληϊάδεω Ἀχιλῆος")

val syllables = LGSyllable.tokenizeNode(node)
```


Note that the resulting syllables are *phonetic*, not *metrical*!

```scala
scala> println(syllables.map(_.string).mkString("\n"))
Μῆ
νιν
ἄ
ει
δε
θε
ὰ
Πη
λη
ϊ
ά
δε
ω
Ἀ
χι
λῆ
οσ
```
