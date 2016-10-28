----------
title: Algorithmic Compositional Techniques Developed for "Pleonid"
author: Stuart Popejoy
date: 2015-05-15
----------

Pleonid is an algorithmic composition for quintet premiered in May
2012.  It was generated, arranged and scored by software. This paper
describes the methods used to produce the composition, including a
novel re-working of the "Tone Clock" by Peter Schat, as well as the
use of braids as both a motif- and counterpoint-generating
techique.

Source code, graphics and figures
---

While the performed version of Pleonid was written in Java, I have since
migrated the code to Haskell, "bugs and all" to be able to produce
the exact same composition (given the same parameters). Thus, source
code examples are in Haskell. Music figures were generated in Haskell
and exported via MusicXML to Sibelius for output. Braid graphics are
generated using the Haskell Diagrams library.


Gamut
-----

When working with pitch degrees, it can be productive to use other values than
the octave's 12-note period. In my work, I use the term "gamut" to describe other
periods over which I might repeat intervallic formations, or "transpose" parts for
orchestrating different registers. In Pleonid, a gamut value of 10 is used for generation
and orchestration; again this parameter can be changed to produce a different character.



Generative Techniques for Pitch Sequences
=========================================

"Pleonid" employs a minimum of stochastic techniques,
preferring a generative/elaborative approach. The first phase of
generation involves manipulating a single scale to produce a series of
pitch sequences which form the core material of the piece. A single melodic sequence provides the seed.

![Seed melody of *Pleonid*.](figures/pleonid/01-seed__small.png)

This is an example of how Pleonid is really a *parameterization* of a
software program. The source melody is an input, which if changed produces
entirely new material.

The pitch-class set of the source melody is determined, `[0,2,3,4,5,7]`
(Forte no. [6-8](https://en.wikipedia.org/wiki/List_of_pitch_class_sets)).
This is used as a concrete scale, as opposed to a basis
for set operations.

![Pitch class set and main scale.](figures/pleonid/01a-seedScale__small.png)


Finding all N-ads of the source scale
-------------------------------------

The first elaboration finds every chord or "sub-scale" that can be
built from this scale/sequence. From the source scale's interval vector,
`[2,1,1,1,2]`, every possible ordered sum is computed: every chord
or "N-ad" (dyad, triad etc) that the scale permits.

The function `genNAds` achieves this with concatenation of two
recursive list comprehensions.^[The original Java code is considerably longer and less elegant. For
this formulation, credit is due to @Cale on the #haskell
IRC channel for producing this brilliant formation in response to my
desperate plea for help.]

> genNAds :: Num a => [a] -> [[a]]
> genNAds []     = [[]]
> genNAds (x:xs) = [x:ps | ps <- genNAds xs] ++
>                      [(x+p):ps | p:ps <- genNAds xs]

To illustrate, let's take the sequence `C D E F`, whose intervals are `[2,2,1]`.
The resulting tuples are `[[2,2,1],[2,3],[4,1],[5]]`.

![Generating all "N-ads" from a sequence.](figures/pleonid/02-genNAds__small.png)

Performing this operation on the Pleonid interval vector `[2,1,1,1,2]`
produces 16 tuples. The final dyad is discarded, as it is simply the scale
boundary, although the first tuple is simply the full scale; in retrospect, dropping
the last dyad seems arbitrary.

![Pleonid scale tuples.](figures/pleonid/02a-genNAdsPleonid.png)



Steering Chords: a modified Tone Clock
====

The next procedure uses this body of chords to produce a series of
pitch sequences.  To do so, I modified techniques invented by the
Dutch composer Peter Schat, which he dubbed the "Tone Clock."  ^[Schat, Peter
(1993). Also see <http://en.wikipedia.org/wiki/Tone_Clock> and
<http://www.peterschat.nl/clockwise.html>. Thanks to Jochem van Dijk
for hipping me to it.]

In the Tone Clock, triads are grouped with their inversion as a class.
Thus the major triad, represented by the intervals `[4,3]`, is classified
with its inverse, the minor triad `[3,4]`, and so on. The 12-tone scale has
12 of these triad classes, which form the "hours" of Schat's Clock.

Schat then provides a method for finding every configuration by which this triad class can saturate
the 12-tone space, called "steerings". In the case of the major/minor triad, there
are exactly two configurations: major/minor/major/minor at `0,2,6,8`,
and minor/minor/major/major at `0,2,4,6`.

![Steerings of the major/minor triad class "hour" in the Tone Clock.](figures/pleonid/03-ixSteerings.png)

Each triad class or "hour" has a fixed number of
configuration "steerings", with 33 total configurations over all classes.
Each configuration can be seen as producing a tetrad
of the bottom-note placements. ^[For Schat these tetrads can be related
intervallically to other triad hours in the clock, producing a relationship between hours.]

Steerings in Pleonid
--------------------

In Pleonid, I expanded the concept of steering to apply to any "N-ad" instead of just triads.
However, once we move to more notes than 3, the notion of "inversion" of intervals to form
classes must become *rotation*: like the major triad class `([3,4],[4,3])` can be seen as
the 2 possible rotations of the interval pair, a tetrad class would require 3 rotations. For example,
the tetrad `[1,3,2]` is classified with `[3,2,1]` and `[2,1,3]`.

![Rotations of the tetrad [1,3,2].](figures/pleonid/03a-tetradRotation__small.png)

I also chose to use Pleonid's "gamut" of 10 instead of the octave 12
to constrain the space into which the steerings "fit" the chords. This however
means that the full saturation of Schat's triads in the 12-tone space is not
always possible, as the chord size might not evenly divide into the gamut.
So, this constraint is removed: the search is for the *maximal* saturation
of the gamut, instead of the *total* saturation of Schat's clock.

Even without this constraint, some chords still cannot "steer", meaning there
is no combination with itself or a rotation that will fit into the gamut.

Steering algorithm
------------------

The search for all valid configurations of a tuple class in a gamut
begins with the observation that every N-ad is steered by another
"M-ad" where `M` is N divided by the gamut. Thus a 5-tuple is steered
by some dyad (2-tuple) in a 10-gamut.

Thus the search space is every possible "steering" M-ad in the gamut,
configured with every possible rotation of the N-ad placed in each "slot"
in the M-ad. Collisions are invalidated, and duplicates removed, producing
a final result of valid configurations.

For example, finding all steerings of the 5-tuple `[2,1,2,2]` in a 10-gamut
means searching every possible dyad (90 total) with every possible
rotation (4 total) resulting in 360 candidates. Some optimizations are possible,
for instance dyads having intervals larger than 1/2 the gamut size are
"un-steerable". For this 5-tuple, just 2 steerings are found.

![Steering [2,1,2,2] in a 10-gamut.](figures/pleonid/05-pleoSteering3.png)

The last tuple of the second steering is noteworthy, as the last note
is "gamut-wrapped". The rotation of the interval is `[2,2,1,2]` but since
the last note goes above the gamut boundary (Bb), it must be "wrapped"
(modulo) for the gamut, resulting in the C# pitch instead of a B.
This illustrates how non-standard gamuts create interest: in a 12-gamut the
chroma would be identical; a D above the octave "sounds the same" as
the D below. Here, having a C# instead of a B adds new pitch
information to the composition.

Of Pleonid's 15 tuples produced above, 3 tuples are un-steerable but
the rest are quite productive, producing 49 distinct steerings.

Preserving seed melody features, Part 1
----------------------------------------

The 49 steerings created have an almost random character,
except for their "source" being an interval derived from the scale
tuples. To amplify the intervallic character, the steerings are
filtered by a rule where at least one of the steering tuples must be
built from the seed scale.

With the `[0,2,3,4,5,7]` seed scale, the steering `[0,2,3,5,7],[4,6,8,9,1]` is allowed, since
the first tuple `[0,2,3,5,7]` can be built from the seed scale. Meanwhile, the
steering `[0,1,3,5,7],[2,4,6,8,9]` is discarded, since both tuples
have notes outside of the seed scale.

This filter reduces the 49 steerings to 32.

Preserving seed melody features (or not), Part 2
---

The tuples then undergo
a mapping procedure, designed to map any scale tones back to the
register they appear in the original seed sequence. So for instance, if a
`G` appears in a chord, it would be mapped to below middle-C.

![Mapping pitches to their register in the original seed sequence.](figures/pleonid/06-mappingCorrect.png)

As only discovered later, when migrating the code to Haskell, the original code
had a bug, resulting in a fairly chaotic remapping of pitches:

![Actual mapping in Pleonid code.](figures/pleonid/07-mappingIncorrect.png)

Indeed, the attempt to "preserve seed melody features" more or less
fails: instead of preserving seed-scale tones in the melody, the buggy
mapping distorts seed-scale pitches *more* than non-seed-scale ones. Nonetheless,
these bugs form part of the character of the piece, adding some intervallic
interest to a gamut-limited set of chords.


Pitch sequences from steerings
-----------------

The 32 modified steerings are made into pitch sequences or "lines" by arpeggiating
the tuples constituting the steering, and then interleaving the results.

![Interleaving steerings to generate lines.](figures/pleonid/08-interleave.png)

I wanted to avoid monotonically increasing or decreasing lines, so these are filtered
from the results. However this results in only one result filtered. We end up
with 31 sequences, which represent the totality of pitch material used in the composition.

Rhythm + Pitch = Motif: Braids for Rhythmic Generation
===============================

The 31 sequences are given rhythmic character to form the main motivic "lines" of
the piece. This is accomplished
by projecting the pitch sequences onto a strand of a *braid*.

Braids are 2-dimensional representations of knots, used in a [branch of mathematics](https://en.wikipedia.org/wiki/Knot_theory) that
seeks to describe and understand the essential features of knots, with applications in
bioinformatics, fluid mechanics and cryptography. Braids themselves form a
[group](https://en.wikipedia.org/wiki/Braid_group), but most significantly for Pleonid,
they present a cartesian-style left-to-right "plot" of information that
resembles music notation or "piano rolls".

![A braid from Pleonid. The numbers index the vertical position to pitches.](figures/pleonid/braid_strands_05-20x7.png)

Braid musical features
----------------------

A braid is built from "strands" which proceed from left to right; as
such, each strand represents a voice changing pitch in time. Positions
on the x-axis are fixed time intervals (like an eighth
note). Positions on the y-axis are *indexed* to a scale, instead
of a direct y-axis representation of pitch.

In braids, strands cross each other by going "over" or "under" the adjacent
strand. Indeed, these crossings form the essential algebra of braids: describing
these crossings alone is enough to identify any braid. However, when using
braids for music, we must decide on what the polarity of these crosses mean.

In Pleonid, the crosses are used to describe non-adjacent pitch changes. An "over"
cross, as well as a flat non-cross, is interpreted as a note sounding at the indexed
pitch. If that note changes to an adjacent pitch index, that is simply a single
"over" cross; however if the note changes to a value two indexes away, only the first cross
is "over", followed by "under" crosses until the target pitch is reached.

Projecting a pitch sequence as a strand
---------------------------------------

The first step in using braids in Pleonid is to project a pitch sequence
as a strand of a braid, travelling from pitch to pitch as indexed on the Y-axis.

A sequence strand starts at the index of the first pitch, immediately traveling
to the next pitch. Pitches further away on the index take longer to get to,
resulting in longer notes.

The last note of a sequence requires a strategy, as there is no more information
for how to direct the strand: the strand simply "arrives" at the last pitch.
In Pleonid, this is considered the end of the strand, with some associated information
loss. Other strategies could include "looping" back to the first value, but as we
will see, this would result in less-interesting braids. In Pleonid, we simply give this
last value the minimum time length, an eighth note.

![The source line.](figures/pleonid/10-strandSource__small.png)

![The strand generated from the source line.](figures/pleonid/strand_05.png)

![The resultant melody.](figures/pleonid/11-strandResult.png)

Elaboration: Braids for Counterpoint
====

Strands in a braid present an opportunity for musical polyphony.
The creation of the strand is indeed enough to make an entire braid, with
the other strands simply crossing as instructed by the single strand. This
would not make for interesting accompanying parts, just long tones with
infrequent, single-index pitch changes. In Pleonid, braids are further
elaborated using a "terracing" strategy, vertically inverting and reflecting the source strand material.

Each transition in the source strand is seen as a "column" or "step", being either
UP, DOWN or FLAT; the cross type is identified as OVER or UNDER. Moving away vertically from
the strand step, FLATs and crosses are alternated; when a cross is
generated, it is "reflected" from the previous cross --  a DOWN becomes an UP
and an OVER becomes an UNDER. The alternating flats and inverting crosses
produces a visually "terraced" effect. At the vertical edges, duplicate FLATs are
inserted if a cross would violate the braid boundary.

Notably, here too a "bug" was discovered in the original code, where the terracing
procedure was slightly different when moving downward from the strand as opposed to upward.
Again, the bugs inform the character of the piece, and the Haskell version reproduces the
bug with a parameter to restore uniformity if desired.

![Elaborating a braid. The "terracing" result is shown on every-other step to illustrate the columnar approach.](figures/pleonid/strand_tcol_01__small.png)

![The resulting braid.](figures/pleonid/braid_strands_01-11x6__small.png)

Braid Loops
-----------------

While a braid appears to have as many strands as Y-indexes, the strands actually
"loop" onto each other any time they terminate at a different position than they started.
This is an important feature for knot representations, as a complex braid might describe
only a single loop!

![Braid loops, color-coded: this braid has 7 strands but only 3 loops.](figures/pleonid/braid_seqs_13-20x7.png)

Loops have compelling musical properties, as interpreted in Pleonid: a loop length will always
be a multiple of the "braid length" or number of steps. If the braid length is seen as a "bar",
single-strand loops will repeat every bar, while 2-loops will only repeat every two bars, and so forth,
creating easily recognizable temporal interplay between parts. Indeed in Pleonid loops, not strands,
are assigned to parts, so a braid of the same "height" often results in different polyphony.

![Braid loops realized. The first staff is a 4-bar phrase, the second 1-bar, the third 2-bar.](figures/pleonid/braid-13.png)

In the figure, the braid loops shown above are orchestrated for three parts, using a 10-semitone register divide.
The top staff has the longest loop, lasting the full 4 bars. The second staff has the shortest loop, repeating a
one-bar figure 4 times. The third staff has a 2-bar figure, repeated twice.

Braid Chords
============

The 31 melodic figures, or "lines", have created 31 braids. The final generative procedure
analyzes the




I also chose 10 as a register "layout" for the quintet
instrumentation. 10, the "dominant 7" interval, is near the major- and
minor-sixth interval that makes for sonorous and pleasing
voicings. Meanwhile each voice ends up with a harmonic/melodic
"neighborhood" distinct from the other instruments.
