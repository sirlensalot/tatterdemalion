----------
title: Algorithmic Compositional Techniques Developed for "Pleonid"
author: Stuart Popejoy
date: 2015-05-15
----------

"Pleonid" (2012, Stuart Popejoy) is an algorithmic composition for
quintet. This paper describes the methods used for its creation, "warts
and all". It also discusses the advantages found after porting the entire
composition from Java into Haskell, a typed functional programming language.

Generative Techniques for Pitch Sequences
=========================================

"Pleonid" employs a minimum of random or stochastic techniques
preferring a generative, elaborative approach. The first phase of
generation involves manipulating pitch sets to arrive at a series of
pitch sequences, or "lines", which will form the basis of the piece.
The path to these lines is covered in this section.

The "seed" of the entire composition is a melodic sequence.

![Seed melody of *Pleonid*.](figures/pleonid/01-seed__small.png)

This is normalized into a *pitch class set* of value `[0,2,3,4,5,7]`
(No. 6-8 in Forte classification). Departing from set theory, I use this
ordered sequence as a "scale" for further transformations.

![Pitch class set and main scale.](figures/pleonid/01a-seedScale__small.png)

Gamut
-----

One of the properties of the sequence/scale is its range, namely that
it is less than the 12-semitone range of a standard scale. The term
"gamut" is used to describe this range, with 12 being the usual value
in Western equal-tempered music. For our purposes, gamut is preferred
to "octave" which invokes the chroma identity.

Using gamuts other than 12 create interest by effectively transposing
pitches by register, as well as limiting or expanding the search space
for intervals and melodies.

In this case of the pitch class set-sequence, the reduced range
creates an effective gamut of 7. For many procedures in Pleonid I use
a gamut of 10, such as for the "steerings" described below, mainly to
create interesting non-octave symmetries. When we discuss orchestration
below we'll see 10 used again for its intervallic character.

Generating N-ads
----------------

The first transformation is to generate every chord or "sub-scale" that
can be built from this scale/sequence.

The interval vector of the pitch set is used, that is the ordered
intervals or deltas between the pitches: `[2,1,1,1,2]`. By enumerating
every possible ordered sum of these values, I determine every chord
or "N-ad" (dyad, triad, tetrad etc) that can be projected onto this
sequence.

The function `genNAds` achieves this with concatenation of two
recursive list comprehensions:

> genNAds :: Num a => [a] -> [[a]]
> genNAds []     = [[]]
> genNAds (x:xs) = [x:ps | ps <- genNAds xs] ++ 
>                      [(x+p):ps | p:ps <- genNAds xs]

This is a first illustration of the admirable expressiveness of
functional programming. The corresponding Java code is considerably
longer and more bug-prone. ^[Credit is due to "Cale" on the #haskell
IRC channel for producing this brilliant formation in response to my
desperate plea for help. There were other equally delightful offers,
one using monadic list notation.]

To illustrate, let's take the sequence `C D E F`, whose intervals are `[2,2,1]`.
The resulting tuples are `[[2,2,1],[2,3],[4,1],[5]]`.

![Generating all "N-ads" from a sequence.](figures/pleonid/02-genNAds__small.png)

Performing this operation on the Pleonid interval vector `[2,1,1,1,2]`
produces 16 tuples. Like the example, the final dyad simply bounds the scale, 
so I drop it as trivial/uninteresting. Note the full scale is the first tuple.

![Pleonid scale tuples.](figures/pleonid/02a-genNAdsPleonid.png)

The Tone Clock
--------------

The motivation for generating these interval sets, or chords, is to subdivide
a scale or gamut in a way to generate diverse yet related intervallic
material. The approach borrows from by procedures invented by the Dutch composer
Peter Schat, called the "Tone Clock." ^[Schat, Peter (1993). Also see <http://en.wikipedia.org/wiki/Tone_Clock> and <http://www.peterschat.nl/clockwise.html>. Thanks to Jochem van Dijk for hipping me to it.]

The tone clock represents the 12 distinct triads that can be "set"
into the 12-tone gamut. Triads are identified as *normalized,
invertible* classes. Thus the major triad is represented as `(4,3)`
and classified with its inverse, the minor triad `(3,4)`.

The 12-tone scale has 12 of these triad classes. Schat dubs these the
"hours" of the "tone clock". Each "hour" has a fixed number of
configurations by which the triad class can be placed to saturate all
notes of the scale.

In the case of the major/minor triad, hour "IX" on his clock, there
are exactly two configurations: major/minor/major/minor at `0,2,6,8`,
and minor/minor/major/major at `0,2,4,6`. Schat calls these
configurations "steerings", as if we were "steering" the chord
through the 12-tone gamut.

![Steerings of "Hour IX", the major/minor triad class in the Tone Clock.](figures/pleonid/03-ixSteerings.png)

Each triad class or "hour" has a fixed number of
configuration "steerings", with 33 total configurations over all classes.
Each configuration can be seen as producing a tetrad
of the bottom-note placements. ^[For Schat these tetrads can be related
intervallically to other triad hours in the clock, producing a relationship between hours.] 

Steerings in Pleonid
--------------------

The classification and steering of a triad can be applied to apply to any N-ad. Clearly
a dyad or a tetrad can be configured the same way within the scale. Of course,
with more than 2 intervals, we are no longer simply "inverting" the chord
but *rotating* it, such that for the tetrad `(1,3,2)` we have `(3,2,1)` and
`(2,1,3)`.

![Rotations of the tetrad (1,3,2).](figures/pleonid/03a-tetradRotation__small.png)

Likewise, the gamut can be other values than 12. 

With different chord sizes and gamuts, total saturation (i.e., "using
up" all of the gamut values with a steering configuration) becomes
difficult or impossible. In Pleonid this requirement is removed,
instead searching for *maximal* instead of *total* saturation.

By relaxing this constraint, I was able to "steer" most tuples below
1/2 the size of the gamut of 10. Even so, some chords do not
"steer", either because the note count is too large, or the intervals
do not permit any further placements.

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

For example, finding all steerings of the 5-tuple `(2,1,2,2)` in a 10-gamut
means searching every possible dyad (90 total) with every possible 
rotation (4 total) resulting in 360 candidates. Some optimizations are possible,
for instance dyads having intervals larger than 1/2 the gamut size are 
"un-steerable". For this 5-tuple, just 2 steerings are found.

![Steering (2,1,2,2) in a 10-gamut.](figures/pleonid/05-pleoSteering3.png)

The last tuple of the second steering is noteworthy, as the last note
is "gamut-wrapped". The rotation of the interval is `(2,2,1,2)` but since
the last note goes above the gamut boundary (Bb), it must be "wrapped"
(modulo) for the gamut, resulting in the C# pitch instead of a B.

This is one way non-standard gamuts create interest. In a 12-gamut the
chroma would be identical: a D above the octave "sounds the same" as
the D below. Here, C# sounds quite different than B, adding new pitch
information to the composition.

This is performed for the 15 scale tuples shown above. Only 3 tuples
are "un-steerable," while the rest were quite productive, producing 49
distinct steerings.

Preserving seed melody features (or not)
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

This filter reduces the 49 steerings to 32. The tuples then undergo
a mapping procedure, intended to map any scale tones back to the
register they appear in the original seed sequence. So for instance, if a
`G` appears in a chord, it would be mapped to below middle-C.

![Intended mapping of pitches onto original seed sequence.](figures/pleonid/06-mappingCorrect.png)

This mapping procedure did not go as planned however! A bug in the original
Java code resulted in a more or less random mapping. 

![Buggy mapping makes for unplanned results.](figures/pleonid/07-mappingIncorrect.png)

Thus the attempt to "preserve seed melody features" more or less
fails. The filtering above prefers tuples carrying the seed-scale
pitches, but the buggy mapping affects seed-scale pitches *more* than
non-seed-scale ones. Thus these very pitches are the most
distorted. At least, the mapping succeeds in adding some intervallic
interest to a gamut-limited set of chords.

This bug was only discovered during the port to Haskell. Like much
combinatorial code in Java, the mapping code was creaky and complex,
while the Haskell code is concise and far simpler. I now
have both methods, the broken and the correct one. Future works will
choose which "sounds better". So much for formal purity!

Lines from chords
-----------------

We now have 32 steerings, which group tuples of a particular size: 2 5-tuples,
or 3 3-tuples, etc. Melodies are generated from this by simply interleaving
the values to create a longer line. 

![Interleaving steerings to generate lines.](figures/pleonid/08-interleave.png)

To maximize interest, monotonically increasing or decreasing lines --
lines that only move in one direction -- are discarded. Interestingly
this results in only one result filtered. 

This results in 31 lines, which form the foundation of the actual musical
composition. We're ready to create real musical ideas.

Rhythm + Melody = Motif: Braids
===============================

The next step in composing Pleonid is to use the pitch information 
in the lines to produce rhythmic information. This is accomplished
by projecting the pitch sequences into a "strand" and using this
to generate a "braid" of contrapuntally-related motives.

My concept of braids borrows from *knot theory* in mathematics, in which
idealized knots or braids are formalized as polynomials, and categorized
by salient features such as how many crosses occur, how many strands 
are being tied, etc. ^[Knot theory is obscure but has contributions from
Markov and others, with applications in bioinformatics, fluid mechanics, cryptography. 
See <http://en.wikipedia.org/wiki/Knot_theory>, <http://en.wikipedia.org/wiki/Braid_group>.]

A braid in Pleonid is derived from the "braid representation" of a knot. ^[See <http://katlas.org/wiki/Braid_Representatives> for examples of braid generation from knots in Mathematica.]
It is a two-dimensional representation, of fixed dimension, where strands cross
over and under each other as they move horizontally. In Pleonid, all of these
features are given musical meaning.

![A braid from Pleonid. The numbers on the left index the vertical position to pitches.](figures/pleonid/braid_strands_05-20x7.png) 

Braid musical features
----------------------

A braid is built from "strands" which proceed from left to right; as
such, each strand represents a voice changing pitch in time. Positions
on the x-axis are fixed time intervals (like an eighth
note). Positions on the y-axis are *indexed* to a scale, instead
of a direct y-axis representation of pitch (ie chromatic).

In braids, any change in y-position indicates a *cross*, meaning another
strand must make the complementary change at the same x-position. The cross
has a particular polarity such that one strand is crossing "over" the other.

For musical assignment, an "under" cross is seen as a "hidden" pitch change,
meaning the previous pitch still plays for that unit of time. An "over" cross
means an immediate shift to the indexed pitch. 

Projecting a pitch sequence as a strand
---------------------------------------

The first step is to "project" a pitch sequence as a strand.
An index is made from the sorted set of all pitches used, so 
that pitches will be plotted on the y-axis at their index.

The first "step" consists of the pitch index and a "weave", which 
is an instruction to move to the next x-position. The "weave" dictates
if the next move is UP, DOWN, or FLAT; if not FLAT, it also indicates
if the move is UNDER or OVER. 

The algorithm locates the next pitch index in the sequence. If
adjacent to the current index, it assigns an OVER transition with UP
or DOWN. If the pitch index is the same, it assigns FLAT.

If the pitch index is not adjacent, it assigns UNDER, with a direction
UP or DOWN which will "lead to" the next index. This continues until
the next pitch index is reached.

The process repeats until the last pitch, which does not need its own
"step" since the index has already been determined by the steps leading up
to it. 

![The source line.](figures/pleonid/10-strandSource__small.png)

![The strand generated from the source line.](figures/pleonid/strand_05.png)

The resulting rhythmic behavior of the projection turns the pitch sequence
into a motive. A strange feature of the strand realization is the last pitch.
Since there is no "step" governing it, it is assigned an "extra" time value.

![The strand realized as motive.](figures/pleonid/11-strandResult.png)

Braid generation from a strand: contrapuntal motives
----------------------------------------------------

They rules also allow *generation* of contrapuntal motives to go with the
source strand. 


Braid "sequences"
-----------------

The resulting braids have an interesting property: strands that end
on a different y-position than where they started "join" with a different
strand that starts at that position. This is how braids represent *loops*
in knots. Since loops in music have a different connotation, I called 
these longer collections of strands "sequences".


!["Sequences" of a braid.](figures/pleonid/braid_seqs_05-20x7.png)


This adds an interesting dimension to the motifs that can be generated
by braids. Sequences are not necessarily the same length, but they
are a multiple of the overall braid x-dimension. This creates
multi-"bar" motives with shorter motives repeating.

Instrumentation and Arrangement
===============================

I also chose 10 as a register "layout" for the quintet
instrumentation. 10, the "dominant 7" interval, is near the major- and
minor-sixth interval that makes for sonorous and pleasing
voicings. Meanwhile each voice ends up with a harmonic/melodic
"neighborhood" distinct from the other instruments.

