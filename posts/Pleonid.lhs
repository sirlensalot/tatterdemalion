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
longer and more bug-prone. I give credit to "Cale" on the #haskell IRC
channel for coming up with the first version of this brilliant
formation, in response to my desperate plea for help. (There were other
equally delightful offers, one using monadic list notation).

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
a scale or gamut in a way that will generate interesting and diverse intervallic
material. The approach used was inspired by procedures invented by the Dutch composer
Peter Schat which he called the "Tone Clock" (*Toonklok*).

The tone clock represents the 12 distinct triads that can "fit" into
the 12-tone gamut.  Triads are represented as *normalized, invertible*
identities. Normalization picks the most compact version, like
pitch-class sets. Thus the major triad is represented as `(4,3)`, the
first inversion, instead of the 2nd inversion `(3,5)` or 3rd
`(5,4)`. They are *invertible* such that the major triad is
classified with its inverse, the minor triad `(3,4)`.

In the 12-tone scale, there are only 12 of these triad identities. 
Schat dubs these the "hours" of the "tone clock", presenting these as
foundational colors or harmonies to compose with.  He next observes
that each "hour" has a small number of ways the triad identity can be
placed in order to saturate all notes of the scale.

In the case of the major/minor triad identity, hour "IX" on his clock,
there are only two ways to produce this saturation:
major/minor/major/minor at `0,2,6,8`, and minor/minor/major/major at
`0,2,4,6`. Schat dubs these configurations "steerings", as though we are 
"steering" the intervals through the 12-tone scale.

![Steerings of Hour IX, the major/minor triad identity in the Tone Clock.](figures/pleonid/03-ixSteerings.png)

Each triad identity "hour" has a similarly fixed number of
configuration "steerings" possible, resulting in 33 total
configurations. Each configuration can be seen as producing a tetrad
of the bottom-note placements. For Schat these tetrads can be related
to another triad in the "clock" producing a relationship between "hours".

Steerings in Pleonid
--------------------

Steering can be generalized to apply to any N-ad, not just triads. Clearly
a dyad or a tetrad can be configured the same way within the scale. Of course,
with more than 2 intervals, we are no longer simply "inverting" the values
but *rotating* them, such that for the tetrad `(1,3,2)` we have `(3,2,1)` and
`(2,1,3)`.

![Rotations of the tetrad (1,3,2).](figures/pleonid/03a-tetradRotation__small.png)

To apply the operation to other gamuts than 12, relaxing the
saturation requirement proved to be productive.  Triads combine with
the 12-gamut very well, in that 3 divides perfectly into 12. Pleonid's
10-gamut would only allow 5-tuples if they needed to saturate the
gamut. 

By removing this constraint, I was able to "steer" most tuples below
1/2 the size of the gamut. This still preserves the character of
overlapping tuple configurations such that their note values do not
collide. Even so, some chords will not "steer", either because the
note count is too large, or the intervals do not permit any further
placements.

Steering search
---------------

To steer a tuple, I have to search for all valid placements of a tuple
in the gamut. I leverage the observation that every N-ad is steered by
another "M-ad" where `M` is N divided by the gamut. Thus a 5-tuple is
steered by some dyad (2-tuple) in a 10-gamut.

I generate every rotation of every possible normalized M-ad in the
gamut. For each M-ad, I evaluate every possible configuration of the
tuple.  With the 5-tuple and dyad example, I would try all 5 rotations
of the 5 tuple in both "slots", resulting in 25
configurations. Collisions and duplicates are removed, producing the
final result, 2 "steerings" or valid configurations.

This is performed for the 15 scale tuples shown above. Only 3 tuples
are "un-steerable," while the rest were quite productive, producing 49
distinct steerings. For an example, the 5-tuple `(2,1,2,2)` is found
to steer into the 10-gamut two ways:

![Steering (2,1,2,2) in a 10-gamut.](figures/pleonid/05-pleoSteering3.png)

The last tuple of the second steering is noteworthy, as the last note
is "gamut-wrapped". The rotation of the interval is `(2,2,1,2)` but since
the last note goes above the gamut boundary (Bb), it must be "wrapped"
(modulo) for the gamut, resulting in the C# pitch instead of a B.

This is one way non-standard gamuts create interest. In a 12-gamut the
chroma would be identical: a D above the octave "sounds the same" as
the D below. Here, C# sounds quite different than B, adding new pitch
information to the composition.

Preserving seed melody features (or not)
----------------------------------------

The 49 distinct steerings created have an almost random character,
except for their "source" being an interval derived from the scale
tuples. To amplify the intervallic character, the steerings are
filtered by a rule where at least one of the steering tuples must be
built from the source scale.

Thus, the steering `[0,2,3,5,7],[4,6,8,9,1]` is allowed, since
`[0,2,3,5,7]` is in the source scale `[0,2,3,4,5,7]`. Meanwhile, the
steering `[0,1,3,5,7],[2,4,6,8,9]` is discarded, since both tuples
have notes outside of the source scale. 

This filter reduces the 49 steerings to 32. The tuples then undergo
a mapping procedure, intended to map any scale tones back to the
register they appear in the original seed sequence. So for instance, if a
`G` appears in a chord, it would be mapped to below middle-C.

![Intended mapping of pitches oto original seed sequence.](figures/pleonid/06-mappingCorrect.png)

This mapping procedure did not go as planned however: a bug in the original
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

This results in 31 lines. These form the foundation of the actual musical
composition. We're ready to create real musical ideas.

Rhythm + Melody = Motif: Braids
===============================

Working in the pitch domain lends itself to countless procedures and
transformations, not to mention endless melodic and harmonic contexts
to work with. Rhythm puts up more of a resistance to formalization and
proceduralization. 

My approach to applying rhythmic structure to melodies is inspired and
powered by a form of notation used in knot theory in mathmatics, called
*braid notation*. This allows a normalized knot presentation that
captures fundamental features: 

1. How many distinct strands constitute the braid. You can tie a knot with one or many strands.

2. Where, and how many times, a cross occurs. You can obviously cross
a strand with itself, or with other strands.

3. The "polarity" of the cross: over-under or under-over.

![A braid from Pleonid.](figures/pleonid/braid00.png)

In Pleonid, braid notation is used to structure the generation of related musical motives.
All of the features of a braid are given specific musical meaning. 

Braid dimensions
----------------

Any braid has a width and a height; the braid in the figure above is of height 7
and width 21. Like music notation, I interpret the y-axis to be in the pitch domain,
while the x-axis is time. However, the y-axis does not represent scale degrees,
but is instead *indexed* to particular pitch values. 

Thus the example has 7 distinct pitches or "degrees" in play. Future
work will consider the possibility of each strand having its own index
of 7 values, such that while the strands will all share the same
"indexing space", the indexed values will vary for each motif. In
Pleonid, these are indexed to specific pitch values: as such, insofar
as strands represent different parts, they are all playing in the same
"key" (modulo transposition).

On the x-axis, time moves consistently forward; in Pleonid these are generally eighth-notes.
Thus the example is 21 eighths long, perhaps represented as 21/8 time, or 12/8 + 9/8, etc.

Strands vs sequences
--------------------

In braids, the number of positions or "degrees" does not represent the number of unique
strands, since the braid is seen as a loop. Thus a braid that starts in position 1 but
ends in position 2 "continues" into another "frame" of the braid. 

I use the term "sequence" or "strand sequence" to indicate strands
longer than the braid length, reserving "strand" for the single
traversal across the braid. While the example has 7 "strands", it has only 3
"sequences".

![The three sequences of the 7x21 braid.](figures/pleonid/braid00-sequences.png)

The first strand starts and ends at the same, topmost degree, so it
forms a sequence of one strand. The second strand starts at the second
degree down, but ends on the third.  It thus continues "into" strand
three, which itself ends on degree four, continuing again.  Finally
this strand ends back on the second degree, completing the loop and
making a sequence of 3 strands, here called sequence 2. The fifth,
sixth and seventh strands form sequence 3.





 while the hor 



Pleonid, it takes the following form.


The y-axis is generally in the pitch domain,
With braid notation, I saw a way to generate 
Braids offer one way to "weave" phrases, motifs with rhythm and melody. With 
the pitch domain on the y-axis, and time on the x-axis, braid notation 
offers a form of counterpoint, with over-and-under guiding the phrasing.

There are any number of ways to interpret a braid as music. Pleonid's approach
is to interpret 



Instrumentation and Arrangement
===============================

I also chose 10 as a register "layout" for the quintet
instrumentation. 10, the "dominant 7" interval, is near the major- and
minor-sixth interval that makes for sonorous and pleasing
voicings. Meanwhile each voice ends up with a harmonic/melodic
"neighborhood" distinct from the other instruments.
