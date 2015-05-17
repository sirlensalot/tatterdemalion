% Algorithmic Compositional Techniques Developed for "Pleonid"
% Stuart Popejoy
% May 2015

"Pleonid" (2012, Stuart Popejoy) is an algorithmic composition for
quintet. This paper describes the methods used for its creation, "warts
and all". It also discusses the advantages found after porting the entire
composition from Java into Haskell, a typed functional programming language.

Generative Techniques for Pitch Sequences
=========================================

"Pleonid" employs a minimum of random or stochastic techniques preferring a generative, elaborative approach. The "seed" of the entire composition is a melodic sequence.

![Seed melody of *Pleonid*.](figures/01-seed.png)

This is normalized into a *pitch class set* of value `[0,2,3,4,5,7]`
(No. 6-8 in Forte classification). Departing from set theory, I use this
ordered sequence as a "scale" for further transformations.

![Pitch class set and main scale.](figures/01a-seedScale.png)

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
create interesting non-octave symmetries.

However I also chose 10 as a register "layout" for the quintet
instrumentation. 10, the "dominant 7" interval, is near the major- and
minor-sixth interval that makes for sonorous and pleasing
voicings. Meanwhile each voice ends up with a harmonic/melodic
"neighborhood" distinct from the other instruments.


Generate N-ads
--------------

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

This is a first illustration of the admirable expressiveness of functional 
programming. The corresponding Java code is considerably longer. 

An example is to take the sequence `C D E F`. The intervals are `[2,2,1]`, and 
the resulting tuples are `[[2,2,1],[2,3],[4,1],[5]]`.

![Generating all "N-ads" from a sequence.](figures/02-genNAds.png)

In *Pleonid*, the scale produces 16 N-ads, of which I remove the final dyad
`(0,7)` as trivial/uninteresting.

![Pleonid scale tuples.](figures/02a-genNAdsPleonid.png)

The Tone Clock
--------------

The motivation for generating these interval sets, or chords, is to subdivide
a scale or gamut in a way that will generate interesting and diverse intervallic
material. The approach used was inspired by procedures invented by the Dutch composer
Peter Schat which he called the "Tone Clock" (*Toonklok*).

The tone clock represents the 12 distinct triads that can "fit" into
the 12-tone gamut.  Triads are represented as *normalized, reversible*
identities. Normalization picks the most compact version, like
pitch-class sets. Thus the major triad is represented as `(4,3)`, the
first inversion, instead of the 2nd inversion `(3,5)` or 3rd
`(5,4)`. They are reversible such that the major triad is
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

![Steerings of Hour IX, the major/minor triad identity in the Tone Clock.](figures/03-ixSteerings.png)

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

It is also straightforward to apply the operation to gamuts other than
12, although relaxing the saturation requirement is productive. Triads
combine with the 12-gamut very well, in that 3 divides perfectly into
12. Pleonid's 10-gamut would only allow 5-tuples if they needed to saturate
the gamut. 

By removing this constraint I was able to "steer" other tuple
sizes. This still preserves just the character of overlapping tuple
inversion "placements" such that their note values do not collide.
Even so, some chords will not "steer", either because the note count is
too large, or the intervals do not permit any further placements. 

The next generative procedure in the Pleonid composition, then, is to
search out all of the relaxed "steerings" of each of the 15 scale
tuples above in a 10-gamut. 

The outcome was very successful, with only three tuples being
"un-steerable". An example of the results of the steering procedure is
the 3rd tuple, the 5-tuple `(2,1,2,2)`. This steers into the 10-gamut two ways:

![Steering (2,1,2,2) in a 10-gamut.](figures/05-pleoSteering3.png)

The last tuple of the second steering is noteworthy, as the last note
is "gamut-wrapped". The rotation of the interval is `(2,2,1,2)` but since
the last note goes above the gamut boundary (Bb), it must be "wrapped"
(modulo) for the gamut, resulting in the C# pitch. 

This is one way non-standard gamuts create interest; in a 12-gamut the
chroma would be identical, but here, the C# creates new intervallic
information.

Preserving/projecting seed melody features
------------------------------------------

The steering procedure creates 49 distinct steerings of almost random
character except for their "source" being an interval derived from the
scale tuples. To amplify the intervallic character, the steerings
are filtered by a rule where at least one of the steering tuples
must be built from the source scale. 

Thus, the steering `[0,2,3,5,7],[4,6,8,9,1]` is allowed, since
`[0,2,3,5,7]` is in the source scale `[0,2,3,4,5,7]`. Meanwhile, the
steering `[0,1,3,5,7],[2,4,6,8,9]` is discarded, since both tuples
have notes outside of the source scale. 

This filter reduces the steering count to 32. These are then
concatenated and de-duplicated to yield a sequence of 36 different
"chords" or tuples which will form the basis of the composition.

These chords then undergo a somewhat botched mapping procedure. 
The intent was to map any scale tones back to the register they
appear in the original seed motif. So for instance, if a `G` appears
in a chord, it would be mapped to below middle-C. 

The original Java code however had a bug which was not corrected
before performance, and was only revealed in the Haskell rewrite. 
As a result, the pitches selected for remapping were more or less
randomly chosen. The mapping was at least stable, so most of the
*structural* properties were preserved. And at least the intent
of adding more intervallic interest to a gamut-limited set of chords
succeeded. 

As we will see this was not the last unintended consequence of overly
complex procedural code. Finding issues like this during the rewrite
was distressing from a formal point of view, and makes me happy to 
be using a more powerful, expressive and correct language going forward.

Lines from chords
-----------------

At this point the chords were complete



