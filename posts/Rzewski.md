----------
title: "Squaring" and triangle numbers in Rzewski's Coming Together
author: Stuart Popejoy
date: 2021-01-13
----------

Abstract
===

Rzewski's _Coming Together_ presents a strong concept to exploit aleatory elaboration of what
is otherwise an austere, procedurally through-composed monophonic epic bassline, paired
with the evocative recitation of text snippets from the prison letters of X while at the Attica
prison, leading up to the uprising in YEAR. Focusing on the procedural method of the bassline,
we investigate interesting features of Rzewski's "squaring" concept and how numerical features of
that approach are uniquely exploited to make a unified _temporal_ construction, with some observations
on possible compositional motivations for the approach. Finally we present an alternate scoring
of the main line to aid the bassline performer in coming to terms with the musical and rhythmic structure
therein.

Introduction
===

My first exposure to _Coming Together_ is as a performer, specifically as a bass guitarist playing the main
line. Since then I've noticed that while electric bass is suggested in Rzewski's performance instructions
for the piece, a surprisingly small portion of realizations have bass guitar at all, and even less giving
the line entirely to that instrument. The 20th anniversary of
the Attica uprising in 2021 inspired me to take up the piece again with the intention of organizing a new
performance, with the goal of organizing a new realization of the piece with remote performance techniques,
starting with a live recording of my performance of the bassline.

Approaching the score as a bassline performer is quite intimidating. The torrent of notes is constantly
changing, and while the concept starts out with an easier-to-percieve structure, it's not long before
the cascade of permutations is extremely difficult to have a "feel" for, beyond straight woodshedding and
memorization (which would be a remarkable feat).

My work in algorithmic/computer-generated composition
inclined me to search for the algorithm such that I could express it in computer code.
Such an undertaking has musical benefits for the performer or composer, as another technique
to approach procedural methods that have been employed for centuries by composers and improvisors.
Expression in code can expose formality in a very tangible way,
especially when generation can be parameterized, as realizations of different parameterizations can
be experienced directly through software playback or scoring.

_Coming Together_'s bassline is entirely procedurally-generated using an elaborative technique that Rzewski calls
"squaring", and the result is a piece with 8 sections of equal length, 49 4/4 bars of continuous 16th notes. The first
"A" section is generated with two related procedures, while "B" starts with a third procedure before reusing one from
the "A" section. Subsequent sections are generated from these two employing inversion and/or retrogradation.
Given the scale of the overall work, this is quite an economical formalization, and the realized line
is very successful in providing contour and large-scale perceivable structure, even independently of the
other parts, dynamic instruction, etc.

Simply discovering this overall structure aids in performance. However I was also motivated to break out of the strict
4-note-to-a-beam scoring, so that as a performer I could "read" the structure while practicing and performing.
My Haskell software suite includes scoring via MusicXML output so this is another motivation for coding
the procedure, to try different methods to illustrate the structure. The final result is offered as a score
for performers to use for practicing and performance, with apologies to Rzewski for undermining the
presumably intentional austerity of presentation. My justification is to encourage more bassists to take
on this piece and add it to a very small classical literature for bass guitar in particular.

Finally, it is interesting to consider how Rzewski was able to create the procedure that starts the B section,
since it is faithful to some of the more mysterious aspects of "triangle numbers", which is the mathematical
domain of Rzewski's "squaring" technique, within analytical number theory. Indeed the very name "squaring"
reveals how Rzewski's engagement was as much "geometric" as numerical, in how these techniques are deployed
to achieve musical objectives, like a consistent 4/4-divisible overall length.

Fundamental procedures
===

The main line of the piece is built entirely from a sequence or "string" of 7 distinct pitches in a G minor pentatonic scale.
For our illustrations below we use a single-character mnemonic for each pitch.

```
Pitch:     G1  Bb1  C2  D2  F2  G2  Bb2
Mnemonic:  g   b    c   d   f   h   j
```
[Figure rzewski-notes]

Elaborating this line left-to-right iteratively produces 7 strings of increasing length: `g`, `g b`, `g b c`, `g b c d`, `g b c d f`, `g b c d f h`, `g b c d f h j`. Concatenating these together makes a string of 28 notes.

```
g g b g b c g b c d g b c d f g b c d f h g b c d f h j
```
[Figure rzewski-string]

The first half of the "A" section uses the same
elaboration now on the 28-note string, resulting in a final sequence of 406 notes.

```
g
g g
g g b
g g b g
...
g g b g b c g b c d g b c d f g b c d f h g b c d f
g g b g b c g b c d g b c d f g b c d f h g b c d f h
g g b g b c g b c d g b c d f g b c d f h g b c d f h j
```

The second half reverses
this procedure to iteratively reduce the 28-note by removing a note from the beginning of the list, resulting
in a 378-note sequence.

```
  g b g b c g b c d g b c d f g b c d f h g b c d f h j
    b g b c g b c d g b c d f g b c d f h g b c d f h j
      g b c g b c d g b c d f g b c d f h g b c d f h j
...
                                                  f h j
                                                    h j
                                                      j
```

## Triangular numbers

The length of the first concatenation (28) and of the two elaborated sequences (378 and 406) are all "triangular
numbers", from mathematical literature on _figurate numbers_: triangular numbers are the accumulating length of entries
in a triangular figuration.  Here is the primary Rzewski triangle, with the cumulative length being the triangular
number sequence `1 3 6 10 15 21 28`:

```
                   Rank  Length
          g        1     1
         g b       2     3
        g b c      3     6
       g b c d     4     10
      g b c d f    5     15
     g b c d f h   6     21
    g b c d f h j  7     28
```

Elaboration in the first part of section A forms a larger triangle with an
accumulated length of 406, "rank" 28:

```
Rank:   1     2     3      4      5       6    ...       27              28
Length: 1     3     6      10     15      21   ...       378             406
                                               ...
        g     g     g      g      g       g    ...        g               g
             g     g b    g b    g b     g b   ...       g b             g b
                         g      g b     g b c  ...      g b c           g b c
                                               ...     g b c d         g b c d
                                               ...    g b c d f       g b c d f
                                               ...   g b c d f h     g b c d f h
                                               ...  g b c d f h     g b c d f h j
```

The second half of the A section iteratively takes one note off the top for
a total length of 378. While this sequence
of lengths is not triangular, considering the lengths for the reverse of the procedure
clearly shows why the final total is triangular.

```
Rank:        27              26        ...     4         3        2      1
Length:      27              53        ...     372       375      377    378
Rev. length: 378             351       ...     10        6        3      1
                                       ...
             g b               b       ...
            g b c           g b c      ...
           g b c d         g b c d     ...
          g b c d f       g b c d f    ...
         g b c d f h     g b c d f h   ...
        g b c d f h j   g b c d f h j  ...   d f h j    f h j    h j     j
```

The total length of the A section is 406 + 378, or 784. Remarkably, this sum is
neatly divisible by 16, resulting in 49 4/4 bars of 16th notes.

## Triangles and "Squaring"

While the mathematical presentation of this procedure references triangles, Rzewski call his process "squaring". This is
undoubtedly because of the relationship of triangle figures to square figures, namely that every triangle can be combined
with an adjacent triangle in the sequence to form a square. The square's dimensions are the square of the rank of the
larger triangle.

```
Tri. Rank:     1      2        3           4
Length:        1      3        6           10

Triangle:      A      B        C           D
                     B B      C C         D D
                             C C C       D D D
                                        D D D D


Rank ^ 2:             4        9           16

Square:              A B     B B C      C C C D
                     B B     B C C      C C D D
                             C C C      C D D D
                                        D D D D

```

Clearly for Rzewski this relationship is important. Section A's length, 784, represents "squaring" the rank 28 triangle
(406) with the rank 27 triangle (378), and the length is the square of 28: 784.  28 factors to 7 * 4, so the
squared value of 4 (16) combines with 7 squared (49) to result in the 49 bars of 4/4 (really 16/16).

This is a musically remarkable fact; while 4/4 is a very regular or "even" time signature, the triangular procedure
arrives at this regularity with lengths that are decidedly independent of 16 bar units, resulting in copious syncopation
and rhythmic and motific richness. The choice of 7 notes in the original string is thus not arbitrary
but the exact length needed to produce the specific musical structure.

## Generation from a different source string



Further elaboration within the "squared" constraint
===

For section B, Rzewski employs a new generation procedure to once again produce 406 notes. Like section A,
the last 28 notes of the generated sequence is iteratively reduced, to form the same "square"
of 784 notes albeit with different pitch material. However, the generation procedure in section B is very different than the
simple, almost "automatic" triangular generation of section A.

Section B starts by repeating the full 28-note string built up in section A. Recall that this string is the triangular
elaboration of the 7-note core string, consisting of 7 sub-strings.

```
g   g b   g b c   g b c d   g b c d f   g b c d f h   g b c d f h j
```

The end of this generation procedure reverses the order of the 7 sub-strings, going instead from longest to shortest.

```
g b c d f h j   g b c d f h   g b c d f   g b c d   g b c   g b   g
```

This 28 note sequence is then reduced a la section A.

Thus, we can see that Rzewski's goal for this procedure is to mutate the "source" string into this new material, also 28
notes, that reverses the substrings. The triangular methods of section A are no help here, yet somehow the result has to
be the same length as the result of that triangular procedure. Rzewski accomplishes this goal in an almost magical
fashion.

## Iterative prefixing

Section B's elaboration procedure takes the list of strings and removes the last one to form a "prefix" prepended to
each of the remaining strings. As this process repeats, the new prefix is appended with the removed string. Eventually
all that is left is the prefix, whose construction has reversed the order of the strings.

To illustrate, let's look at a smaller construction, built from 4 strings instead of 7. If Rzewski had composed using a
4 note sequence, the main string would be 10 notes long.

```
g   g b   g b c   g b c d
```

The result of section A's generation would now be 45 notes long (the 10th-ranked triangle number). Section B
starts with the full presentation of the 4 strings. For reasons
that will be demonstrated, we will imagine an empty "prefix" prepended to these strings.

```
Prefix   String
         g
         g b
         g b c
         g b c d

Length: 10
```

The first iteration removes the last string `g b c d` and makes it into a prefix, which is prepended to each of the remaining
strings.

```
Prefix     String
g b c d    g
g b c d    g b
g b c d    g b c

Length: (4 * 3) + 6 = 18
```

The next iteration appends the last string `g b c` onto the prefix.

```
Prefix            String
g b c d  g b c    g
g b c d  g b c    g b

Length: (7 * 2) + 3 = 17
```

The last iteration appends the last string `a b` onto the prefix, and in doing so completes the reversal.

```
Prefix                 String
g b c d  g b c  g b    g

Length: (9 * 1) + 1 = 10
```

Mission accomplished: a rule-based iterative process has reversed the strings, and the resulting length,
10 + 18 + 17 + 10, equals the desired triangle number 55.

In _Coming Together_, based on the 7 string construction, these lengths are:

```
28                28
(6 * 7) + 21      63
(5 * 13) + 15     80
(4 * 18) + 10     82
(3 * 22) + 6      72
(2 * 25) + 3      53
(1 * 27) + 1      28

Length:           406
```
