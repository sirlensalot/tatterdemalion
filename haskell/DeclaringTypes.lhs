% Declaring Types
% Stuart Popejoy
% December 2014

*Author's Note: this article continues a small series introducing
Haskell to experienced programmers. The [first
installment](http://slpopejoy.github.io/2014/11/27/FunctionApplicationDefinition/)
covered basics of function definition and application.*

In the last article, we looked at some surprising things hidden in basic features of the Haskell language. In this article we continue this investigation by looking at type signatures.

Type Synonyms
=============

Haskell provides the ability to build new types from existing types, in a
manner that is superficially similar to `typedef` in C/C++. If we type

> type ProgrammingLanguage = String
> type Year = Int

we now have a type synonym `ProgrammingLanguage` that is
interchangeable with `String`; likewise `Year` is synonymous with `Int`. 
We can use these to make a nicely "literate" function signature:

> getTIOBEScore :: ProgrammingLanguage -> Year -> Double
> getTIOBEScore pl y = undefined

(I'm using `undefined` here to declare a function type without
implementing it. Evaluating `undefined` at runtime causes an
[exception to be
raised](http://hackage.haskell.org/package/base-4.7.0.1/docs/Prelude.html#v:undefined),
but it will compile just fine, as `undefined` "inhabits all
types". Leaving functions undefined is a way to test that you've "got
the types right" before implementing, which can be very useful.)

The type of `getTIOBEScore` is certainly more informative than just
`String -> Int -> Double`, but beyond documentation, it's
identical. We're not required to use those types to call the function:

> aString :: String
> aString = "Haskell"
>
> aYear :: Int
> aYear = 2012
>
> callGetTIOBEScore = getTIOBEScore aString aYear

This code compiles fine, which you can try yourself using the [source](https://raw.githubusercontent.com/slpopejoy/tatterdemalion/master/haskell/DeclaringTypes.lhs) of this article. 

So far, `type` isn't buying us much, but that's because we're only
scratching the surface. Consider this type synonym:

> type StringIntTuple = (String, String)

`StringTuple` is a synonym for a "2-tuple" of String and Int. It's just like our
synonyms above, identical in usage to a bare tuple:

> firstOfStringIntTuple :: StringIntTuple -> String
> firstOfStringIntTuple = fst

`fst` is a Haskell function that extracts the first element of a 2-tuple; `snd`
gets the second element. Here we're doing the "functions as values" thing we 
introduced in the last article; we could write `firstOfStringIntTuple t = fst t`
to do the same thing, but why? `fst` does exactly what we want. 

In any case let's try it out:

> tryOutFSIT = firstOfStringIntTuple (aString, aYear) -- returns 2012

