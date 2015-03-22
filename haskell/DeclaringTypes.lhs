% Declaring Types
% Stuart Popejoy
% March 2015

*Author's Note: this article continues a small series introducing
Haskell to experienced programmers. The [first
installment](http://slpopejoy.github.io/2014/11/27/FunctionApplicationDefinition/)
covered basics of function definition and application.*

In the last article, we looked at some surprising things hidden in
basic features of the Haskell language. In this article we continue
this investigation by looking at type signatures.

Once again this article is "literate haskell"; and imports must precede code, so allow me to ...

> import Data.Map (Map, elems, keys)
> import Data.Char (toUpper)

... for use below.

The 'type' keyword
==================

Haskell provides the ability to build new types from existing types, in a
manner that is superficially similar to `typedef` in C/C++. Using the [type](https://wiki.haskell.org/Keywords#type)
keyword we can define the following:

> type ProgrammingLanguage = String
> type Year = Int
> type Score = Double

This creates a type synonym `ProgrammingLanguage` that is
interchangeable with `String`. Likewise, `Year` is synonymous with `Int`, 
and `Score` with `Double`.
 
We can use these to make a nicely "literate" function signature:

> getTIOBEScore :: ProgrammingLanguage -> Year -> Score
> getTIOBEScore pl y = undefined

Without the typedefs, `getTIOBEScore` would simply be `String -> Int -> Double`,
requiring us to stare at `pl` and `y` in the code to try to understand their function.
Under eta reduction, the definition can omit named variables altogether, making
the self-documenting type even more useful:

> getTIOBEScoreEtaReduce :: ProgrammingLanguage -> Year -> Score
> getTIOBEScoreEtaReduce = undefined

Note: [undefined](http://hackage.haskell.org/package/base-4.7.0.1/docs/Prelude.html#v:undefined)
is a magic function in Haskell that "inhabits all types". It's basically a TODO that
the compiler is OK with, and can allow you to "get the types right"
before writing an implementation. Here, I'm only interested in the types,
not the implementation; I leave the functions `undefined` so that 
my literate-haskell article will still compile. Understandably,
invoking `undefined` at runtime results in an error.

~~~~
ghci> getTIOBEScore "Haskell" 2013
*** Exception: Prelude.undefined
~~~~

Our typedefs have no value beyond documentation. We can invoke the
functions with just the "bare" underlying types, or with other synonyms:

> type OOProgLang = String
>
> java :: OOProgLang
> java = "Java"
>
> callGetTIOBEScoreOO = getTIOBEScore java 2014

Type variables
==============

So far, the `type` keyword is a simple beast, providing synonyms only.
Adding *type variables* increases its power significantly. 

To illustrate, we'll write some declarations using the `Map` type from
the `Data.Map` module, imported above. First let's declare synonyms
for fully-specified map types:

> -- index scores by language
> type LangScores = Map ProgrammingLanguage Score
> 
> -- enumerate language "families"
> data LangFamily = OO | FP | Blub
>
> -- map languages to families
> type LangFamilies = Map ProgrammingLanguage LangFamily

Simple enough. Now we can have signatures with `LangScores` or
`LangFamilies`. Saves on typing, but not much else is gained.

What the two maps have in common is their key type, `ProgrammingLanguage`.
We can write a type that captures this similarity, allowing us to abstract
over "maps keyed by language":

> type LangMap a = Map ProgrammingLanguage a

This creates an interesting type. The `a` after `LangMap` is a *type variable*,
which can be populated with any type, anywhere the type synonym is used.

> earliestYear :: LangMap Year -> Year
> earliestYear = minimum . elems

More point-free fun
-------------------

We looked at point-free style in the last article, so I'll continue by
unpacking the implementation of `earliestYear` above.

[elems](http://hackage.haskell.org/package/containers-0.5.6.3/docs/Data-Map-Lazy.html#v:elems) is a function that takes a Map
and returns its values as a list:

~~~~
ghci> :t elems
elems :: Map k a -> [a]
~~~~

[minimum](http://hackage.haskell.org/package/base-4.7.0.2/docs/Prelude.html#v:minimum) operates on ordered values, to return the minimum:

~~~~
ghci> :t minimum
minimum :: Ord a => [a] -> a
~~~~

So, our definition could have been simply 

~~~~haskell
earliestYear m = minimum (elems m)
~~~~

But any time one function consumes the input of another, we can simply compose them
with `(.)` to create a new function.

~~~~haskell
earliestYear m = (minimum . elems) m
~~~~

Clearly, `earliestYear`, as a function,
simply equals our composed function `(minimum . elems)`, since they take the same
input to produce the same output. So, we drop the argument altogether.

Type variables at the usage site
--------------------------------

We can use `LangMap` more polymorphically. Let's say we want
a function that can ensure two `LangMap`s have the same keys, regardless of value
type. To do this, we'll use type variables with `LangMap` in the function declaration:

> hasSameLangs :: LangMap a -> LangMap b -> Bool

Instead of fixing the type, we've employed type variables that will allow the
value types of the first and second arguments to vary. Note that if we had
used the *same* variable name, the types would not be allowed to vary: we need `a`
and `b` to allow the arguments to have different types.

This is an important point. We quickly discussed type variables in the
previous article when we examined the type of `$`, the function
application operator:

~~~~
ghci> :t ($)
($) :: (a -> b) -> a -> b
~~~~

The two type variables provide important constraints on the usage of
the function. `a` and `b` demarcate *distinct types*, guaranteeing
that the second argument (of type `a`) can safely be provided to the
unary function supplied in the first argument (of type `(a -> b)`).
Even though we have no idea what the types are, we have safely "locked in"
their usage with the function.

Let's go ahead and implement our simple function:

> a `hasSameLangs` b = keys a == keys b 

This uses an idiomatic Haskell style for defining binary functions,
using the backticks around `hasSameLangs` to define it "infix": we
could write `hasSameLangs a b = ...` to the same effect. Instead, the
infix style mirrors the use of `==` in the implementation, comparing the
`keys` of each map. You will see this style pop up often in Haskell
library source, so it's worth getting familiar with it.

Note that the use of "a" and "b" in the definition has **no relation**
to the `a` and `b` in the declaration. In Haskell, type variable scope
is entirely distinct from code, so there is no clash reusing the variable names.

Indeed, type declarations in Haskell form a kind of mini-language,
sporting operators, functions and all sorts of amazing and confusing
functionality.  The `type` keyword and the `::` operator drop us into
a distinct "syntax zone" of type declarations.

Kinds
=====

We've seen `type` used to simply create a type synonym (`ProgrammingLanguage`,
`Year`), and the more complex case of denoting polymorphically-related types
(`LangMap a`). Much like functions have their "arity" -- unary, binary etc -- types
have *kind*, indicating how many additional types are needed to fully populate it. 

To interrogate the "kind" of a type, we use the ":k" command in ghci.

~~~~
ghci> :k Year
Year :: *
ghci> :k ProgrammingLanguage
ProgrammingLanguage :: *
~~~~

"Simple" types are of kind `*`, which simply means they are "ready to go", like
a nullary function: no input is required to use them. `String`, `Int` and such are
ready to be inhabited by their values.

~~~~
ghci> :k LangMap
LangMap :: * -> *
~~~~

`LangMap` has a more complex kind, `* -> *`, meaning it must be
further specified with another type before it can be inhabited. The
function-like syntax here is perhaps confusing, getting into the
"mini-language" aspect of type declarations. `LangMap` can be seen as
a *type function* that takes an argument to produce a fully-specified
type. Put simpler, the kind `* -> *` indicates "a type with a
single variable".

What about more than one variable? We need look no further than `Map`.

~~~~
ghci> :k Map
Map :: * -> * -> *
~~~~

Intuitively a `Map` takes two types. The kind `* -> * -> *` indicates it needs
two types to produce an inhabitable type. If we apply just one, we end
up with something like a partially-applied type:

~~~~
ghci> :k Map String
Map String :: * -> *
~~~~

Indeed, we can see our type synonym `LangMap` as a partially-applied `Map`. 

Function Type Synonyms 
======================

We don't have to limit our use of `type` to simple values. We can also write
type synonyms for function types. Consider a family of functions that transform
a String: padding, un-padding, filtering etc:

~~~~Haskell
padString :: Int -> Char -> String -> String
unpadString :: Char -> String -> String
filterString :: Char -> String -> String
~~~~

In the interests of capturing the similarity of these functions, we can
create a "string transformer" type, indicating a function that takes a String
and returns a transformed String:

> type StringTx = String -> String

We can redefine our functions using this type:

~~~~Haskell
padString :: Int -> Char -> StringTx
unpadString :: Char -> StringTx
filterString :: Char -> StringTx
~~~~

But now that we're comfortable with higher-kinded types, let's define
a generic "transformer" type ...

> type Tx a = a -> a

... and fix it to String in our functions.

> padString :: Int -> Char -> Tx String
> padString i c s = replicate i c ++ s
>
> unpadString :: Char -> Tx String
> unpadString c = dropWhile (== c)
>
> filterString :: Char -> Tx String
> filterString c = filter (/= c)

Stare at those types a little longer, as they contain a great
truth about Haskell types: there's usually more to them than meets the eye.
Looking at `filterString`, one might think it's a unary
function, since there's only one `->` in the signature. 

Of course, `type Tx a` is simply hiding the second function arrow, so we can
dismiss any confusion as syntactic tricks. However, it's entirely reasonable and common to define a datatype 
(with `newtype` or `data`) whose contents are a function:

> newtype MyFunction a b = MyFunction { getFun :: a -> b }

Real-world examples of this include the Reader and State monads. 
The number of function arrows in a signature don't always
indicate the true "arity" of the function.

Follow the types
----------------

In Haskell, it behooves you to "follow the types"
until you're confident you know what's going on. If we came across `filterString`
without having seen `Tx String` first, we might be confused by the type. 
Our trusty ":t" command in ghci isn't terribly helpful:

~~~~
ghci> :t filterString
filterString :: Char -> Tx String
ghci> :t Tx String
<interactive>:1:1-2: Not in scope: data constructor ‘Tx’
<interactive>:1:4-9: Not in scope: data constructor ‘String’
~~~~

The ":i" (info) command comes to our rescue. Use ":i" it to
interrogate types, ":t" is only for functions and constructors.

~~~~
ghci> :i Tx String
type Tx a = a -> a
  	-- Defined at ....
type String = [Char] 	-- Defined in ‘GHC.Base’
~~~~

Function Types In ... Function Types
====================================

We can use our `Tx` type in function signatures to indicate clearly
that we need a transformer function.

Consider a function that takes a list of Strings, an index, and a
transformer function, in order to transform the String at the index.
Without `Tx String`, the type would look like this:

> txStringAtVerboseType :: (String -> String) -> Int -> [String] -> [String]

With `Tx String` our intent becomes clearer:

> txStringAt :: Tx String -> Int -> [String] -> [String]

We can go further and say this is a "String list transformer" function:

> txStringAt' :: Tx String -> Int -> Tx [String]

Lastly, we can go full general, and realize we're transforming *any type*
at an index in a list:

> txAt :: Tx a -> Int -> Tx [a]
> txAt t i l | null (drop i l) = l -- leave list unchanged with bad index
>            | i < 0           = l
>            | otherwise       = take i l ++ t (l !! i):drop (i + 1) l 
>

Specific to general
-------------------

The progression of our functions above illustrates a common experience
that often happens when writing Haskell code: first cuts are specific,
then less so, and finally we arrive at the fully-general solution.

The code in `txAt` is fully general for any type in a list, but
without sacrificing any of the functionality we wanted in a
String-specific function. By moving to the general case, we are
confronted with the essence of the problem: indexing and splicing. 
But because of our polymorphism, the types line up beautifully:

> txStringAt = txAt
> txStringAt' = txAt
> txStringAtVerboseType = txAt

Begin Opinionated Rant
----------------------

"Specific to general" might well be seen as what software engineers do
all day long: apply abstraction to go from solving specific to general
problems. Outside of Haskell, however, general solutions can have a high cost, 
either in safety, maintainability, or utility.

In impure, type-impoverished languages, "general" solutions make
assumptions about subtypes, and then suffer from bugs arising when the
assumptions are false, or contracts not honored. In response,
developers attempt to code around the subtypes, which makes for
brittle, un-maintainable code (often seen with big inheritance
hierarchies).  

The other response is to craft one-size-fits-all interfaces which
limit the utility of the code to a minimal subset of functionality,
often adhering to some "pattern" in an attempt to mold requirements to
a "safe" model of interaction.

What emerges is the "architect vs developer" model, where only super-experienced
developers are trusted with abstraction, while in-the-trenches developers
are expected only to fit pieces together. 

My experience in Haskell is that the opposite obtains; solutions
naturally evolve to their full generality, making for astonishingly
reusable code with no reduction in utility or safety. It's all made
possible by having rock-solid type safety, combined with referential
integrity. We gain the confidence to solve the general problem without
fearing our solution will fail on sub-cases. *End rant.*


Conclusion
==========

This article will hopefully get you a little more comfortable with Haskell
types. There's obviously more to it, but the idea was to introduce three
concepts that are unique to Haskell (and it's strict-FP cousins): 

**Functions as types**. It's really important to understand that a
bareword in a type signature can denote a function, not just a simple
value. Armed with this intuition, you can investigate advanced
concepts, like how the State monad can "magically" represent a mutable
variable inside a function that neither accepts the variable as an
argument nor returns it. 

**Type variables**. This simple mechanism powers a lot of the expressiveness
and capability of Haskell. There's more to be covered here, such as 
constraints on variables, but the point is to at least understand their
scope (ie, distinct from the definition code), and the role of multiple
type variables (representing distinct types).

**Kinds**. We didn't go too deep here, intending on a basic introduction with
examples of different-kinded types. The point is, don't be intimidated
by kinds, they simply indicate if a type needs another type or two added
to it to create something you can actually use.
