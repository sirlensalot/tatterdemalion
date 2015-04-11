% Types & Kinds
% Stuart Popejoy
% March 2015
 
This article seeks to give a whirlwind tour of basic type signatures
in Haskell. Types are pretty important in Haskell so it's good to
make sure you understand the basics.

This is another "literate haskell" article, meaning that the examples
are executable code, and that I have to put imports at the top. 
So here goes:

> import Data.Set (Set, fromList)
> import Data.Map (Map, insertWith, empty, split, elems)
> import qualified Data.Map as M

Simple Types
============

Types such as `String` and `Int` are straightforward to specify and use. 

> duplicate :: Int -> String -> String
> duplicate count s = concat (replicate count s)

Creating simple datatypes is easy.

> data Person = Person { name :: String, age :: Int } deriving Show
> --   ^        ^        ^               ^                
> --   typename ctor     field           field

The `data` keyword declares a datatype `Person`. It has a single
constructor, also called `Person`, which specifies two fields `name`
and `age`. Our new type `Person` is as easy as `String` or `Int` to
use in type signatures.

> isTeen :: Person -> Bool
> isTeen (Person _ a) = a > 10 && a < 20

`isTeen` takes a `Person` and returns a `Bool`.  To do so, it
"deconstructs" the argument to access the age as `a`. The first field,
`name`, is not needed, so the underscore wildcard is used. This syntax
is called "pattern matching", a name that makes more sense when a type
has more than one constructor. More on that later.

With these simple types, the type name alone fully specifies
the type. Once we issue `String`, `Int` or `Person` in a type signature,
we can supply code in which these types will be *inhabited* with values.

Type synonyms
=============

`String` is not what it seems, however. It's neither a built-in type like 
`Int` or a simply-constructed type like `Person`. GHCI can tell us the
real story with the `:i` (short for "info") command:

~~~~
ghci> :i String
type String = [Char] 	-- Defined in ‘GHC.Base’
~~~~

The `type` keyword introduces a **type synonym**, superficially like
`typedef` in C/C++.  `String` is in fact a list of `Char` values!
`String` is more comforting to look at than `[Char]` I suppose. Don't
tell C programmers, though.

Since it's just a synonym, anywhere we see `[Char]` we can use String, and
vice-versa.

> sue :: String
> sue = ['S','u','e']
>
> dave :: [Char]
> dave = "Dave"
>
> dave25 :: Person
> dave25 = Person dave 25
>
> sue30 :: Person
> sue30 = Person sue 30

List type and patterns
----------------------

The list type has special syntax in type declarations. The braces enclose
the type of the list, so you can have `[Int]`, `[Person]`, etc. 
List also has special support in pattern-matches:

> startsWith :: Char -> String -> Bool
> startsWith _ []     = False
> startsWith c (s:_)  = c == s
> startsWith _ _      = False

Pattern-matching captures information about how the value is
constructed. Remember, in an immutable language, once a value is 
constructed, it never changes, so this is a valid test. Here, `[]` means "the empty list",
`(s:_)` matches on the (head:tail) construction of this type, which is
a singly-linked list in the LISP model. The underscores serve to wildcard
match on any constructor, and to avoid creating unused variable bindings.
Pattern-matching works downward from the first match, so the last match 
of `_ _` matches anything that wasn't captured above.


Less-simple types
=================

The list type is more complex than `Person` or `Int`: it's a
type that must have *another type* specified to be useable. A similar
type is `Set`, although it doesn't have special syntax:

> setOfNames :: Set String
> setOfNames = fromList ["Dave","Sue"]

Like list, `Set` needs another type, here `String`, to be a valid declaration.

It's worth pointing out here that in this regard, Haskell is **much**
more strongly-typed than most other languages. There is no "Array" or
"List" container types that can hold disparate types of values;
there's no nulls, and no casting. A list, `Set` or any similar
container *must* have a fully-specified type saying what it holds, before
you can write code to the specification. The container is guaranteed
to only hold values of that type.

Here's list and Set together in a function signature:

> ages :: [Person] -> Set Int
> ages ps = fromList $ map age ps

This function maps over the `age` field of each `Person` in `ps`, and
then builds a `Set` with `fromList`. We can write this perhaps more
clearly in point-free style:

> agesPointFree :: [Person] -> Set Int
> agesPointFree = fromList . map age


Types needing even more types
=============================

We're not limited to types that take one extra type. We can have
as many as we want. A good example of one taking two extra types is `Map`.


> indexPersonsByName :: [Person] -> Map String [Person]
> indexPersonsByName = foldl ins empty 
>     where ins m p = insertWith (++) (name p) [p] m

This builds a `Map` indexing a name to all persons with that name.
We use `foldl`, a *left-fold*, which is a "reduce" operation 
to make a single value (the `Map`) out of a list of values (the Person values).
`empty` is the initial value, to which we insert values using `insertWith`;
this function `(++)` is used to resolve key collisions by operating on the old
and new value. Here we're inserting a singleton list `[p]`, so `++` simply concatenates
the lists together.

We can make this code easier to read with type synonyms:

> type Name = String
> type Age = Int

Since synonyms are nothing but sugar, we don't have to re-define `Person`
to use them. We can use type synonyms for more complex types, too.

> type PersonsByAge = Map Age [Person]
> type PersonsByName = Map Name [Person]

Now we have some nice, literate type signatures:

> getPersonsOver :: Age -> PersonsByAge -> [Person]
> getPersonsOver a = concat . elems . snd . split a

Polymorphism: Type Variables
============================

Examine our two `Map` type synonyms above, and you'll see a similarity:
both have `[Person]` as their value-type. If we want to generalize
on this fact, we'll need some new tools. Enter *type variables*, variables
that only appear in the type declaration code.

> getAllPersons :: Map a [Person] -> [Person]
> getAllPersons = concat . elems

Our polymorphic function `getAllPersons` can now be used with a
`PersonsByAge` value, or `PersonsByName`, or any `Map` with a list of
persons as the value type.

> getAllPersonsByAgeAndName :: PersonsByAge -> PersonsByName -> [Person]
> getAllPersonsByAgeAndName pa pn = getAllPersons pa ++ getAllPersons pn

By using the type variable `a`, we're indicating that the type can change
from call to call, and is unknown to the code in this function.
The lower-case of `a` is how
Haskell recognizes a type variable; anything starting with upper case
(ie, `Map`, `Person`) must be a specific type.

Interestingly, type variables can even be used in type synonyms.

> type PersonsMap a = Map a [Person]

We've moved beyond C-style typedefs now: `PersonsMap` is a *polymorphic
type synonym*, where the type variable declared on the left-hand
side will be supplied to the type declaration on the right.

> countPersons :: PersonsMap a -> PersonsMap Int
> countPersons = M.fromList . map txform . M.toList 
>    where txform (_,ps) = (length ps,ps)

This function converts a `PersonMap` of unknown key type to a map where
the keys are the lengths of the contained lists. We use `toList` and `fromList`
to convert the `Map` to a list of tuples, which we transform in `txform`.


Tuples are another type in Haskell with special syntax, as the signature 
of `toList` shows:
```
ghci> :t M.toList
M.toList :: Map k a -> [(k, a)]
```

Tuples don't have to just have two elements, they can have as many as you
like. However the only way to get at elements in the tuple is to pattern
match:

> type FirstNameMiddleInitialLastName = (String,Char,String)
>
> getMiddleName :: FirstNameMiddleInitialLastName -> Char
> getMiddleName (_,c,_) = c

"Two-tuples" enjoy having the functions `fst` and `snd` ("first" and
"second") to get the respective values. But these cannot interoperate
with 3- or greater tuples, since they are fundamentally different
types.

Function types
==============

So far, our discussion of types has focused on symbols appearing
between the `->` arrows of function declarations. This is familiar, as
most languages leave the argument and return-type specification
outside of the type system. Not so in Haskell; *all of our function
declarations are themselves types.*

For example, our function above, `isTeen`, is a unary function
returning a boolean: `Person -> Bool`. The type of `isTeen` is the
entire declaration `Person -> Bool`, not just the word-like things.  

To illustrate, we can make a type synonym for `isTeen`'s type,
with a reasonably obvious name:

> type PersonPredicate = Person -> Bool

Like any type synonym, we can use this where we'd use `Person -> Bool`.

> isNamedDave :: PersonPredicate
> isNamedDave p = name p == "Dave"

We can also use this  this in declarations with
more arguments, like any other type:

> hasName :: String -> PersonPredicate
> hasName n p = (name p) == n
>
> findPerson :: PersonPredicate -> [Person] -> Maybe Person
> findPerson pp ps = f (filter pp ps)
>       where f []    = Nothing
>             f (a:_) = Just a

A small detail here is a type synonym built with function 
arrows specifies a *function*, not just arguments. To write 
`findPerson` without `PersonPredicate`, we'd need parentheses
to make it clear that the first argument is a function:

> findPersonAgain :: (Person -> Bool) -> [Person] -> Maybe Person
> findPersonAgain = findPerson

Finally, we can use a type variable in a function type synonym, too.

> type Predicate a = a -> Bool

Now instead of the limited `PersonPredicate`, we can simply 
use `Predicate` on any type. However, `a -> Bool` is 
shorter that "Predicate" so maybe we'll just stick with that.

Kinds
=====

We'll finish this introduction to type signatures with a quick discussion
of "kinds" in Haskell. We've talked about types that don't need any other 
types, like `Int`; types that need one other type, like `Set` or list; and
types that need two, like `Map` or the unary function type `(->)`. 

In Haskell, this cardinality is expressed via "kinds", which have a funny
function-like syntax. We can find out the kind of a type with the ":k" command
in GHCI:

```
ghci> :k Int
Int :: *
ghci> :k Person
Person :: *
```

One-star kinds are the simple, self-sufficient types.

```
ghci> :k Set
Set :: * -> *
```

`Set` is of kind `* -> *`, which you can see as it being a "type function"
that needs a value to evaluate to an inhabitable type. Once `Set` gets
it's type, it's a simple one-star kind again:

```
ghci> :k Set Int
Set Int :: *
```

You can probably guess now what Map looks like:

```
ghci> :k Map
Map :: * -> * -> *
ghci> :k Map Int
Map Int :: * -> *
ghci> :k Map Int String
Map Int String :: *
```

And remember, functions are types too, so the function arrow itself
has a kind:

```
> :k (->)
(->) :: * -> * -> *
```

As you go forward with Haskell, pay close attention to the type 
signatures, and remember that a type can have every other sort
of type baked into it: functions, data values, data values that
contain functions, lists, tuples, synonyms for all of the above.
Use GHCI's ":i", ":t", and ":k" commands obsessively, use the source
code, and "follow the types"!
