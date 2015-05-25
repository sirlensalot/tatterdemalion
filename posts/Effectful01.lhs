----------
title: Effectful Haskell: IO, Monads, Functors
author: Stuart Popejoy
date: 2015-05-24
----------

To code any decent-sized application in Haskell, you have to be comfortable
with *effectful* programming. 

Haskell famously offers "pure" functions, and they are great, referentially
transparent and all that. But IO will be done. Assuming you want to 
factor your IO activity into more than just `main`, your app will have a 
lot of functions in IO (or `MonadIO`).

At the same time, you'll want to enrich your pure functions with core
library modules that "encode" effectful behavior into your types, like
maintaining state, or reading from a static environment. Finally,
you'll want to integrate all of this with fancy solutions like
webservers, http clients, loggers, database access, and such.

This article is the first in a series on "effectful programming". My
informal definition of "effectful" encompasses

1. Actual side-effects (IO)
2. Stuff that seems like side-effects (State, Writer, etc.)
3. Contexts that persist over function calls (Reader, State, etc.)
4. Non-local control flow (Maybe, Either).

In all of these cases, "pure" programming, mapping specified inputs to
outputs, gives way to richer metaphors that offer all the utility of 
imperative or mutable programming, but with the *exact contract of
execution* specified in the type. 

It's the best of all possible worlds. Let's get started.

> import Data.Set (Set,singleton)
> import Control.Applicative ((<$>))

It all starts with IO
=====================

To write an app, you implement `main :: IO ()`.

> main :: IO ()
> main = putStrLn "Hello World!"

`main` is the gateway drug into `IO`, and the first one's free. "Hello World"
is a one-liner, requiring no imports, and not even a type signature 
if you're really lazy.

Functions with the famous type signature `IO ()` have zero functional
mojo; we run them for side-effects only. You can use `IO ()` to
squander any benefits of strongly-typed functional programming, with
magical values emerging from the big scary world outside the compiler.

> sendSecret :: IO ()
> sendSecret = writeFile "/tmp/secret" "Who is Benjamin Disraeli?"
>
> andTheAnswerIs :: IO String
> andTheAnswerIs = readFile "/tmp/secret" 

Fortunately, IO isn't an all-or-nothing proposition. 

First and most obviously, you can use pure code in IO whenever you
like. This will arise naturally if your factoring is halfway decent.

> -- | pure function to count characters in a string
> countChar :: Char -> String -> Int
> countChar c = length . filter (==c)
>
> -- | use 'countChar' in IO on a file
> countCharInFile :: Char -> FilePath -> IO Int
> countCharInFile c f = do
>   contents <- readFile f
>   return (countChar c contents)
>

`countChar` is 100% pure, and easy to test in GHCI.

```haskell
ghci> countChar 'a' "aba"
2
```

However, GHCI makes it easy to test IO functions too. Let's see
how many times 'a' shows up in this article:

```haskell
ghci> countCharInFile 'a' "posts/Effectful01.lhs"
1106
```

So what is this strange thing we call `IO`?

IO
==

IO in haskell is famously a Monad, but first and foremost, it's just
another *type*. It's kind is `* -> *`. ^[See [this article](2015-04-10-Types.html) for an intro to kinds.]

```haskell
ghci> :k IO
IO :: * -> *
```

Thus, just like you'd write `Set String` to make Set inhabitable
with String values, you write `IO Handle` to indicate an IO action
that produces a Handle. 

If you want an `IO` action that produces a Set of Strings, you'll
need to break out parentheses:

> setProducingAction :: IO (Set String)
> setProducingAction = return $ singleton "contrived"


As we noted above, `IO ()` means the function has no meaningful
output. `()` is the "unit type", with the single inhabitant `()`. It allows
us to return a value from a function (and thus still be "functional",
as opposed to `void` functions in other languages), but indicate that
the return value is of no meaning.

`IO ()` indicates we only execute this action for its side effects.
However, functions like `readFile` are of type `IO String`, meaning "run side-effects
and give me a `String` result." 

In this regard, the two-kinded--ness of `IO` means something
completely different than, say, `Set`. The second type in Set
indicates what values it "holds"; the second type in IO
indicates what single value it "produces".

Effectful IO
------------

IO is obviously an action-packed, effect-having champ. However, it is
also "effectful" in a larger sense, since IO implements the
typeclasses `Functor`, `Applicative` and `Monad`. These form a hierarchy, such that any `Monad` is an
`Applicative`, and any `Applicative` is a `Functor`. ^[The
Functor-Applicative-Monad hierarchy was only a convention until
recently, when GHC 7.10 started enforcing it.]. 

These are the
"effectful trio" of abstractions: all of the types we'll use to level-up our code implement them.
Effectful programming in Haskell is all about learning how to leverage
the APIs of these types. Let's start with the most powerful, `Monad`.

Monad: return and bind
======================

Anything that is a `Monad` will support certain operations. Let's ask GHCI what
they are:

```haskell
ghci> :i Monad
class Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  fail :: String -> m a
```

We see four functions making up the typeclass. We also see that any Monad
must be of kind `* -> *`, like IO. This is true of `Applicative` and `Functor`
as well.

The two most important functions in Monad are `return` and `(>>=)`; the second
is an infix operator called "bind". Let's look at each.

return
------

In isolation, `return` is disarmingly straightforward, albeit with a strange
name. 

```haskell
ghci> :t return
return :: Monad m => a -> m a
```
It simply allows you to stick a value into the monadic "context". Take an `a`,
you get `Monad m => m a`.

> returnIsEasy :: String -> IO String
> returnIsEasy s = return (s ++ " is in IOooooooo")

Just like that, we wrapped a String in `IO`. We're coding with the pros.

Note that `return` is unnecessary if you're calling a function that is 
already in your monad:

> ioNoReturn :: IO String
> ioNoReturn = returnIsEasy "Elvis"

Pretty minimal. What's the point? The clue is in the name `return`.

Functions whose type is monadic (their result is of type `Monad x`) are different
than your usual function: instead of returning the monad, they "run inside"
the monadic context, and any values must be "returned" back to the context. 

So to really understand `return`, we have to understand its partner in crime, bind.

bind (>>=)
----------

Bind is the real magic of `Monad`, formalizing how you operate on a monadic value. 

In monadic operation, we don't simply grab the underlying value and go to town.
Instead, we use "bind" to get at the good stuff: you have to supply 
a *function* to operate on the value and "give it back" to the monadic
context. The type makes this clear:

```haskell
ghci> :t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

The first argument to `>>=` is the monad itself, `m a`. The second argument
is the monadic function, which takes the bare value `a` and gets to work,
producing the transformed value `b` in the monadic context: `(a -> m b)`.

This inversion of control allows the Monad instance to enforce all kinds
of invariants on what that computation is allowed to do. You don't simply 
"call" a monadic function, you "bind" to it. You don't simply return a value
from a function, you "return" it back to the context, using the API provided
by the monad. The monad's code is in charge.

It's a mode of computation of staggering generality and
utility. Monadic functions working under `bind` have a 
different "shape" than normal input-to-output functions. This
makes radically different modes of computation available to pure code.

Monads and IO
-------------

Indeed, `IO` is a funny instance in the effectful zoo. When we use
types like `State` and `Reader` and `Maybe` we reap huge gains in
elegance and power; what's more, the code is right there on Hackage
for us to study and understand. 

IO, however, is mysterious. Its guts are buried deep inside GHC. We
use it as a monad "just because": a program without IO isn't much of a
program.

Instead, it's the *compiler* getting the main utility from monadic
control. Through `Monad`, the compiler can ensure that all our `IO`
commands happen in strict sequence, helping it safely move values in
and out of the pure runtime.

The good news is that Monad (with its effectful cousins) is a
fantastic abstraction for `IO`, making it easy to compose and factor other
effectful types with it.  

Binded by the light
===================

Working with bind isn't hard once you get used to it. 

The bind operator, `>>=`, is used infix, with a monad inhabitant on
the left side, and our consuming function on the right. This creates a
left-to-right code "motion":

> countCharInFileBind :: Char -> FilePath -> IO Int
> countCharInFileBind c f = 
>   readFile f >>= \cs -> return (countChar c cs)

Our function counts how often a character shows up in a text file. 
We bind "`readFile f`", of type `IO String`, with a lambda function.
Its argument `cs` has the contents of the file from `readFile`. In
the lambda, we call our pure function `countChar`, and `return` the Int
result back to IO.

With eta reduction, bind makes for some beautiful compositions:

> countCharInFileEta :: Char -> FilePath -> IO Int
> countCharInFileEta c f = readFile f >>= return . countChar c

The infix syntax really shines here, with the `cs` argument composing
onto the end of `countChar c`. It's as though `>>=` were a "pipeline,"
flowing the contents of the file into our composition `return . countChar c`.

Side-effects only: >>
---------------------

When we use functions of type `IO ()`, we're only interested in the side
effects. Nonetheless, we still have to bind to use them, leaving us with
an unused argument of type `()`. 

An example is `putStrLn`, which writes a line of text to stdout. Let's use
it to log a message before performing our computation:

> countCharMsgBind :: Char -> FilePath -> IO Int
> countCharMsgBind c f = 
>    putStrLn ("count " ++ [c] ++ " in " ++ f) 
>                 >>= \_ -> readFile f    -- yuck
>                 >>= return . countChar c

The "`\_ ->`" is pointless. Enter `>>`, a modified bind
that swallows the useless argument. 

```haskell
ghci> :t (>>)
(>>) :: Monad m => m a -> m b -> m b
```

With `>>`, we simply "sequence" the next action.


> countCharMsg :: Char -> FilePath -> IO Int
> countCharMsg c f = 
>    putStrLn ("count " ++ [c] ++ " in " ++ f) 
>                 >> readFile f           -- much better!
>                 >>= return . countChar c

Local definitions
-----------------

`where` and `let ... in` can be interchanged in pure code as a
matter of style. In monadic code however, it gets tricky to
use `where`.

Let's change our logging to output the character count. To do so
we'll need to capture it in a variable first, so we fire up a 
`where` clause. Unfortunately, the compiler yells at us.

```haskell
countCharBroken c f = 
   readFile f >>= 
      \cs -> putStrLn ("Counted " ++ show count ++ " chars")
             >> return count
   where count = countChar c cs 

posts/Effectful.lhs:332:32-33: Not in scope: ‘cs’ …
Compilation failed.
```

As the error shows, the lambda after `readFile f` is not in scope for
the `where` section. Only `c` and `f` from the top level are in
scope. There's not really any good place to put the `where`.

`let` and `in` work better, "above" the lambdas that need it.

> countCharLog :: Char -> FilePath -> IO Int
> countCharLog c f = 
>    readFile f >>= 
>       \cs -> let count = countChar c cs 
>              in putStrLn ("Counted " ++ show count ++ " chars") 
>                 >> return count




Do notation
===========

If you've got a lot of work to do, writing a ton of lambdas
can get unweildy and confusing. Our simple function `countCharLog`
has three lambdas; we can easily imagine more.

Enter `do` notation, an alternate syntax for Monads which nicely
"cleans up" repeated `>>=`, `>>` and `let` expressions. Here's `countCharLog` rewritten with `do`:

> countCharLogDo :: Char -> FilePath -> IO Int
> countCharLogDo c f = do
>   cs <- readFile f
>   let count = countChar c cs
>   putStrLn $ "Counted " ++ show count ++ " chars"
>   return count

Starting from the top, we have "`cs <- readFile f`". "`<-`" is the ["draw
from"](https://wiki.haskell.org/Keywords#.3C-) operator, which invokes
bind under the hood. The argument of the next lambda appears as an "assignment" on
the left of the arrow. The following expressions have `cs` in scope, exactly
like nested lambdas.

Next we define `count` with `let`. No `in` is required: definitions are automatically in scope for subsequent expressions.

Our side-effect--only expression comes next. We simply 
issue our `putStrLn` call and proceed to the next line.
Under the hood, this is sequenced to the following expression with `>>`.

If you stare long enough at `countCharLog` and `countCharLogDo`, you'll see
how `do` notation is simply a reformat of our bind lambdas, leveraging
newlines to clean up the code.

Just do it
----------

Do notation has been characterized as a way to "write imperative code
in Haskell", ostensibly "easier to read" than, say, the right-to-left
sequencing of function composition. This perhaps explains some of the
general confusion surrounding Monads, when they are seen as
"programmable semicolons" or some other broken metaphor about
imperative coding.

It is far better to see `do` as simply a code-cleaning
tool. It's a way of chaining binds of monadic functions and lambdas,
with the left-to-right, top-to-bottom orientation accumulating
variables in scope. It's a *style*, embedded in syntax. 

Monads don't have to go left-to-right.  "Reverse bind" (`=<<`) can be
used to write monadic code that flows the other way.

> countCharRevBind :: Char -> FilePath -> IO Int
> countCharRevBind c f = return . countChar c =<< readFile f

This looks more like functional code. However some syntactic gotchas await.

In a pure function, `f` would be a candidate for eta reduction. 
Unfortunately, the infix precedences of `=<<` and `.` don't play
nicely, so point-free style gets a little clumsy.

> countCharRevEta :: Char -> FilePath -> IO Int
> countCharRevEta c = (return . countChar c =<<) . readFile

"Forward" bind works better with the other infix operators, and maps
directly to `do` sugar. Thus, it's easier and more idiomatic to use `>>=` (and `do`) instead of reverse bind.

Like all sugar, `do` notation gets addictive. Until you're really
comfortable with monads, you should regularly "de-sugar" `do` blocks
back into `>>=`, `>>`, `let` and lambdas. This will reveal the monadic
functionality explicitly.

IO is a Functor 
===============

Since `IO` is a Monad, it's also a `Functor`, which is horribly useful.

`Functor`'s main function is `fmap`. It looks a lot like Monad's reverse bind.

```haskell
ghci> :t fmap
fmap :: Functor f => (a -> b)   -> f a -> f b
ghci> :t (=<<)
(=<<) :: Monad m  => (a -> m b) -> m a -> m b
```

Unlike bind, the function used in `fmap` is "pure": it makes the
transformation from `a` to `b` without needing to `return` it to the context.
The Functor implementation itself will lift the `b` result for us "outside" the function.

`fmap` is really the classic functional operation `map`, best known
as a way to transform all of the elements in a list. 

> incrementAllBy :: Int -> [Int] -> [Int]
> incrementAllBy i is = fmap (+ i) is 

```haskell
ghci> incrementAllBy 2 [1,2,3]
[3,4,5]
```

Many tourists to functional programming learn `map` (and maybe its
cousin `reduce`/`fold`) as a list operation only, before proudly stamping FP
onto their resume. Haskellers go a lot further with `Functor`, mapping
functions over *any* type with the `Functor` shape.

Effectful Functors
------------------

Wtih the effectful types, `fmap` allows
us to "plug" a pure operation into an effectful one. 

Bind forces us to
shape effectful operations differently than pure ones. It can therefore be tempting to see pure
transformations as in "a different world", confining them to
`let` clauses or stashing them in other functions altogether. The effectful programmer
instead uses `fmap` on monadic values, composing pure transformations
with an effectful result.

Our char-counting function is an excellent candidate for plugging in `fmap`.

> countCharFmap :: Char -> FilePath -> IO Int
> countCharFmap c f = fmap (countChar c) (readFile f)

Our code is looking more and more functional -- the only
indication here that our function is at all impure is the type `IO Int`.
Let's "follow the types" to see how `fmap` accomplishes this:

```haskell
ghci> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b

ghci> :t countChar 'c'
countChar 'c' :: String -> Int

ghci> :t readFile "path"
readFile "path" :: IO String
```

Here, we're using GHCI to examine the types of partially- and
fully-applied functions. `countChar 'c'` creates a unary function of
type `String -> Int`, which is suitable for "plugging into"
`fmap`. Meanwhile, the fully-applied `readFile "path"` is 
of type `IO String` -- and IO is a `Functor`.

Putting the types together, we see the resuling type is `IO Int`.

```haskell

ghci> :t fmap (countChar 'c') (readFile "path")
fmap (countChar 'c') (readFile "path") :: IO Int

```

In short, we use `fmap` to convert our effectful `String` result into an `Int`.
The pure conversion happens *within the effectful context:* no `return` required.

`fmap` has an infix synonym `<$>`, which we can use to make those
pesky parentheses disappear.

> countCharInfix :: Char -> FilePath -> IO Int
> countCharInfix c f = countChar c <$> readFile f

That's some pretty sweet code right there.

List is a Monad
===============

So far we've focused our Monad/Functor discussion on `IO` while
referencing other "effectful" types. It's important to 
realize that nothing about Monad or Functor is necessarily effect-related.
To do this, let's take a quick glance at the humble list type, `[a]`. 

We know already that it's a `Functor`: the `map` operation is
a "Greatest Hit" of functional programming. Intuitively, we can imagine applying a function to every element of a list, creating a new list 
with the transformed elements. How is it a `Monad` though?

`return` is easy. It simply creates a singleton list
out of the value. 

```haskell
ghci> return 1 :: [Int]
[1]
```

Bind is more interesting. It's type is "`[a] -> (a -> [b]) -> [b]`":
the monadic function will take each value of the list, and return
the transformed result *as a list*. 

The result is a new list, but made from a
bunch of *list results*, not individual values like `fmap`. Therefore
the List monad must *concatenate* these results to make the new list.

We can see therefore how `Monad` offers strictly more powerful
transformations than `Functor`. `fmap` can only transform an
individual value "in place", while `>>=` can return an empty list, or a list
with more elements than the original. Functor is "structure preserving",
while Monad is "structure transforming".

Here's a simple example, using bind to repeat `Int` values as many times
as the value itself.

> listBindEx :: [Int] -> [Int]
> listBindEx is = is >>= (\i -> replicate i i)

```haskell
ghci> listBindEx [1,2,3]
[1,2,2,3,3,3]
```

The intermediate values `[1]`, `[2,2]` and `[3,3,3]` have been merged
to create the final results. 

To make things more interesting, let's look at what it means to bind up
two lists. We'll use `do` notation to avoid unsightly bind lambdas.

> listBindTwo :: [Int] -> [Int] -> [Int]
> listBindTwo is js = do
>    i <- is
>    j <- js
>    [i,j]

``` haskell
ghci> listBindTwo [1,2,3] [5,6]
[1,5,1,6,2,5,2,6,3,5,3,6]
```

It looks a lot like "list comprehensions", which is no accident. List
comprehensions are indeed the Monadic use case for lists. Lists are therefore first-class Monads, which is a great way to unseat
the assumption that Monads are somehow "imperative". 

In `listBindTwo`,
it is impossible to interpret `i <- is` happening "before" `j <- js`.
They are simply two nested lambdas that produce the cartesian result
of the two lists. How we *declare* functions is not necessarily related
to how they *fire*.

Conclusion
==========

We've covered a lot of ground, but we're just getting started. 

The next article will turn to the pure effectful types, and introduce
`Applicative` use cases. We'll look at monad transformers, plus the
typeclasses surrounding them that make working with transformers so
much easier.

Finally we'll sew them all together in a fully-working, if trivial, example.
Stay tuned!

