----------
title: Effectful Haskell: IO, Monads, Functors
author: Stuart Popejoy
date: 2015-05-14
----------

*"Code written in Haskell is guaranteed to have no side effects."...*

*..."because nobody will ever run it?"* [(xkcd)](http://xkcd.com/1312/ "xkcd reference") 

To code any decent-sized application in Haskell, you have to be comfortable
with *effectful* programming. 

Haskell famously offers "pure" functions, and they are great, referentially
transparent and all that. But IO will be done. Assuming you want to 
factor your IO activity into more than just `main`, your app will have a 
lot of functions in IO (or `MonadIO`).

At the same time, you'll want to enrich your pure functions with the core
library modules that "encode" effectful behavior -- such as maintaining 
state over function calls, or reading from a static environment -- into your types.

Finally, you'll want to integrate all of this with fancy "solution" libraries
offering webservers, http clients, loggers, database access, and such. 

This article is the first in a series on "effectful programming". My
informal definition of "effectful" encompasses

1. Actual side-effects (IO)
2. Stuff that seems like side-effects (State, Writer, etc.)
3. Contexts that persist over function calls (Reader, State, etc.)
4. Non-local control flow (Maybe, Either).

In all of these cases, "pure" programming, mapping specified inputs to
outputs, gives way to richer metaphors that offer all the utility of 
imperative or mutable programming, but with the *exact* contract of
execution specified in the type. 

It's the best of all possible worlds. Let's get started.

> {-# LANGUAGE FlexibleContexts #-}
> import System.IO
> import Control.Monad.Reader
> import Data.Set (Set,singleton)
> import Control.Applicative

It all starts with IO
=====================

To write an app, you implement `main :: IO ()`.

> main :: IO ()
> main = putStrLn "Hello World!"

`main` is the gateway drug into `IO`, and the first one's free. "Hello World"
is a one-liner, requiring no imports, and not even a type signature 
if you're really lazy.

Functions with the famous type signature `IO ()` have zero functional mojo.
`()` is the empty type, meaning the function returns nothing: side-effects only.

You can use `IO ()` to squander any benefits of strongly-typed functional
programming, with magical values emerging from the big scary world
outside the compiler.

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
ghci> countCharInFile 'a' "posts/Effectful.lhs"
1482
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

() is not null
--------------

As we noted above, `IO ()` means the function has no output. `()` is the "unit type",
which is uninhabitable by any value: nothing can become `()`, and `()` cannot
be casted or coerced into any other type. 

As such, it is entirely different from `null` in those *other* languages, where
it can inhabit any type, and any type can be null, causing bugs aplenty. 
`()` can never have a value except itself. 

`IO ()` therefore means that we're only executing this action for its side effects.
However, functions like `readFile` are of type `IO String`, meaning "run side-effects
and give me a `String` result." 

In this regard, the two-kinded--ness of `IO` means something
completely different than, say, `Set`. The second type in Set
indicates what values it "holds"; the second type in IO
indicates what single value it "produces".

Effectful IO
------------

IO is obviously an action-packed, effect-having champ. However, it is
also "effectful" in our sense here, since IO implements the
typeclasses `Functor`, `Applicative` and `Monad`. 

These typeclasses form a hierarchy, such that any `Monad` is an
`Applicative`, and any `Applicative` is a `Functor`. ^[The
Functor-Applicative-Monad hierarchy was only a convention until
recently, when GHC 7.10 started enforcing the it]. They form the
"effectful trio" of abstractions.

All of the types we'll use to level-up our code implement this trio.
Effectful programming in Haskell is all about learning how to leverage
the APIs of these types. 

Let's start with the most powerful, `Monad`.

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

Functions whose type is monadic (their result is of type Monad x) are different
than your usual function: instead of returning the monad, they "run inside"
the monadic context, and any values must be "returned" back to the context. 

So to really understand `return`, we have to understand its dual: bind.

bind (>>=)
----------

Bind is the real magic of `Monad`, formalizing how you operate on a monadic value. 

With non-monadic
"container" (two-kinded) types, you'd just grab the underlying value
via an accessor function or whatever and go to town. A monad however
forces you to use bind to get at the good stuff: you have to supply 
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
fantastic abstraction for IO. It's easy to compose and factor all of
the effectful types with `IO`, with a little help from something
called *monad transformers*. 

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
We bind "readFile f", of type `IO String`, with a lambda function.
It's argument `cs` has the contents of the file from `readFile`. In
the lambda we call our pure function `countChar`, and `return` the Int
result back to IO.

With eta reduction, bind makes for some beautiful compositions:

> countCharInFileEta :: Char -> FilePath -> IO Int
> countCharInFileEta c f = readFile f >>= return . countChar c

The infix syntax really shines here, with the `cs` argument composing
onto the end of `countChar c`. Bind here resembles a "pipeline" of the
file contents "flowing" into `return . countChar c`.

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

```
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
"cleans up" repeated `>>=`, `>>` and `let` expressions.

Here's `countCharLog` rewritten with `do`:

> countCharLogDo :: Char -> FilePath -> IO Int
> countCharLogDo c f = do
>   cs <- readFile f
>   let count = countChar c cs
>   putStrLn $ "Counted " ++ show count ++ " chars"
>   return count

Starting from the top, we have "`cs <- readFile f`". `<-` is the ["draw
from"](https://wiki.haskell.org/Keywords#.3C-) operator, which invokes
bind under the hood. The argument of the next lambda appears as an "assignment" on
the left. Subsequent expressions have `cs` in scope, exactly
like they would be in nested lambdas.

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

> countCharReverseBind :: Char -> FilePath -> IO Int
> countCharReverseBind c f = return . countChar c =<< readFile f

This looks more like functional code. However some syntactic gotchas await.

In a pure function, `f` would be a candidate for eta reduction. 
Unfortunately, the infix precedences of `=<<` and `.` don't play
nicely, so point-free style gets a little clumsy.

> countCharRevBindEta :: Char -> FilePath -> IO Int
> countCharRevBindEta c = (return . countChar c =<<) . readFile

"Forward" bind works better with the other infix operators, and maps
directly to `do` sugar. It's more idiomatic use `>>=` (and `do`) than reverse bind.

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
The Functor implementation itself will lift the `b` result for us "outside".

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

Wtih the effectful types, `fmap` is insanely useful. It allows
us to "plug in" a pure operation into an effectful one. 

Bind forces us to
shape effectful operations differently than pure ones. It can therefore be tempting to see pure
transformations as in "a different world", confining them to
`let` clauses or stashing them in other functions altogether. The effectful programmer
instead uses `fmap` with bind, composing pure transformations
with an effectful result.

Our char-counting function is an excellent candidate for plugging in `fmap`.

> countCharInFileFmap :: Char -> FilePath -> IO Int
> countCharInFileFmap c f = fmap (countChar c) (readFile f)

Our code is looking more and more functional -- the only
indication here that our function is at all impure is the type `IO Int`.
Let's "follow the types" to see how `fmap` accomplishes this:

```haskell
ghci> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b

ghci> :t countChar 'c'
countChar 'c' :: String -> Int

ghci> :t readFile "some string"
readFile "path" :: IO String
```

Here, we're using GHCI to examine the types of partially- and
fully-applied functions. `countChar 'c'` creates a unary function of
type `String -> Int`, which is suitable for "plugging into"
`fmap`. Meanwhile, the fully-applied `readFile "some string"` is 
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

> countCharInFileFmap2 :: Char -> FilePath -> IO Int
> countCharInFileFmap2 c f = countChar c <$> readFile f

That's some pretty sweet code right there.

List is a Monad
===============

So far we've focused our Monad/Functor discussion on `IO`, and
referencing other "effectful" types. It's important however to 
realize that nothing about Monad or Functor is necessarily effect-related.
To do this, let's take a quick glance at the humble list type, `[a]`. 

We know already that it's a `Functor`: the `map` operation is
a "Greatest Hit" of functional programming. Intuitively, we can see
how to apply a function to every element of a list, creating a new list 
with the transformed elements. How is it a `Monad` though?

`return` is easy. It simply creates a singleton list
out of the value. 

```haskell
ghci> return 1 :: [Int]
[1]
```

Bind is more interesting. It's type is "`[a] -> (a -> [b]) -> [b]`":
the monadic function will take each value of the list, and return
the transformed result as a list. 

The result is a new list. Remember though that it's now made of a
bunch of *list results*, not individual values like `fmap`. Therefore
the List monad must *concatenate* these results to make the new list.

We can see therefore how `Monad` offers strictly *more powerful
transformations* than `Functor`. `fmap` can only transform an
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

As we can see, the intermediate values `[1]`, `[2,2]` and `[3,3,3]` have been merged
to create the final results.

To make things more interesting, let's look at what it means to bind up
two lists. We'll use `do` notation to avoid unsightly bind lambdas.

> listBindTwo :: [Int] -> [Int] -> [Int]
> listBindTwo is js = do
>    i <- is
>    j <- js
>    return [i,j]

``` haskell
ghci> listBindTwo [1,2,3] [5,6]
[1,5,1,6,2,5,2,6,3,5,3,6]
```

It looks a lot like "list comprehensions", which is no accident. List
comprehensions are indeed the Monadic use case for lists. 

Lists are therefore first-class Monads, which is a great way to unseat
the assumption that Monads are somehow "imperative". In `listBindTwo`,
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


Effectful Haskell: Configuration
================================

Now that we have some idea how to work with Monads and Functors, let's
start looking at how we can integrate some of the nifty effectful types
in the Haskell libraries.

One "effect" we'd like to have is the ability to read values from an
environment that is present over all of our function calls. An obvious
example is accessing a configuration data structure throughout our program. 

Given an example config:

> data AppConfig = AppConfig {
>     logfile :: FilePath
>   , version :: String
>   , maxMessageLength :: Int
> } deriving (Show)


The most obvious way to read this config is to pass it to every
function that needs it. Our examples will be a function in IO
that opens our logfile and writes a preamble:

> initLogFile :: String -> AppConfig -> IO Handle 
> initLogFile preamble config = do
>   handle <- openFile (logfile config) WriteMode
>   hPutStrLn handle (preamble ++ "\nVersion: " ++ version config)
>   return handle

and a pure function that validates message lengths:

> validateMessage :: String -> AppConfig -> Either String ()
> validateMessage msg config = 
>      if (length msg > maxMessageLength config)
>      then Left ("Message too long: " ++ msg)
>      else Right ()

No problem with this, except it's a little ... "manual". We'd prefer
to *formalize* this practice, as functions
"participating in a configured environment". Along the way it'd be
nice to get `config` out of the argument list, since it's not
really germane to the function's specific job.

One way to formalize our approach is with a type synonym.

> type ConfigReader a = AppConfig -> a

> initLogFileTS :: String -> ConfigReader (IO Handle)
> initLogFileTS = initLogFile
> 
> validateMessageTS :: String -> ConfigReader (Either String ())
> validateMessageTS = validateMessage

Our equations prove that the `ConfigReader` type synonym works.  It's
a light formalization that also hides `AppConfig` from our
signatures. But, we still have to deal with a `config` argument to
read any values out, and we have to make sure that argument is the last 
in the list, etc.

To really formalize our approach, we need environment-querying
functions to just "show up" in our function's scope, with the value
magically inserted somewhere outside of the argument list. The type
that implements this magic is called `Reader`.

Well, `ReaderT` actually.

ReaderT and the case of the missing monad
=========================================

A funny thing happened on the way to modern Haskell: some classic
monads disappeared. `Reader` is one such classic. It's type is `(->
r)`: it literally formalizes an "extra argument" into a datatype.

It's also nowhere to be found. All we can find is a type synonym defining 
it in terms of `ReaderT Identity`. Huh?

```
ghci> import Control.Monad.Trans.Reader (Reader)
ghci> :i Reader
type Reader r = ReaderT r Identity
```

Well, it turns out that `Reader` is just a specialization of a more
general type `ReaderT`, a *monad transformer* that can be
composed with other effectful types. In the relatively rare case
that you want to use `Reader` all by itself, the type synonym `Reader`
fits the bill with the trivial `Identity` monad. Otherwise you'll
use it in your effectful monad "stack".

Monad Transformers
------------------

Effectful types are great, but they're even better used together, 
which is the point of this article. Just because you're in IO doesn't
mean you don't want to read from config, right? Monad transformers
allow monads to be "stacked" together, to give you the exact
computational context you're looking for.

To understand this, it's useful to look at the kinds. Monads are of kind
`* -> *`, implying they "contain" another type. A Monad Transformer will
be of kind `(* -> *) -> * -> *`, meaning that in addition to its 
contained type, it also takes *another Monad* -- the `* -> *` in
parentheses.

Now, a monad does not have to just be two-kinded. ReaderT, for example,
needs the type of the environment. So the kind of ReaderT must include
this.

```
ghci> :k ReaderT
ReaderT :: * -> (* -> *) -> * -> *
```

Looking at that kind, we can start to see what our ReaderT "stack" will look
like. We want to work in `IO` and read config of type `AppConfig`. Thus
our type is `ReaderT AppConfig IO`. We've just built a new monad!

```
ghci> :k ReaderT AppConfig IO
ReaderT AppConfig IO :: * -> *
```

OK let's get to work.

Using ReaderT
=============

Here's our logfile initialization function using ReaderT:


> initLogFileRT :: String -> ReaderT AppConfig IO Handle
> initLogFileRT preamble = do
>   f <- reader logfile
>   v <- reader version
>   h <- liftIO $ openFile f WriteMode
>   liftIO $ hPutStrLn h (preamble ++ "\nVersion: " ++ v)
>   return h

Effectful Haskell in the house. The computational context is
expressed as the "return type" of the function; the arguments
are freed up to express the specific domain of the function itself.

To access config, we use `reader` which uses the specified accessor
function on the environment and binds to the value returned. 

```
ghci> :t reader
reader :: MonadReader r m => (r -> a) -> m a
```

ReaderT also has `ask` which returns the entire `AppConfig` value.

liftIO 
------ 

The other change to our code is we had to sprinkle `liftIO` in front
of our IO functions. This is how the non-transforming `IO` monad
can work in a stack: its functions must be "lifted" into scope.

`liftIO` is in the typeclass `MonadIO`, which any monad transformer
must implement in order to work with `IO`. Thus, here, `liftIO`
is implemented by `ReaderT` to "lift" the monadic value coming 
out of IO into the ReaderT context. 

Indeed, this is the nitty-gritty of transformers. In order for 
two monads to "stack together", the "outer" monad must be able
to "lift" all of the operations of the "inner" monad into its
context. To do so, the "outer" monad must implement some
typeclass that expresses the API of the inner monad, and provide
an implementation that "lifts" those operations accordingly.

For monad authors, this means a lot of boilerplate. For us monad
*users*, it's no problem at all. What's more, monad authors
usally maintain a corresponding typeclass that expresses all of the
monad's core functionality, which is extremely useful.

Transformer Typeclasses
=======================

We mentioned `MonadIO` above, which is the typeclass expressing "a
monad that can interoperate with IO". `liftIO` is its sole function,
to be used with any IO value to lift it into the transformer context.

Such typeclasses exist for all the standard monad/monad transformers.
`StateT` has `MonadState`, `ErrorT` has `MonadError`, and so on. Unlike
`MonadIO`, these classes expose the core operations of the monad as
functions, so you can directly use these functions in a transformer stack. 

For ReaderT, the typeclass is `MonadReader`. 

```
ghci> :i MonadReader
class Monad m => MonadReader r (m :: * -> *) | m -> r where
  ask :: m r
  local :: (r -> r) -> m a -> m a
  reader :: (r -> a) -> m a
```

We described `ask` and `reader` above as the magical API functions
that read from our environment. `local` is less-used, allowing a 
scoped modification of the environment.

There's a big bonus to having these typeclasses around, especially
as our monad stacks get bigger. The typeclasses allow us to "select"
which functionality we want our functions to participate in. 

How is this useful? Consider our pure validation function above.
If we adapt it using our concrete monad stack, it's type becomes
`ReaderT AppConfig IO ()`:

> validateMessageRTIO :: String -> ReaderT AppConfig IO (Either String ())
> validateMessageRTIO msg = test <$> reader maxMessageLength where
>     test l | length msg > l = Left ("Message too long: " ++ msg)
>            | otherwise      = Right ()

(Recall that `<$>` is infix `fmap`. We're mapping in our pure validation
function `test` into the monadic read of `maxMessageLength`.)

The big problem here? So much for our "pure" function! It's now tied
to big bad `IO`, the impurest-est of all. Fortunately, we can use
typeclass constraints to get IO out of the picture:

> validateMessageRTM :: (Functor m, Monad m) => 
>                       String -> ReaderT AppConfig m (Either String ())
> validateMessageRTM msg = test <$> reader maxMessageLength where
>     test l | length msg > l = Left ("Message too long: " ++ msg)
>            | otherwise      = Right ()

(Remember how I said `Functor` will be a required superclass of `Monad` 
in GHC 7.10? I haven't upgraded yet, so I have to put `Functor m` in 
my constraint! The future is bright.)

This is better: `IO` is gone, we just need our stack to operate on
some Monad/Functor and we've got Reader goodness. 

But we're not done yet: this will only work if `ReaderT` is the
"outermost" monad on the stack. In other words, we're still concretely
specifying `ReaderT` itself, which will get us into trouble if
we don't stack our monads just so.

Not only that, `ReaderT` isn't the only Reader. There's also `RWST`, a
convenience monad transformer that bundles `Reader`, `Writer` and
`State` into a single monad. It's really not that hard to stack these
together, but whatever: RWST is a thing, and it's not `ReaderT`: so it
won't work with `validateMessageRTM`.

However, any stack with `ReaderT` is a stack that implements `MonadReader`.
`RWST` implements `MonadReader`. The point is, the polymorphism offered
by the transformer typeclass `MonadReader` gives us most flexibility.
It's really the most *specific*: in fact we could care less what monad
is calling us, as long as it's `MonadReader AppConfig`. So we constrain
to that.

> validateMessageMR :: (Functor m, MonadReader AppConfig m) => 
>                      String -> m (Either String ())
> validateMessageMR msg = test <$> reader maxMessageLength where
>     test l | length msg > l = Left ("Message too long: " ++ msg)
>            | otherwise      = Right ()

Putting it all together
=======================

TODO this section is NOT ready

First, we'll define a contrived function to read in our values as a
tuple. Don't try this at home; using Aeson to read values as JSON, or almost
anything else, is better than this hack.

> readConfig :: FilePath -> IO AppConfig
> readConfig f = fmap (fromTup . read) (readFile f) 
>     where fromTup (a,b,c) = AppConfig a b c 

Now we can create a file with the following contents:

```
("/tmp/logfile","1.0.0",141)
```

and `readConfig` will turn it into an `AppConfig`. Note the use of `fmap`
to apply pure functions to the result of `readFile f`. The function is composed
from `read`, which converts a String into some type, and our `fromTup` function,
which takes a tuple and creates an `AppConfig`. Type inference figures
out the tuple type from the `AppConfig` constructor, which is fed into `read`.
Typesafe hackery FTW.
