% Effectful Haskell
% Stuart Popejoy
% April 2015

*"Code written in Haskell is guaranteed to have no side effects."...*

*..."because nobody will ever run it?"* [(xkcd)](http://xkcd.com/1312/ "xkcd reference") 

To code any decent-sized application in Haskell -- something that
reads config, or builds up some state, in addition to whatever
missle-firing IO that you throw down -- you have to be comfortable
with *effectful* programming. 

Haskell famously offers "pure" functions, and they are great, referentially
transparent and all that. But IO will be done. Assuming you want to 
factor your IO activity into more than just `main`, your app will have a 
lot of functions in IO (or `MonadIO`).

At the same time, you'll want to enhance your pure functions with core
types that "encode" effectful behavior, such as Reader, Writer and
State. Finally you'll want to integrate all of this will fancy
libraries offering webservers, http clients, loggers, database access,
and such.

This article will offer a breadcrumb trail to follow in pursuit of
"effectful" programming. To me, "effectful" captures the notions of

1. Actual side-effects (IO)
2. Stuff that seems like side-effects (e.g. State, Writer)
3. A "computational environment" that persists through various function calls (e.g. Reader)
4. Non-local control flow (Maybe, Either). 

This means using types that implement the `Functor` ==> `Applicative`
==> `Monad` typeclass hierarchy, and managing the complexity that can
arise when you use all of these things together.

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

`main` is the gateway drug into `IO`, and the first one's free: "Hello World"
is a one-liner, requiring no imports, and not even a type signature 
if you're really lazy.

Functions with the famous type signature `IO ()` have zero functional mojo.
`()` is the empty type, meaning the function returns nothing:
you run it for side-effects only. You can use `IO` to
squander any benefits of strongly-typed functional programming, if you want.

> sendSecret :: IO ()
> sendSecret = writeFile "/tmp/secret" "Who is Benjamin Disraeli?"
>
> andTheAnswerIs :: IO String
> andTheAnswerIs = readFile "/tmp/secret" 

Fortunately, IO isn't an all-or-nothing proposition. 

First and most obviously, you can use pure code in IO whenever you
like. This will arise naturally if your factoring is halfway decent.

> countCharInFile :: Char -> FilePath -> IO Int
> countCharInFile c f = do
>   contents <- readFile f
>   return (countChar c contents)
>
> countChar :: Char -> String -> Int
> countChar c = length . filter (==c)

`countChar` is 100% pure, and easy to test in GHCI.

```
ghci> countChar 'a' "aba"
2
```

GHCI makes it pretty easy to test IO functions too though.

```
ghci> countCharInFile 'a' "Effectful.lhs"
761
```

"Pure" functions, or at least non-effectful ones, have their limits.
What we want to exploit is how `IO` can be composed with other
effectful types to create the exact "computational environment" to model
your solution. This is accomplished with *monad transformers*,
and made manageable with *typeclass constraints*.

Lastly, it's important to understand how to "dip into" other computational
contexts within a larger one: for instance, to run some failure-prone
pure computations in `Maybe` in the middle of an IO function. This
is all about getting comfortable with `Functor`, `Applicative` and
`Monad`.

But first, what is this `IO` thing anyway??

IO
==

I've avoided calling `IO` a "monad" because first and foremost, it's a
*type*, of kind `* -> *` (see my [previous
article](http://slpopejoy.github.io/2015/04/10/Types/) for an intro to
kinds).

```
ghci> :k IO
IO :: * -> *
```

Thus, just like you'd write `Set String` to make `Set` inhabitable
with `String` values, you write `IO Handle` to indicate an IO action
that produces a `Handle` value. If you wanted an `IO` action that
returns `Set String`, you'd need to break out parentheses:

> setProducingAction :: IO (Set String)
> setProducingAction = return $ singleton "contrived"

`IO ()` means the function has no output -- `()` is the "unit type",
which is uninhabitable by any value. Not to be confused with `null`
from those *other* languages, which can inhabit just about anything:
`()` can never have a value except itself. Thus, `IO ()` is all about
side effects, like `main` or `putStrLn` -- we ignore the empty result.

`IO` is, however, a monad. Most of the IO code you will write will use
monadic expressions, which is why we often see `do` notation in IO
functions, the syntactic sugar designed to make coding with Monads
easier.

So, to understand working with IO and other effectful types, we'll need
to know how to work with Monads.

Monad: return and bind
======================

`Monad` is a typeclass, meaning any instance of `Monad` will support 
certain operations:

```
ghci> :i Monad
class Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  fail :: String -> m a
```

Note also the `* -> *` kind. All monads are like IO in that they
require *at least* one other type.

Monad is a member of a set of typeclasses, of which it is strictly 
the most powerful; at the other end is `Functor` and in the middle
is `Applicative`. All are `* -> *` kind, and as a set they form 
the "effectful trio" that we will use again and again.

Until recently, this membership was maintained by convention. While it
would make no sense for a Monad to not also be a Functor, it is not
guaranteed, and what's more it is not expressed in the type
system. With GHC 7.10, the Functor => Applicative => Monad hierarchy
is finally enforced.

return
------

The two most important functions in Monad are `return` and `(>>=)`,
called "bind".  `return` is pretty easy to understand: it allows you
to lift a value into the monadic context.

```
ghci> :t return
return :: Monad m => a -> m a
```

> returnIsEasy :: String -> IO String
> returnIsEasy s = return (s ++ " is in IOooooooo")

You get "return" for free from calling any function with the same underlying type.

> ioNoReturn :: IO String
> ioNoReturn = returnIsEasy "Elvis"


bind (>>=)
----------

Bind is the real magic of `Monad`. With it, we use "monadic functions"
to access, transform and "re-encode" values back into the monadic context.

```
ghci> :t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

Bind formalizes how you operate on a monadic value.  Instead of just
grabbing values via an accessor and going to town, "bind" requires you
to supply a function `(a -> m b)` to obtain the value as an argument
and `return` it, or some transformation of it, back to the monadic
context.  

This inversion of control allows the Monad instance to enforce all kinds
of invariants on what that computation is allowed to do. You don't simply 
"call" a monadic function, you "bind" to it, with a function that accepts
the value and "puts it back". 

IO is a funny Monad instance, in a way. When we use types like `State`
and `Reader` and `Maybe` we reap huge gains in elegance and power;
what's more, the code is right there on Hackage for us to study and
understand. IO, however, is mysterious, and we use it as a monad "just
because". The compiler relies on the monadic context to clearly "demarcate"
the IO code, which is how it guarantees proper sequencing, and how it
can safely move values into and out of the pure runtime.

Binded by the light
===================

Working with bind isn't hard once you get used to it. The bind operator,
`>>=`, is used infix, with a monad inhabitant on the left side, and our
consuming function on the right. This creates a left-to-right code "motion,"
as opposed to the right-to-left flow of function composition.

> countCharInFileBind :: Char -> FilePath -> IO Int
> countCharInFileBind c f = 
>   readFile f >>= 
>       \contents -> return (countChar c contents)

Our function counts how often a character shows up in a text file. 
We bind "readFile f", of type `IO String`, with a lambda whose argument
is the `contents` read from the file. In it, we call our pure function `countChar`
and `return` the result back as `IO Int`. 

With eta reduction, bind makes for some beautiful compositions:

> countCharInFileEta :: Char -> FilePath -> IO Int
> countCharInFileEta c f = readFile f >>= return . countChar c

You can almost visualize a "pipeline" of the file contents "flowing" into
`countChar` with `>>=`.

As we noted above, functions of type `IO ()` are run just for side effects,
like `putStrLn`. Using normal bind, our lambdas would capture a useless `()` non-value.
For this case we use `>>` to simply "sequence" the next operation.

```
ghci> :t (>>)
(>>) :: Monad m => m a -> m b -> m b
```

Here we'll use it to log before performing our computation:

> countCharMsg :: Char -> FilePath -> IO Int
> countCharMsg c f = 
>    putStrLn ("counting " ++ [c] ++ " in " ++ f) >>
>        readFile f >>= return . countChar c

When we want to create local definitions in pure code, we use `where`
or `let ... in`. In monadic code, though, it gets a little tricky to
use `where` because of all the closures we're making. A `where` section
at the end of a bunch of monadic binds won't have any of those binded
variables in scope. The lambdas can call out to `where` functions, but those
functions can't "call into" the lambda's arguments.

`let`/`in` works better, inside of the lambdas. We can stick our definitions
"above" the code that needs them, and "below" the bound arguments we want to 
act upon.

> countCharLog :: Char -> FilePath -> IO Int
> countCharLog c f = 
>    readFile f >>= \contents -> 
>        let cc = countChar c contents 
>        in putStrLn ("Counted " ++ show cc ++ " of " ++ [c] ++ " in " ++ f) 
>           >> return cc

Here we use `let` to capture the results of the pure call in order to 
log it with `putStrLn`, before `return` sends it back to the monadic context.

Do notation
===========

Lots of bind lambdas gets unweildy and confusing eventually. 
`do` notation offers nice syntax to "clean up" repeated `>>=`, `>>`
and `let` expressions.

> countCharLogDo :: Char -> FilePath -> IO Int
> countCharLogDo c f = do
>   contents <- readFile f
>   let cc = countChar c contents
>   putStrLn ("Counted " ++ show cc ++ " of " ++ [c] ++ " in " ++ f)
>   return cc

`do` offers the ["draw from"](https://wiki.haskell.org/Keywords#.3C-) operator `<-`,
which invokes bind under the hood. With it, the argument of the bound 
lambda function `contents` looks like a "normal" variable assignment of the
results of `readFile f`, and is in scope for all subsequent "statements".

Likewise, `let` in do notation puts an invisible `in` around the following
expressions. Finally, side-effect-only expressions like `putStrLn` are simply
"left alone" like they would be in imperative, mutable code.

Do notation is sometimes characterized as "a way to write
imperative-style code in Haskell", ostensibly "easier to read" than,
say, the right-to-left sequencing of function composition. It's better
to see it as a code-cleaning tool, a way of chaining binds of monadic
functions and lambdas, oriented toward accumulating variables in
scope.

Do reflects the left-to-right orientation of `>>=`. But we can go right-to-left
with "reverse bind" `=<<`, resulting in code that looks more like function 
composition.

> countCharReverseBind :: Char -> FilePath -> IO Int
> countCharReverseBind c f = return . countChar c =<< readFile f

Unfortunately, it doesn't compose as nicely, so eta-reduction gets
clumsy. 

> countCharReverseBindEta :: Char -> FilePath -> IO Int
> countCharReverseBindEta c = (return . countChar c =<<) . readFile

In the end, "forward bind" works with infix style better.

Do notation is great. However, until you're really comfortable with monads,
you should regularly "de-sugar" `do` blocks back into `>>=`, `>>`, `let` and lambdas,
as it makes the monadic functionality more explicit and clear.

IO is a Functor 
===============

Like all monads, `IO` is also a `Functor` (and `Applicative`)
instance. It's important to understand how useful IO as a Functor can be!

`Functor`'s main function is `fmap`.

```
ghci> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b
```

To see it's relationship to Monad, it's illustrative to compare `fmap`
to Monad's "reverse bind", `=<<`:

```
ghci> :i (=<<)
(=<<) :: Monad m => (a -> m b) -> m a -> m b
```

With bind, we act on a monadic value and return it to the Monad's effectful 
context. `fmap` however is simply about applying a pure function "inside"
the context. 

The classic operation `map` is well-known in pretty much every
functional-ish language in existence; it allows you to apply a function
over the elements of a list, collecting the results in a list.
In Haskell, `map` is the specialization of `fmap` for lists.

> incrementAllBy :: Int -> [Int] -> [Int]
> incrementAllBy i is = fmap (+ i) is 

Many tourists to functional programming learn `map` (and maybe its
cousin `reduce`/`fold`) as a list operation only before stamping FP
onto their resume. Haskellers go a lot further with Functor.

With effectful types like `IO`, `fmap` allows us to "plug in" a pure operation
into a effectful one. Given some effectful result, we want to transform it
further, so we 'fmap' the transformation over the effectful action. 

Our char-counting function is an excellent candidate for plugging in `fmap`.

> countCharInFileFmap :: Char -> FilePath -> IO Int
> countCharInFileFmap c f = fmap (countChar c) (readFile f)

Our "effectful" code is looking more and more functional. Indeed the only
indication here that our code has "magic" IO involved is the type `IO Int`.

Following the types makes it clear what `fmap` is accomplishing:

```
ghci> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b

ghci> :t countChar 'c'
countChar 'c' :: String -> Int

ghci> :t readFile "path"
readFile "path" :: IO String

ghci> :t fmap (countChar 'c') (readFile "path")
fmap (countChar 'c') (readFile "path") :: IO Int

```

Given our effectful operation `readFile f` which returns a `String`,
`fmap` is used to apply a pure function that converts a `String` to an
`Int`. This happens *within the effectful context:* no `return` required.

This combination of `fmap` with a monadic call is so common that the
infix synonym `<$>` is used to make those pesky parentheses disappear:

> countCharInFileFmap2 :: Char -> FilePath -> IO Int
> countCharInFileFmap2 c f = countChar c <$> readFile f

That's some pretty sweet code right there.

List is a Monad
===============

We've talked a lot about `IO` so far, which is strongly associated
with Monad, such that Monad is strongly associated with effectful
code. Let's challenge that now by looking at a highly pure Monad, the
humble list.

We saw above how `fmap` works on lists, indeed the `map` operation is
a "Greatest Hit" of functional programming. It's easy to understand
how a function could be applied to a list's values, with `fmap`
creating a new list from the results.

So we understand how list is a `Functor`. How is it a `Monad` though?

`return` is pretty easy to understand, it simply creates a singleton list
out of the value. 

```
ghci> return 1 :: [Int]
[1]
```

Bind is more interesting. Recall that bind takes a function which
accepts the underlying value, and returns a transformed value into the
monadic context. With list, the type of bind is `[a] -> (a -> [b]) ->
[b]`. 

Like `fmap`, the function will receive every value in a separate
call. Unlike `fmap`, the function has to return the new value as a
list. Clearly, list's implementation of `bind` will need to
concatenate these list results to form the new list.

We can see therefore how Monad offers strictly more powerful
transformations than Functor: while fmap can only transform an
individual value "in place", `>>=` can return an empty list, or a list
with more elements than the original.

To illustrate:

> listBindEx :: [Int] -> [Int]
> listBindEx is = is >>= (\i -> replicate i i)

```
ghci> listBindEx [1,2,3]
[1,2,2,3,3,3]
```

To make things more interesting, let's look at what it means to bind to
two lists.

> listBind2 :: [Int] -> [Int] -> [Int]
> listBind2 is js = is >>= (\i -> js >>= (\j -> [i,j]))

```
ghci> listBind2 [1,2,3] [5,6]
[1,5,1,6,2,5,2,6,3,5,3,6]
```

If this looks like "list comprehensions", that's exactly correct: list
formalized as a monad creates list comprehensions. Thus, lists are an
excellent way to "unlearn" the notion that Monads and do notation are
somehow akin to imperative programming:

> listBind2Do :: [Int] -> [Int] -> [Int]
> listBind2Do is js = do
>   i <- is
>   j <- js
>   [i,j] 

```
ghci> listBind2Do [1,2,3] [5,6]
[1,5,1,6,2,5,2,6,3,5,3,6]
```

As the result shows, there is no way to see `i <- is` happening
"before" `j <- js` in the `do` code above. The specifics of how list
implements bind determines the final result, not the order the
functions are declared. After all, we're just composing functions: how
they *fire* is not related necessarily to how they are *declared*.

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
to *formalize* this practice in our app, however, as functions
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
light formalization that also hides `AppConfig` from our
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
r)`: it literally formalizes an "extra argument" into a *datatype*.

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
