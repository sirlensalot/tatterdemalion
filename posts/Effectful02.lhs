----------
title: Effectful Haskell: Reader
author: Stuart Popejoy
date: 2015-05-24
----------


> {-# LANGUAGE FlexibleContexts #-}
> import System.IO
> import Control.Monad.Reader
> import Control.Applicative


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
