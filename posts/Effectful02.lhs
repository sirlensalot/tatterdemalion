----------
title: Effectful Haskell: Reader, Transformers, Typeclasses
author: Stuart Popejoy
date: 2015-05-24
----------

This article is the second in a series on "effectful" Haskell. In the
[last one](Effectful01.html) we looked at how to work with `IO` as a
`Monad` and a `Functor`, using bind, `return`, `fmap`, and do
notation. We also briefly examined the list type as a monad.

The type we'll be examining in this article, `ReaderT`, is fully pure,
built from "normal" Haskell code, requiring no magic from the compiler
like `IO`. 

Of course, we'll still want to use it with `IO` and other effectful
types, so we'll examine how *monad transformers* allow us to "stack" 
these types into the exact behavior we want. 

We'll also allow
our functions to polymorphically target the exact subset of behavior
they need, by using the *typeclasses* associated with these types:
`MonadReader` and `MonadIO`.

> {-# LANGUAGE FlexibleContexts #-}
> import System.IO
> import Control.Monad.Reader
> import Control.Monad.Trans.Reader (Reader)
> import Control.Applicative
> import System.Environment
> import Control.Monad.State

Use case: Configuration
=======================

Pretty much any app needs configuration. Whether it comes on the command line
or from a file, you'll need to get at the information in all sorts of places
in your code. Factoring this properly can be a challenge in any language.

We'll follow the common practice of defining a data structure around our specific
config. We won't worry just yet how we inflate it. Here's an example config
with some contrived properties:

> data AppConfig = AppConfig {
>     logfile :: FilePath
>   , version :: String
>   , maxMessageLength :: Int
> } deriving (Show)

Haskell has magical ways to provide "global variables", such as using an `IORef`
in IO. There's nothing unsafe about this, but it's unsatisfying. Everything is
simply in IO, so the types don't help us understand our program or validate its
correctness.

Our first attempt, then, is to simply pass our config value into every
function as an argument. We'll use two contrived functions as examples.
The first initializes an application log file handle, writing a preamble
with the application version:

> initLogFile :: String -> AppConfig -> IO Handle 
> initLogFile preamble config = do
>   handle <- openFile (logfile config) WriteMode
>   hPutStrLn handle (preamble ++ ", version: " ++ version config)
>   return handle

Our application will also be sending messages that cannot exceed some
maximum length. So we provide a validation function to be used before
we send.  It returns the `Either` type, with `Left` canonically
indicating failure and `Right` success.

> validateMessage :: String -> AppConfig -> Either String ()
> validateMessage msg config = 
>      if (length msg > maxMessageLength config)
>      then Left ("Message too long: " ++ msg)
>      else Right ()

Formalization via type synonyms
-------------------------------

No problem with this, except it's a little ... "manual". Every time we need the config
value, we're adding it to the argument list ad-hoc. It would be nice to
*formalize* this, as a way of declaring that these functions share some environment,
and maybe to get `AppConfig` out of the argument list.

An initial approach would be to use a type synonym.

> type ConfigReader a = AppConfig -> a

Our polymorphic type signature specifies a function taking an `AppConfig` value
and returning some `a`. Let's try it on our API functions:

> initLogFileTS :: String -> ConfigReader (IO Handle)
> initLogFileTS = initLogFile
> 
> validateMessageTS :: String -> ConfigReader (Either String ())
> validateMessageTS = validateMessage 

As the equations prove, our code still works with the type synonym. We
achieve a light formalization and get the explicit argument out of the
signature. 

It's just for show though, since we still have to wrangle arguments to
use the underlying functions. If we want to use both functions together,
we're going to have to explicitly pass the `AppConfig` value to both.

> -- validate our prompt before using it to open the logfile
> validateAndInitLogTS :: String -> ConfigReader (IO (Maybe Handle))
> validateAndInitLogTS prompt config = 
>     case validateMessage prompt config of
>         Left err -> putStrLn ("Invalid prompt: " ++ err) 
>                     >> return Nothing
>         Right () -> Just <$> initLogFile prompt config

At this point the code is just confusing: we now have some mystery
`config` value, which we'd have to reference the definition of `ConfigReader`
to understand. 

Our ideal solution would allow us to code "under" this contract, such
that all functions sporting the same type can reference this environment
without having to pass it to and fro.

Formalization with a newtype
----------------------------

Often in Haskell, a type synonym is often a data type waiting
to be born. What happens if we simply `newtype` a function that takes an
environment?

> newtype CReader a = CReader { runCR :: AppConfig -> a }

The "data" of our new type, `AppConfig -> a`, looks like our type synonym above.
We've wrapped it in a constructor and given it an accessor `runCR`. (Remember,
a `newtype` is the same as a `data` but with only one constructor with one field.)

Let's try it out in our API:

> initLogFileCR :: String -> CReader (IO Handle)
> initLogFileCR p = CReader $ \c -> initLogFile p c

> validateMessageCR :: String -> CReader (Either String ())
> validateMessageCR m = CReader $ \c -> validateMessage m c

Hmmm ... OK. How about using both functions at the same time?

> validateAndInitLogCR :: String -> CReader (IO (Maybe Handle))
> validateAndInitLogCR msg = CReader $ \c -> 
>     case runCR (validateMessageCR msg) c of
>         Left err -> putStrLn "Invalid init message" >> return Nothing
>         Right () -> Just <$> runCR (initLogFileCR msg) c
>
> -- some example code for how we'd use 'CReader' in, say, main
> runCRWithConfig :: AppConfig -> IO (Maybe Handle)
> runCRWithConfig config = runCR (validateAndInitLogCR "Hello CR") config

This code is pretty awkward, but it's an important step forward in our
quest for a more elegant solution. 

First, we have a much stronger formalization: functions in `CReader`
return *a function that takes a config*, wrapped in the `CReader`
constructor. Thus every function starts with the lambda `CReader $ \c ->`, followed
by the code to do stuff. 

Second, to call another function using `CReader`, we use the accessor 
`runCR` to get at the lambda function, and supply the config value `c` to it.
We've reworked function application here, in that we first call the outer
function, and then apply our environment to it.

What is nice about this is the consistency of usage: every function under `CReader`
uses the same code and style to get at, and pass around, the environment. What isn't
nice is all of the explicit wrapping and unwrapping we have to do.

Formalization with Functor
--------------------------

You might have noticed that our newtype is of kind `* -> *`.

```haskell
ghci> :k CReader
CReader :: * -> *
```

This makes it potentially a candidate for implementing the famous
trio of Monad, Applicative and Functor! However, we should see if 
we need all this power. What does Functor offer us?

```haskell
ghci> :i Functor
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
```

`fmap` allows us to execute a function "inside" of our Functor. 
That sounds like it could be nifty. Let's implement it.

> instance Functor CReader where
>     fmap f cr = CReader $ \c -> f (runCR cr c)

Sure enough, it looks a lot like our code above: we use `runCR`
to "unwrap" the product of some `CReader a` function, run `f` on
it, and "wrap" it back up with `CReader -> \c`. 

Let's try to swap out our boilerplate with Functor!

Thing is, `fmap` needs that first `CReader` to get going. If
we want to use `fmap` for our formalization, we're missing something,
at least for our baseline API functions that simply want to 
use the config. Let's use "type holes" to let GHC tell us what we need:

```haskell
validateMessageF m = fmap (validateMessage m) _
```

Here we've issued `fmap` with the partially-applied function `validateMessage m`.
Our implementation above had `CReader $ \c -> validateMessage m c`, but why bother
with that `\c -> ... c` when `validateMessage m` is the same thing? Point-free FTW.

The underscore is a "type hole", so we can figure out the missing piece: it will give us
an informative error when we try to compile:

```haskell
posts/Effectful02.lhs:219:49: Found hole ‘_’ with type: CReader AppConfig …
    Relevant bindings include
      m :: String
        (bound at .../Effectful02.lhs:219:20)
      validateMessageF :: String -> CReader (Either String ())
        (bound at .../Effectful02.lhs:219:3)
    In the second argument of ‘fmap’, namely ‘_’
    In the expression: fmap (validateMessage m) _
    In an equation for ‘validateMessageF’:
        validateMessageF m = fmap (validateMessage m) _
Compilation failed.
```

Hmmm. We need a `CReader AppConfig`! How interesting. We need a
version of our Functor that simply returns the config itself.
Fortunately that's pretty easy to write:

> askConfig :: CReader AppConfig
> askConfig = CReader id

We called it "askConfig" because we're "asking" the environment
to return itself. Note we used 'id', the function from base, instead
of writing `CReader $ \c -> c`. We're ready to get all Functor-y.

> validateMessageF :: String -> CReader (Either String ())
> validateMessageF m = fmap (validateMessage m) askConfig
>
> initLogFileF :: String -> CReader (IO Handle)
> initLogFileF p = fmap (initLogFile p) askConfig

This is starting to look pretty good. What about our function
that wants to use both?

> validateAndInitLogF1 :: String -> CReader (IO (Maybe Handle))
> validateAndInitLogF1 p = fmap doInit (validateMessageF p)
>     where doInit :: Either String () -> (IO (Maybe Handle))
>           doInit (Left err) = putStrLn ("Invalid prompt: " ++ p)
>                               >> return Nothing
>        -- doInit (Right ()) = ???

Uh-oh. `fmap` can't help us here. In the second pattern-match on `doInit`,
we somehow need to call `initLogFileF`: *but we don't have the config anymore*.
"doInit" is a pure function that's supposed to run "inside" of the Functor,
implying that we would need to capture the config argument elsewhere.

We're essentially doing control-flow, and Functor isn't the right fit. 
We could instead fmap a function with `askConfig` that then manually
supplies config to the underlying functions, but that obviously
defeats our desire to formalize our computational environment.

Formalization with Monad
------------------------

We made some progress with our formalization but ran aground. Let's see 
what a Monad implementation buys us.

> instance Monad CReader where
>    -- return :: a -> CReader a
>    return = CReader . const 
>    -- >>= :: CReader a -> (a -> CReader b) -> CReader b
>    a >>= f = CReader $ \c -> runCR (f ((runCR a) c)) c

NB: my comments "inline" the types of `return` (normally
`a -> m a`) and bind (normally `m a -> (a -> m b) -> m b`).

`return` is straightforward. Given an `a`, we simply want a `CReader` with
it wrapped up: `CReader $ \c -> a`. Since we're ignoring the config argument `c`
we simply use `const` and compose it with the `CReader` constructor.

Bind is tricky. Here's an exploded version to help understand what's going 
on:

```haskell 
    -- >>= :: CReader a -> (a -> CReader b) -> CReader b
    a >>= f = CReader $ \c -> let a' = runCR a c
                                  f' = f a'
                              in runCR f' c  
```

We take advantage of the fact that a CReader inner function will always supply
the config, and evaluate the function in `a` with it to get `a'`. Then, we'll
supply that `a'` to our second argument, `f`, to get a `CReader b`, which we'll
call `f'`. 

However, we're still inside a lambda inside of a constructor, so that `CReader b`
needs to be "unwrapped" again to get at its underlying lambda. Thus the last call
to `runCR` with `f'` and `c`.

We're in business. Let's roll.

> validateMessageM :: String -> CReader (Either String ())
> validateMessageM m = askConfig >>= return . validateMessage m

> initLogFileM :: String -> CReader (IO Handle)
> initLogFileM p = askConfig >>= return . initLogFile p 

> validateAndInitLogM :: String -> CReader (IO (Maybe Handle))
> validateAndInitLogM p = do
>    v <- validateMessageM p
>    case v of 
>      Left err -> return (putStrLn ("Invalid prompt: " ++ p)
>                        >> return Nothing)
>      Right () -> do
>         h <- initLogFileM p
>         return (fmap Just h)


We've built our ideal solution. It frees client code from passing variables, and
implementation code from wrangling them, formalizing our computational
context as an "environment" with `AppConfig` available to read from.

In fact, we've re-built the classic monad 'Reader'. Replace
`CReader` with `Reader` and `askConfig` with `ask` and you've
got one of the greatest hits of effectful types: `Reader`.

Well, `ReaderT` actually.

ReaderT and the case of the missing monad
=========================================

A funny thing happened on the way to modern Haskell: some classic
monads disappeared. 

`Reader` is one such classic. The closest we can find is a type synonym defining 
it in terms of `ReaderT Identity`. 

```
ghci> :i Reader
type Reader r = ReaderT r Identity
```

These days, since monad transformers are strictly more powerful than their
monad grandparents, the Haskell base libraries just supply the transformer
version with a type synonym using the trivial monad `Identity` to 
play the role of the underlying, transformed monad. Indeed, `Reader`
really is `Identity` with `ReaderT` goodness slathered on top.

Reader without the T
--------------------

We'll go ahead and ditch the old versions of our code and rewrite
directly to `Reader` (as opposed to calling out to our previously-defined
versions).

> validateMsgRdr :: String -> Reader AppConfig (Either String ())
> validateMsgRdr msg = do
>   max <- reader maxMessageLength
>   if (length msg > max)
>     then return $ Left ("Message too long: " ++ msg)
>     else return $ Right ()

Okeydoke. We just inlined `validateMessage` into our `CReader` implementation
above.

Reader and IO
-------------

Now we turn to rewriting our IO function. However, it's time to address
a shortcoming in the code above: we return IO "actions"
wrapped up in `CReader`, which means we won't actually perform any IO
until we finish our entire sojourn into our configured environment.

> openLogFileRdr :: Reader AppConfig (IO Handle)
> openLogFileRdr = do
>   f <- reader logfile
>   return (openFile f WriteMode)

What we really want is to *combine* `IO` and `Reader` into a new
effectful type. This is precisely what a *monad transformer* is for: providing
functionality as a "building block" to construct the exact 
type we need. 

ReaderT: a monad transformer
============================

For `initLogFile`, we want to combine IO with Reader functionality, so we'll
use the `ReaderT` monad transformer.

> initLogFileRT :: String -> ReaderT AppConfig IO Handle
> initLogFileRT preamble = do
>   f <- reader logfile
>   v <- reader version
>   h <- liftIO $ openFile f WriteMode
>   liftIO $ hPutStrLn h (preamble ++ ", version: " ++ v)
>   return h

Nice. Our function produces a `Handle` value, and no "config"
argument needed. `reader` is used to obtain the `logfile` and `version`
values.

However, our two IO actions `openFile` and `hPutStrLn` now require the
mysterious function `liftIO` in order to typecheck. To understand this
we need to dig a little deeper into the type. 

Transformer Kinds
-----------------

ReaderT has a pretty scary kind signature:

```haskell
ghci> :k ReaderT
ReaderT :: * -> (* -> *) -> * -> *
```

Yikes! Don't worry though, it makes more sense as you add the necessary types.
Like `Reader`, the first "slot" is for the environment type itself, `AppConfig`.

```haskell
ghci> :k ReaderT AppConfig
ReaderT AppConfig :: (* -> *) -> * -> *
```

The second term is the parenthesized two stars, `(* -> *)`. This is where we
place our "stacked" effectful type. As we noted in the previous article,
Monads, Applicatives and Functors are all *at least* two-kinded: they 
all "contain" or "operate on" or "produce" the second type. 

In this case, we want to use IO, whose kind is of course `* -> *`. So that slots
right in.

```haskell
ghci> :k ReaderT AppConfig IO
ReaderT AppConfig IO :: * -> *
```

And voila, we've arrived at a two-kinded, effectful type! We've "built
our own monad", simply by combining ReaderT and IO. The last slot is for whatever
value our functions produce: thus `ReaderT AppConfig IO Handle` in the example 
above.

All transformers will have a slot for `(* -> *)` where
we can stick another two-kinded type. Because we just made a 
new type with this `* -> *` kind, we can stick it in yet another 
transformer! In this way we can keep building new behavior until
we have the exact type we need.

Transformers and IO
-------------------

Like all monad transformers, `ReaderT` is purpose-built to be used with other types.
Under the hood, this means that it explicitly supports the APIs of a
known set of other types, in order to "lift" their operations into the 
transformer's context. The transformer author is tasked with writing a fair
amount of boilerplate to guarantee this interoperation. The transformer
*user* on the other hand can mix and match the supported types as needed.

However, `IO` is not a transformer, so it needs a little extra help.
To support IO, a transformer must be an instance of `MonadIO`, providing
an implementation of `liftIO`. This allows the transformer to "lift" the
results of an IO action into the transformer's context.

On the user side, we sprinkle these `liftIO` calls whenever we want
to use IO. 

Reader vs ReaderT
-----------------

`Reader` and `ReaderT` aren't really compatible. As we saw above, `Reader` is but 
a type synonym for `ReaderT` combined with the `Identity` monad. As such we 
won't be able to use it with `IO` or any other transformer stacks we cook up.

Fortunately, adapting our pure function to `ReaderT AppConfig IO` is a breeze.

> validateMsgRT :: String -> ReaderT AppConfig IO (Either String ())
> validateMsgRT msg = vfun <$> reader maxMessageLength
>   where 
>     vfun max | length msg > max = Left ("Message too long: " ++ msg)
>              | otherwise        = Right ()

A breeze, and a nice day for golfing, too! Here we use the fact that
`ReaderT` (or any Monad or Applicative) is a `Functor` too, to "plug in"
our pure validation function `vfun` to the `reader` monadic call using 
the infix synonym for `fmap`, `<$>`. Remember
that if you have pure code simply to `return` a value, you
can probably `fmap` instead!

So we're looking pretty good now. We can use `initLogFileRT` and `validateMsgRT`
in the same "stack": `ReaderT AppConfig IO`. 

The only
problem is the future. What if we decide later to add another transformer into our stack?
We'd have to change this already quite long type to `ReaderT AppConfig (Foo Bar (IO ...))`
anywhere it appears. 

But even in the here and now, it's kind of unfortunate we've bound
`validateMsgRT` to `IO`.  There's no IO going on in the function after
all, so it seems a shame to force any calling code to run in IO.

Polymorphic ReaderT
-------------------

A simple solution is to use a type variable in the two-kinded "slot" of
`ReaderT`. We can't have any old type in there though. We'll need
to constrain it to `Monad`, and because of golfing, and that we're not
using GHC 7.10, a `Functor` too. ^[In GHC 7.10, `Monad` is constrained to
`Applicative` and `Applicative` to `Functor`, so the `Functor` constraint
is no longer necessary].

> validateMessageRTM :: (Functor m, Monad m) => 
>                       String -> ReaderT AppConfig m (Either String ())
> validateMessageRTM msg = vfun <$> reader maxMessageLength
>   where 
>     vfun max | length msg > max = Left ("Message too long: " ++ msg)
>              | otherwise        = Right ()

Better: `IO` is gone. We just need our stack to operate on
some Monad/Functor and we're in business.

But we're not done yet: this will only work if `ReaderT` is the
"outermost" monad on the stack. In other words, we're still concretely
specifying `ReaderT` itself, which will get us into trouble if
we don't stack our monads just so.

Indeed, `ReaderT` isn't the only Reader. There's also `RWST`, a
convenience monad transformer that bundles `Reader`, `Writer` and
`State` into a single monad. It's really not that hard to stack these
together, but whatever: RWST is a thing, and it's not `ReaderT`: so it
won't work with `validateMessageRTM`.

MonadReader: an effectful typeclass
===================================

What we want to do instead is *constrain* our function's type to a typeclass,
instead of explicitly specifying the whole type.

The typeclass `MonadReader` enumerates all of the functionality in `ReaderT`,
but because its a typeclass, it can be *implemented* by any transformer that
interoperates with `ReaderT`. 

```haskell
ghci> :i MonadReader
class Monad m => MonadReader r (m :: * -> *) | m -> r where
  ask :: m r
  local :: (r -> r) -> m a -> m a
  reader :: (r -> a) -> m a
  	-- Defined in ‘Control.Monad.Reader.Class’
instance Monad m => MonadReader r (ReaderT r m)
  -- Defined in ‘Control.Monad.Reader.Class’
instance MonadReader r ((->) r)
  -- Defined in ‘Control.Monad.Reader.Class’
```

(Note the freakiness of that last instance: yes, *the
function arrow itself* is an instance of MonadReader.)

Here we see the complete API of `ReaderT`: 

* `ask`, which obtains the entire environment value

* `local`, a more esoteric use case where
you want to run the `m a` argument in an environment "modified" by the `(r -> r)` function argument

* our good friend `reader`, which applies an accessor function to the environment value.

The signature of the typeclass, `Monad m => MonadReader r (m :: * -> *) | m -> r `, 
is complex, but not unlike `ReaderT` above. `r` is the environment type; `m` is constrained
to be an instance of `Monad`, and must be two-kinded. (The last part is functional
dependency syntax which we won't get into here).

The role of typeclasses in types is to constrain polymorphism. In the signature
for `MonadReader`, we see the `m` type argument being constrained to `Monad`. In 
our code, we'll want to constrain our types to `MonadReader`, which will require us
to supply the `r` type, `AppConfig`:

> validateMessageMR :: (Functor m, MonadReader AppConfig m) => 
>                      String -> m (Either String ())
> validateMessageMR msg = vfun <$> reader maxMessageLength
>   where 
>     vfun max | length msg > max = Left ("Message too long: " ++ msg)
>              | otherwise        = Right ()

We're open for business. This function will work with any stack or monad
we can think of, as long as it provides a `MonadReader AppConfig` environment.

MonadIO
-------

Indeed, we can extend this polymorphic concept to our IO function too. 
Of course, this means that we'll want to *additionally constrain* the
monadic argument to `MonadIO`.

We saw `MonadIO` above with `liftIO`. There, it was allowing `ReaderT`,
an instance of `MonadIO`, to lift IO operations into its context. Now
we're going to use the typeclass itself to make our IO function polymorphic.

> initLogFileMR :: (MonadReader AppConfig m, MonadIO m) => 
>                  String -> m Handle
> initLogFileMR preamble = do
>   f <- reader logfile
>   v <- reader version
>   h <- liftIO $ openFile f WriteMode
>   liftIO $ hPutStrLn h (preamble ++ ", version: " ++ v)
>   return h

Our library is now maximally polymorphic: our IO function can be used
by any stack or monad offering both `MonadIO` and `MonadReader AppConfig`,
while our pure function only needs the `MonadReader AppConfig` requirement.

Putting it all together
=======================

We've focused on how to write library functions to make use of ReaderT.
Here we'll write some code to show a working, if contrived, application.

First, we'll define a function to read in our values as a
tuple. Don't try this at home; using Aeson to read values as JSON, or almost
anything else, is better than this hack.

> readConfig :: FilePath -> IO AppConfig
> readConfig f = (fromTup . read) <$> (readFile f) 
>     where fromTup (a,b,c) = AppConfig a b c 

Now we can create a file with the following contents:

```
("/tmp/logfile","1.0.0",20)
```

and `readConfig` will turn it into an `AppConfig`. It does so by
applying the pure functions `read` (which constructs a type value from
a String value) composed with our local function `fromTup`. Figuring
out how `read` determines exactly what type to inflate is left as an
exercise for the reader.

With that, we can write the rest of our little application with
a `main` function to fire up our monad stack, a `go` function to run inside of it,
and a utility `logMsg` function.


> main :: IO ()
> main = do
>   configFile <- head <$> getArgs
>   config <- readConfig configFile
>   runReaderT go config
>
> go :: (Functor m, MonadReader AppConfig m, MonadIO m) => m ()
> go = do
>   h <- initLogFileMR "Starting"
>   m <- liftIO $ getLine
>   v <- validateMessageMR m
>   case v of
>      (Right ()) -> logMsg h $ "Valid Input" 
>      (Left err) -> logMsg h $ "Invalid input: " ++ err
>
> logMsg :: (MonadIO m) => Handle -> String -> m ()
> logMsg h = liftIO . hPutStrLn h

All done. Note how `ReaderT` cleans up our code as promised: `go` calls
`initLogFileMR` and `validateMessageMR` with no config arguments in sight. 

runReaderT
----------

The `main` function gets us where we need to run our monad stack. 

`getArgs` gets the array of command-line
arguments, and applies the pure function `head` to get the first one; we pass this to `readConfig` to get an `AppConfig` value.

At this point it's ready to build our monadic stack with `runReaderT`. 

```haskell
ghci> :t runReaderT
runReaderT :: ReaderT r m a -> r -> m a
```

`runReaderT` sounds like we're "running" some code to get monadic magic,
and looks like it too: the first argument, `ReaderT r m a` is our monadic
function, and the second, `r`, is our configuration value. In our code,
we call `runReaderT go config` with `go` being our monadic value.

However, it's important to realize that `runReaderT` is not a typical 
function but instead an *accessor* to the value contained in a `ReaderT`
monad.

```haskell
ghci> :i ReaderT
newtype ReaderT r (m :: * -> *) a
  = ReaderT {runReaderT :: r -> m a}
```

With `runReaderT`, we're "unwrapping" a value, meaning that `go` applied
with `config` *creates* the monadic value to be used in the underlying code.

In so doing, it also "picks" the type of our polymorphic function. By running
in IO, and using `runReaderT`, we instruct the type inference to use the
type `ReaderT Config IO ()` for `go`.
Indeed, we can call `go` elsewhere to pick a different type for the polymorphic
function at that site:

> goState :: IO ()
> goState = evalStateT (runReaderT go emptyConfig) "" 
>     where emptyConfig = AppConfig "" "" (0 :: Int)

Here our code picks the type `ReaderT AppConfig (StateT String IO) ()`, proving
that `go` is indeed polymorphic.
 
Miscellany
---------------------------

Some other notes about our contrived application code:

**Even though we are interested in a pure result, we still have to bind
to call validateMessageMR**. We can't simply put the call
to `validateMessageMR` inside of the `case` statement (like we could with
the non-monadic `validateMessage`) but instead bind to get the result.
This is because we're using the monadic functionality of `MonadReader AppConfig`.

**logMsg is in MonadIO, not IO**. This is not necessary, but would result
in both calls to `logMsg` requiring `liftIO`. Functions in MonadIO do not 
have to be "lifted" inside of a monad transformer stack. Of course, we
still have to use `liftIO` to call `hPutStrLn` inside of the function.

Under the hood: Looking at the source of ReaderT
================================================

This article is focused on operational usage of ReaderT, in order to give a taste
of how code can be written and factored in a real-world application. However, there's
no reason not to examine how `ReaderT` works, using the code on Hackage.

The meat of ReaderT is the newtype itself:

```haskell
newtype ReaderT r m a = ReaderT {
        runReaderT :: r -> m a
}
```

The newtype contains `r -> m a`, a function taking the environment `r` value and
returning an `a` value contained in the underlying/transformed monad `m`.

Thus, a function of type `ReaderT r m a` has *encoded* a hidden argument
into itself. For example, a function that takes an Int and works in ReaderT
would have the type `Int -> ReaderT r m a`; we can conceptually inline this 
type as `Int -> (r -> m a)`, meaning that once we apply an `Int` argument, 
we're left with a function that takes an `r` argument and returns `m a`.

This is why
we invoke runReaderT with the environment argument *last*, as seen above
with `runReaderT go config`: `go` is fixed to the type `ReaderT AppConfig IO ()`,
which means `go` is a newtype wrapping `AppConfig -> IO ()`. We provide `config`
to `go` and get `IO ()` coming out of `runReaderT`.

Note that *within* our ReaderT functions, we don't want to be
directly manipulating this function, say by constructing a new 

It's important not to confuse the newtype with a type synonym of
`r -> m a`. A type synonym would not allow us to define unique instances
of `Monad`, `MonadTrans` and `MonadIO` which are what allow us to get effectful
work done in the function. We could abuse the ReaderT type and write the following
function:

> broken :: ReaderT String IO ()
> broken = ask >>= liftIO . putStrLn >> ReaderT (const (return ()))
>
> useless :: ReaderT String IO ()
> useless = ask >>= liftIO . putStrLn >> broken

This typechecks, but we have no access to any of the monadic goodness. The
only way to *use* ReaderT is to "leave the newtype alone", letting it's various
instances "rebuild" the resulting `r -> m a` function to pass along the 
results we want.

MonadTrans instance
-------------------

First and foremost, ReaderT needs to support other monadic contexts via `lift`:

```haskell
instance MonadTrans (ReaderT r) where
    lift   = liftReaderT

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)
```

As the type of `liftReaderT` shows, we are literally taking a `m a` 

The newtype however means
that 

 However, because the newtype
encapsulates the last function arrow, all of the instances on the newtype
will be "rebuilding" the enclosed function, basically preventing the 
implementing function from directly accessing the argument.

The accessor function in the newtype acts as the "runner" of the monad stack,
since by "accessing" the result `m a` value we're basically completing our
monadic operation, providing the environment value `r` as an argument.



 built of a function taking the Reader's environment, `r`, that
returns something in the underlying `m a` monad type. The reason we use the 
accessor `runReaderT` to "launch" a function in ReaderT is simply to "extract"
the `m a` value when we're done, providing the environment value `r`.everything done within this runReaderT invocation
will happen via bind, `lift` and `return`.

Let's take a look at `lift` next, since this is a transformer after all.

```haskell
instance MonadTrans (ReaderT r) where
    lift   = liftReaderT
```


