----------
title: Effectful Haskell: Reader, Transformers, Typeclasses
author: Stuart Popejoy
date: 2015-08-25
----------

This article is the second in a series on "effectful" Haskell. In the
[last one](Effectful01.html) we looked at how to work with `IO` as a
`Monad` and a `Functor`, using bind, `return`, `fmap`, and do
notation. We also briefly examined the list type as a monad.

Here we'll be looking at `Reader`, starting with a 
common use case -- accessing application configuration -- and working
our way to effectful bliss, starting from first principles.

Once we're there, we'll examine how we can add other types to our
context, like IO and other monads, using *monad transformers*.

We'll also learn how we can design functions that operate on the exact
subset of monadic functionality we want, by constraining to the
particular typeclass (e.g., `MonadReader`, `MonadIO`) exposed by a
given transformer.


> {-# LANGUAGE FlexibleContexts #-}
> import System.IO
> import Control.Monad.Reader
> import Control.Monad.Trans.Reader (Reader)
> import Control.Applicative
> import System.Environment
> import Control.Monad.State

Use case: Configuration
=======================

Almost any app needs configuration. Whether it comes on the command line
or from a file, you'll need to get at the information in all sorts of places
in your code. Factoring this properly can be a challenge.

We'll follow the common practice of defining a data structure around a specific
config. We won't worry just yet how we inflate it. Here's an example config
with some contrived properties:

> data AppConfig = AppConfig {
>     logfile :: FilePath
>   , version :: String
>   , maxMessageLength :: Int
> } deriving (Show, Read)

Our first attempt to share this config throughout our application will
simply pass it to every function that needs it. We'll use two
contrived functions as examples. 

The first initializes an application
log file handle. It needs the log file path and the version from the config.

> -- | opens a handle as specified in config and writes a preamble.
> initLogFile :: String -> AppConfig -> IO Handle 
> initLogFile preamble config = do
>   handle <- openFile (logfile config) WriteMode
>   hPutStrLn handle (preamble ++ ", version: " ++ version config)
>   return handle

Our application will also deal with messages that cannot exceed some
maximum length. We provide a validation function to enforce this, reading
the maximum length from config.

> validateMessage :: String -> AppConfig -> Either String ()
> validateMessage msg config = 
>      if (length msg > maxMessageLength config)
>      then Left ("Message too long: " ++ msg)
>      else Right ()

In both cases, we simply pass the `AppConfig` value in as an argument.

Formalization via type synonyms
-------------------------------

No problem with this, but it's a little ... "manual". It would be nice
to *formalize* needing config, as a way of declaring that these
functions share some environment.

An initial approach is to use a type synonym.

> type ConfigReader a = AppConfig -> a

This specifies a function that takes an `AppConfig` value and returns
some `a`. Ending our function signatures with this will unify them
as having a last argument of type `AppConfig`.

> initLogFileTS :: String -> ConfigReader (IO Handle)
> initLogFileTS = initLogFile
> 
> validateMessageTS :: String -> ConfigReader (Either String ())
> validateMessageTS = validateMessage 

As the equations prove, our code still works with the type synonym. We
achieve a light formalization and get the explicit argument out of the
signature. It's just for show though. We still have to wrangle arguments to
use the underlying functions. 

To illustrate, let's write a function
to call both of these functions. We'll validate the preamble message,
and only if it's valid will we initialize the logfile.

> -- validate our prompt before using it to open the logfile
> validateAndInitLogTS :: String -> ConfigReader (IO (Maybe Handle))
> validateAndInitLogTS prompt config = 
>     case validateMessage prompt config of
>         Left err -> putStrLn ("Invalid prompt: " ++ err) 
>                     >> return Nothing
>         Right () -> Just <$> initLogFile prompt config

Our type synonym isn't serving us very well here. We have the mystery
`config` argument that is obscured by the type synonym, and we have
to place it in the right spots of our calls to other functions.

Our ideal solution would allow functions to interoperate with a
"shared environment", such that all functions sporting the same type
can reference this environment without having to pass it to and fro.
Let's keep digging.

Formalization with a newtype
----------------------------

Often in Haskell, a type synonym is a datatype waiting
to be born. What happens if we simply `newtype` a function that takes an
environment?

> newtype CReader a = CReader { runCR :: AppConfig -> a }

The "data" of our type, `AppConfig -> a`, looks like our type synonym above.
We've wrapped a function in a datatype, giving it the accessor `runCR`. ^[Remember,
a `newtype` is the same as a `data` but with only one constructor with one field.]

Let's try it out in our API:

> initLogFileCR :: String -> CReader (IO Handle)
> initLogFileCR p = CReader $ \c -> initLogFile p c

> validateMessageCR :: String -> CReader (Either String ())
> validateMessageCR m = CReader $ \c -> validateMessage m c

Hmmm ... OK. Not sure how this is better yet. 

> validateAndInitLogCR :: String -> CReader (IO (Maybe Handle))
> validateAndInitLogCR msg = CReader $ \c -> 
>     case runCR (validateMessageCR msg) c of
>         Left err -> putStrLn "Invalid init message" >> return Nothing
>         Right () -> Just <$> runCR (initLogFileCR msg) c

This code is pretty awkward! Believe it or not, it's an important step forward in our
quest for a more elegant solution. 

We have a much stronger formalization: functions in `CReader`
*must return a function that takes a config*, wrapped in the `CReader`
constructor. Thus every function starts with the lambda `CReader $ \c ->`, followed
by the code to do stuff. 

Application is more unified too. To call another function of type `CReader`, we use the accessor 
`runCR` to get at the lambda function, and supply the config value `c` to it.

What isn't nice is all of the explicit wrapping and unwrapping we have
to do. Note also that the IO code in `initLogFileCR` no longer runs in that function, or
in `validateAndInitLogCR`. Instead, they return an IO "action" in pure code that would
be run after this has all been evaluated. Here's an 
example usage:

> runCRWithConfig :: AppConfig -> IO Handle
> runCRWithConfig config = do
>    let result = runCR (validateAndInitLogCR "Hello CR") config
>    -- IO action runs here
>    mh <- result
>    case mh of Nothing -> error "Log file init failed"
>               Just h -> return h

We'll see later how we can "mix" IO into our initialization code.

Formalization with Functor
--------------------------

You might have noticed that our newtype is of kind `* -> *`.

```haskell
ghci> :k CReader
CReader :: * -> *
```

This makes it potentially a candidate for implementing the famous
trio of Monad, Applicative and Functor! However, we should see if 
we need all this power. What does `Functor`, the humblest of the bunch, offer us?

```haskell
ghci> :i Functor
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
```

`fmap` allows us to execute a function "inside" of our Functor. 
That sounds like it could be nifty. Let's implement it.

> instance Functor CReader where
>     fmap f cr = CReader $ \c -> f (runCR cr c)

That looks an awful lot like our code above! We use `runCR`
to "unwrap" the product of some `CReader a` function, run `f` on
it, and "wrap" it back up with `CReader -> \c`. 

Let's try to swap out our boilerplate with Functor! Our functions
will still end with the `CReader ...` type, but we'll 
use `fmap` to stick our pure functionality into the newtype.

Only thing is, what are we "fmap-ing" *on*? `fmap` needs to
operate on some `CReader` value. Let's use "type holes" to let GHC tell us what we need:

```haskell
validateMessageF :: String -> CReader (Either String ())
validateMessageF m = fmap (validateMessage m) _
```

Note that `validateMessage m` is the same as `\c -> validateMessage m c`, written
point-free. This is the function we want inside of `CReader`. The underscore is a "type hole",
which gives
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
to return itself. (`id` is the same as writing `\c -> c`). We're ready to get all Functor-ific.

> validateMessageF :: String -> CReader (Either String ())
> validateMessageF m = fmap (validateMessage m) askConfig
>
> initLogFileF :: String -> CReader (IO Handle)
> initLogFileF p = fmap (initLogFile p) askConfig

NICE. This is starting to look pretty good. What about our function
that wants to use both?

> validateAndInitLogF1 :: String -> CReader (IO (Maybe Handle))
> validateAndInitLogF1 p = fmap doInit (validateMessageF p)
>     where doInit :: Either String () -> (IO (Maybe Handle))
>           doInit (Left err) = putStrLn ("Invalid prompt: " ++ p)
>                               >> return Nothing
>        -- doInit (Right ()) = ??? how do we call initLogFileF ???

Uh-oh. `fmap` can't help us here. In the second pattern-match on `doInit`,
we somehow need to call `initLogFileF`: *but we don't have the config anymore*.
"doInit" is a pure function that's supposed to run "inside" of the Functor,
implying that we would need to capture the config argument elsewhere.

We're essentially doing control-flow, and Functor isn't the right fit. 
We could instead fmap a function with `askConfig` that then manually
supplies config to `initLogFileF`, but that obviously
defeats our desire to formalize our computational environment.

Formalization with Monad
------------------------

We made some progress with our formalization but ran aground. Let's see 
what a Monad implementation buys us. (Yes, we skipped Applicative. See appendix
for an implementation.)

> instance Monad CReader where
>    -- return :: a -> CReader a
>    return = CReader . const 
>    -- >>= :: CReader a -> (a -> CReader b) -> CReader b
>    a >>= f = CReader $ \c -> runCR (f ((runCR a) c)) c

`return` is straightforward. Given an `a`, we simply want a `CReader` with
the `a` wrapped up: `return a = CReader $ \c -> a`. Point-free, this becomes `CReader . const`.

Bind is tricky. Here's an exploded version to help understand what's going 
on:

```haskell 
    -- >>= :: CReader a -> (a -> CReader b) -> CReader b
    a >>= f = CReader $ \c -> let a' = runCR a c
                                  f' = f a'
                              in runCR f' c  
```

The trick here is we write a *new lambda* that uses `runCR` with the lambda's `c` argument
to get at the "internals" of `a` (as `a'`) and `f'` (the results of applying `f` to `a'`).


We're in business. Let's roll.

> validateAndInitLogM :: String -> CReader (IO (Maybe Handle))
> validateAndInitLogM p = do
>    v <- validateMessageF p
>    case v of 
>      Left err -> return (putStrLn ("Invalid prompt: " ++ p)
>                        >> return Nothing)
>      Right () -> do
>         h <- initLogFileF p
>         return (fmap Just h)


Now we're cooking with gas. Note how our Functor-based functions work just fine in the monadic context.

We've built our ideal solution, true formalization of our
computational context as an "environment" offering `AppConfig` to read
from. It frees client code from passing variables and implementation
code from wrangling them.

As you've probably guessed, we've simply re-built the classic monad `Reader` --
`CReader` is the same as `Reader AppConfig` -- and it's main API function,
`ask`, which returns the environment value, as `askConfig`. `Reader`
is a true champ of effectful Haskell.

The only problem with `Reader` is ... it's nowhere to be found.

ReaderT and the case of the missing monad
=========================================

A funny thing happened on the way to modern Haskell: some classic
monads disappeared. In Reader's case, the closest we can find is a type synonym defining 
it in terms of `ReaderT Identity`. 

```
ghci> :i Reader
type Reader r = ReaderT r Identity
```

`Reader` is there, but it's pretty hard to understand from this definition.
Nonetheless, it works as-is, so we'll check it out briefly before diving into
the "real Reader", `ReaderT`.

Reader without the T
--------------------

Now that we know we like `Reader`, we can ditch our old code and write directly 
to the monad type.

> validateMsgRdr :: String -> Reader AppConfig (Either String ())
> validateMsgRdr msg = do
>   max <- reader maxMessageLength
>   if (length msg > max)
>     then return $ Left ("Message too long: " ++ msg)
>     else return $ Right ()

Instead of using `ask`, we used the function `reader`
which directly applies an accessor function to the retrieved config. The equivalent
code would `fmap` the accessor on the ask result: `max <- maxMessageLength <$> ask`.

With monadic `do` style, we can now magically make our config "appear" in the middle
of our Reader code, via `ask` and `reader`.

Reader and IO
-------------

Now we turn to rewriting our IO function. 

> initLogFileRdr :: String -> Reader AppConfig (IO Handle)
> initLogFileRdr preamble = do
>   f <- reader logfile
>   v <- reader version
>   return $ do
>     h <- openFile f WriteMode
>     hPutStrLn h (preamble ++ ", version: " ++ v)
>     return h

This looks sweet indeed. We're using `reader` as before, here to access
our log filepath and application version. 

However, it has the same problem we alluded to 
above: it's not running in `IO`, but instead returning an unevaluated
IO action (the second `do` section), which would be run sometime later. 
`Reader` is a monad, but
so is `IO`, and you can't simply run both "at the same time". 

Or can you? 

ReaderT: a monad transformer
============================

For `initLogFile`, our intention is to get all IO-rific and open the file right now. 
It's time to break out the `ReaderT` monad transformer.

> initLogFileRT :: String -> ReaderT AppConfig IO Handle
> initLogFileRT preamble = do
>   f <- reader logfile
>   v <- reader version
>   h <- liftIO $ openFile f WriteMode
>   liftIO $ hPutStrLn h (preamble ++ ", version: " ++ v)
>   return h

The `reader` calls are unchanged: we're still in Reader-land.

Our IO actions, however, require the
mysterious function `liftIO`. To understand this
we need to dig a little deeper into the type of `ReaderT`. 

Transformer Kinds
-----------------

`ReaderT` has a pretty scary kind signature:

```haskell
ghci> :k ReaderT
ReaderT :: * -> (* -> *) -> * -> *
```

Don't worry, it makes more sense as you add the necessary types.
Like `Reader`, the first "slot" is for the environment type itself, `AppConfig`.

```haskell
ghci> :k ReaderT AppConfig
ReaderT AppConfig :: (* -> *) -> * -> *
```

The second term is the parenthesized two stars, `(* -> *)`. This is
where we place our "stacked" monad. In this case, we want to use IO.

```haskell
ghci> :k ReaderT AppConfig IO
ReaderT AppConfig IO :: * -> *
```

And voila, we've arrived at a two-kinded, effectful type! We've "built
our own monad", simply by combining ReaderT and IO. As usual, the last slot is for whatever
value our functions produce: thus `ReaderT AppConfig IO Handle` in the example 
above.

All transformers will have a slot for `(* -> *)` where we can stick
another monadic type. Our stack above can thus be put in *another*
transformer, and on and on. In this way we can keep building new
behavior until we have the exact computational context we need.

Transformers and IO
-------------------

Like all monad transformers, `ReaderT` is purpose-built to be used with other monads.
Under the hood, this means that it explicitly supports the APIs of a
known set of monads, in order to "lift" their operations into the 
transformer's context. The transformer *author* is tasked with writing a fair
amount of boilerplate to guarantee this interoperation. The transformer
*user* on the other hand can mix and match the supported types as needed.

However, `IO` is not a transformer. It needs a little extra help.
To support IO, a transformer author must supply an instance of `MonadIO`, providing
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

A breeze, and a nice day for golfing, too. Recall that a pure function can
always be `fmap`'d into a monadic result -- so we put our pure validation
code in `vfun` and attach it with `<$>`.

Looking pretty good now. We can use `initLogFileRT` and `validateMsgRT`
in the same "stack": `ReaderT AppConfig IO`. 

The only
problem is the future. What if we decide later to add another transformer into our stack?
We'd have to change this already quite long type to `ReaderT AppConfig (FooT Bar (IO ...))`
anywhere it appears. 

But even in the here and now, it's kind of unfortunate we've bound
`validateMsgRT` to `IO`.  There's no IO going on in the function after
all, so it seems a shame to force any calling code to run in IO.

Polymorphic ReaderT
-------------------

A simple solution is to use a type variable in the two-kinded "slot"
of `ReaderT`. We can't have any old type in there though. We'll need
to constrain it to `Monad`, and because we like `<$>`, `Functor` too. ^[In GHC 7.10, `Monad` is
constrained to `Applicative` and `Applicative` to `Functor`, so constraining to
both `Monad` and `Functor` isn't necessary.]

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

MonadReader: a transformer typeclass
===================================

What we want to do instead is *constrain* our function's type to a *monadic typeclass*,
instead of explicitly specifying the whole type.

The typeclass `MonadReader` enumerates all of the functionality in `ReaderT`,
but because its a typeclass, it can be *implemented* by any transformer that
has `ReaderT`, `RWST`, or what have you. 

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

(Note the freakiness of that last instance: *the
function arrow itself* is an instance of MonadReader. ^[This reaches its apex
of generality in the Lens library: `view` slots right into a `MonadReader` stack, and the `makeClassy` template haskell generates "HasXXX" typeclasses you can use to constrain the `r` type of your MonadReader constraint.])

Here we see the complete API of `ReaderT`: 

* `ask`: get the config, like our `askConfig` for `CReader`

* `local`, a more esoteric use case where
you want to run the `m a` argument in an environment "modified" by the `(r -> r)` argument

* our good friend `reader`: apply a pure function to the environment.

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

We can extend this polymorphic concept to our IO function too. 
Of course, this means that we'll want to *additionally constrain* the
monadic argument to `MonadIO`.

We saw `MonadIO` above with `liftIO`. There, it was allowing `ReaderT`,
an instance of `MonadIO`, to lift IO operations into its context. Now
we're going to use the typeclass itself to support any stack built off IO.

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

We've focused on how to write library functions to make use of `ReaderT`.
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

and `readConfig` will turn it into an `AppConfig`. It composes the
pure function `read` (which constructs a type value from a String
value) with our local function `fromTup`. Type inference figures out
what `read` is inflating; the reader is encouraged to figure it out too.

With that, we can write the rest of our little application:
a `main` function to fire up our monad stack; a `go` function to run inside of it;
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
>   forever $ do
>      liftIO $ putStr $ "Your message: "
>      m <- liftIO $ getLine
>      v <- validateMessageMR m
>      case v of
>             (Right ()) -> logMsg h $ "Valid Input" 
>             (Left err) -> logMsg h $ "Invalid input: " ++ err
>
> logMsg :: (MonadIO m) => Handle -> String -> m ()
> logMsg h = liftIO . hPutStrLn h 


runReaderT
----------

The `main` function loads our config file in order to launch the monad
"stack."  `getArgs` obtains the array of command-line arguments, and
applies the pure function `head` to get the first one; we pass this to
`readConfig` to get an `AppConfig` value.

With config, we're ready to fire it up with `runReaderT`.

```haskell
ghci> :t runReaderT
runReaderT :: ReaderT r m a -> r -> m a
```

`runReaderT` is very similar to `runCR` for `CReader`. It's an accessor to 
the function powering the Reader magic. However, here, instead of returning
a simple value, it's returning whatever monad is running "outside" of ReaderT.

```haskell
ghci> :i ReaderT
newtype ReaderT r (m :: * -> *) a
  = ReaderT {runReaderT :: r -> m a}
```

It's a confusing concept that "using an accessor function" like `runReaderT` would somehow
construct our monad. Calling `runReaderT go config` makes sense when you 
consider that `go` is itself a monadic value, in this case a function that wants
us to apply the config environment to it. The accessor runs the entire ReaderT stack
and "pulls out" it's result, which in this case is an IO action.

However, since `go` is in fact a polymorphic type, `runReaderT` also "picks the
type" of `go` for this use case. This, plus the fact we're running in IO (in `main`),
fixes the type of `go` to `ReaderT AppConfig IO ()`.

Indeed, we could call `go` elsewhere with a different monad stack:

> goState :: IO ()
> goState = evalStateT (runReaderT go emptyConfig) "" 
>     where emptyConfig = AppConfig "" "" (0 :: Int)

Here our code picks the type `ReaderT AppConfig (StateT String IO) ()`, proving
that `go` is indeed polymorphic.

In `go`, we loop forever, reading input, and logging validation results on it.

```
ghci> :main "etc/effectful02/config.txt"
Your message: Hello
Your message: sdlafjhaslkfjahsflkjasdflkjsdahf
Your message: ^C^C Interrupted.
ghci> readFile "/tmp/logfile" >>= putStrLn
Starting, version: 1.0.0
Valid Input
Invalid input: Message too long: sdlafjhaslkfjahsflkjasdflkjsdahf
```
 
Conclusion
==========

We started with a use case and ended up with a transformer stack. Going forward,
you'll want to experiment with all of the "greatest hits" of monad transformers,
namely `StateT`, but also `MonadError` (to encode your error-throwing logic, instead
of blindly throwing IO errors everywhere), and other nifty things out there 
like logger transformers.

Libraries like the Snap web framework also offer transformers (MonadSnap) to
allow you to factor your code for whether it needs to interoperate with serving
web requests.

Hopefully this also makes implementing Functor and Monad (and Applicative too, see
appendix) less scary. It's not something you have to do every day but it's good
to be familiar with the wrapping and unwrapping that goes on in these implementations.

But most importantly, use `ReaderT`! It's stupidly useful, and combined with some
of the concepts from the `lens` library, can be amazingly flexible.


Appendix: Applicative CReader
=============================

The article doesn't have a good use case for Applicative, but since
GHC 7.10 will require one, let's go ahead and implement it to try it out.

Applicative allows for "effectful function application", with `<*>` standing
in for whitespace-between-arguments, and `<$>` standing in for `$`.
For instance, `mod 7 4` can be performed on list singletons (since lists are
Applicative) as `mod <$> [7] <*> [4]`.

```haskell
ghci> mod <$> [7] <*> [4]
[3]
```

We implement the two essential class methods: `pure`, which lifts bare values into the Applicative context, generally identical to `return` for Monad; and `<*>`, which is just like `fmap` except the function argument is *itself* in the Applicative context.

```haskell
ghci> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b
ghci> :t (<*>)
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
ghci> :t pure
pure :: Applicative f => a -> f a
```

> instance Applicative CReader where
>     pure = return
>     (CReader f) <*> (CReader a) = CReader $ \c -> (f c) (a c)

`pure` is self-explanatory. For `<*>`, much like the `bind` implementation above, we simply apply the `c` from the outer lambda to the
contained function argument `f` and applied value `a`, and wrap the result of applying one to the other into the new `CReader` value.

A contrived usage is to trim the app version String to the max message length.

> trimVersion :: CReader String
> trimVersion = take <$> fmap maxMessageLength askConfig 
>                    <*> fmap version askConfig

```haskell
ghci> runCR trimVersion (AppConfig "logfile" "Version 1.0 build 323451" 11)
"Version 1.0"
```

