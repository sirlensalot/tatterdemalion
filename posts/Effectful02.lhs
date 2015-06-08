----------
title: Effectful Haskell: Reader, State, Transformers
author: Stuart Popejoy
date: 2015-05-24
----------

This article is the second in a series on "effectful" Haskell.  In the
(last one)[Effectful01.html] we looked at how to work with `IO` as a
`Monad` and a `Functor`, using bind, `return`, `fmap`, and do
notation. We also briefly examined the list type as a monad.

The types we'll be examining in this article -- Reader and State --
are different. They're fully pure types built from "normal" Haskell
code, requiring no magic from the compiler like `IO`. Yet they offer
behavior that, in other languages, would be the province of side-effects
and mutability. They do so by implementing the "effectful trio" of
Functor/Applicative/Monad.

As nice as these types are, they're even better used together, and with
IO. To accomplish this, we need *monad transformers* to "stack" them
together. On the other hand, it's nice to "target" functions at the exact
effectful behavior we need, instead of using the entire "stack type", so
we'll want use the particular typeclasses offered (MonadReader, MonadState)
to keep things tidy. 

If this all sounds too complicated, just remember that "it's all in the types":
the payoff is tightly-specified application behavior with "no surprises" that
is just as powerful as any procedural, side-effect--ridden code you'll find. 

> {-# LANGUAGE FlexibleContexts #-}
> import System.IO
> import Control.Monad.Reader
> import Control.Monad.Trans.Reader (Reader)
> import Control.Applicative


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
>   hPutStrLn handle (preamble ++ "\nVersion: " ++ version config)
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

No problem with this, except it's a little ... "manual". Every time we need the config
value, we're adding it to the argument list ad-hoc. It would be nice to
*formalize* this, as a way of declaring that these functions share some environment,
and maybe to get `AppConfig` out of the argument list.

An initial approach would be to use a type synonym.

> type ConfigReader a = AppConfig -> a

> initLogFileTS :: String -> ConfigReader (IO Handle)
> initLogFileTS = initLogFile
> 
> validateMessageTS :: String -> ConfigReader (Either String ())
> validateMessageTS = validateMessage

As the equations prove, we can substitute with our type synonym `ConfigReader`
and the code still works. We achieve a light formalization and get the explicit
argument out of the signature. 

It's just for show though. The arguments are
still there in the original code. Worse, we have to explictly thread the `AppConfig`
value through any code calling these functions, and stick it in the right position. 

Our ideal solution frees client code from passing variables, and
implementation code from wrangling them, formalizing our computational
context as an "environment" with `AppConfig` available to read from.
The magical type that makes this happen is called `Reader`.

Well, `ReaderT` actually.

ReaderT and the case of the missing monad
=========================================

A funny thing happened on the way to modern Haskell: some classic
monads disappeared. 

`Reader` is one such classic. It's type is `(-> r)`: it literally
formalizes an *extra argument* into a datatype. It's exactly what we
need.

It's also nowhere to be found. The closest we can find is a type synonym defining 
it in terms of `ReaderT Identity`. 

```
ghci> :i Reader
type Reader r = ReaderT r Identity
```

It turns out that `Reader` is just a specialization of the more
general type `ReaderT`, a *monad transformer* that can be
composed with other effectful types. Before we dive into that,
we'll stay with the type synonym `Reader` to see how far it gets us.

Reader without the T
--------------------

In pure code, `Reader` gives us exactly what we were looking for:
functions that can access our config value without explicitly
including it in our argument list. Let's adapt our function
`validateMessage` to use Reader:

> validateMsgRdr :: String -> Reader AppConfig (Either String ())
> validateMsgRdr msg = do
>   max <- reader maxMessageLength
>   if (length msg > max)
>     then return $ Left ("Message too long: " ++ msg)
>     else return $ Right ()

This is looking pretty cool. No `config` argument to be found, just
a mysterious function `reader` which seems to get our `maxMessageLength`
value from the ether. 

The key is the type. `Reader`'s kind is three-star:

```haskell
ghci> :k Reader
Reader :: * -> * -> *
```

We populate the first star with `AppConfig`, which specifies the type
of value we will want our environment to provide. Types starting with
`Reader AppConfig` or `ReaderT AppConfig` indicate functions that
participate in this "configuration environment".

The second star is where our types indicate what a particular
function's result is. Here, it's `Either String ()`, our validation
result; a function of type `ReaderT AppConfig Int` would indicate that
an `Int` value will be computed.

Reader and IO
-------------

We get into trouble if we want to use `Reader` on a function that
is already in some other monadic type. `initLogFile`, our other
example function, is of type `IO Handle`. Now what? 

We could try cramming `IO Handle` into the second term of `Reader`.
Here's a simplified function that just tries to open the log file:

> openLogFileWeird :: Reader AppConfig (IO Handle)
> openLogFileWeird = do
>   f <- reader logfile
>   return (openFile f WriteMode)

This typechecks, but doesn't cut the mustard. The IO code returned in
the last line isn't evaluated, but instead returned "un-fired" 
to the calling function, which would have to bind to this result 
separately to get the handle. Our `initLogFile` function above
actually *opened the file* so we could get on with our logging business
in subsequent code.

Doing the converse -- ie, a type like `IO (Reader AppConfig Handle)` --
is simply broken: we'd be returning a pure "Reader action" that isn't 
itself in IO, so it can't open file handles or anything like that. 

Enough nonsense: this is what monad transformers are for, composing 
monads *together* so we can use them at the same time.

ReaderT: a monad transformer
============================





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
