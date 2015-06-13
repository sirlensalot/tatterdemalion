----------
title: Effectful Haskell: Reader, State, Transformers
author: Stuart Popejoy
date: 2015-05-24
----------

This article is the second in a series on "effectful" Haskell.  In the
[last one](Effectful01.html) we looked at how to work with `IO` as a
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
including it in our argument list. 

Here's the pure function `validateMessage` adapted with Reader:

> validateMsgRdr :: String -> Reader AppConfig (Either String ())
> validateMsgRdr msg = do
>   max <- reader maxMessageLength
>   if (length msg > max)
>     then return $ Left ("Message too long: " ++ msg)
>     else return $ Right ()

This is looking pretty cool:

- The type specifies our context: we "read" from an environment having
an `AppConfig`; our particular case returns an `Either String ()` value.
Reader is *three-kinded* (of kind `* -> * -> *`), with the first slot being
the environment type, and the second slot being the typical monadic "result
type".

- There's no pesky `config` argument. We'll see later that functions
*inside of* ReaderT can call other ReaderT functions with no argument. 
Functions *outside* of the reader context will have to provide the AppConfig
value, of course.

- Magically, the `reader` function appears in our scope to obtain
`maxMessageLength` from the AppConfig environment.

Reader and IO
-------------

Clearly our next goal is to apply Reader to our monadic function. 
However, we'll get into trouble if we use plain `Reader` on a 
function that's already a monadic type.

Sticking `IO Handle` into the second term of `Reader` doesn't work.

> openLogFileWeird :: Reader AppConfig (IO Handle)
> openLogFileWeird = do
>   f <- reader logfile
>   return (openFile f WriteMode)

This typechecks, but doesn't do what we want: it returns an *unevaluated
IO action* -- `(openFile f WriteMode)` -- which defeats the whole purpose
of running in IO in the first place: we want IO actions to fire "in place".
Instead we'd be returning a "command" that the calling code would have to
separately call later.

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
>   liftIO $ hPutStrLn h (preamble ++ "\nVersion: " ++ v)
>   return h

Nice. Again, our type signature denotes our exact
context: computing in IO with an `AppConfig` environment to read
from. Our function produces a `Handle` value, and no "config"
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

To use IO, a transformer must be an instance of `MonadIO`, in order to provide
an implementation of `liftIO`. This allows the transformer to "lift" the
results of an IO action into the transformer's context.

On the user side, we have to sprinkle these `liftIO` calls whenever we want
to use IO. 

Reader vs ReaderT
=================










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
