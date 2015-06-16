----------
title: Effectful Haskell: Reader, Transformers, Typeclasses
author: Stuart Popejoy
date: 2015-05-24
----------

This article is the second in a series on "effectful" Haskell.  In the
[last one](Effectful01.html) we looked at how to work with `IO` as a
`Monad` and a `Functor`, using bind, `return`, `fmap`, and do
notation. We also briefly examined the list type as a monad.

The type we'll be examining in this article, `ReaderT`,
is fully pure, built from "normal" Haskell
code, requiring no magic from the compiler like `IO`. It 
nonetheless does a pretty nifty trick that, in other languages,
would require global variables, or at least a dependency-injection
framework.

Nonetheless, we'll still want to use it with `IO` and other effectful
types, so we'll examine how *monad transformers* allow us to "stack" 
these types into the exact behavior we want. But we'll also allow
our functions to polymorphically target the exact subset of behavior
they need, by using the *typeclasses* associated with these types:
`MonadReader` and `MonadIO`.

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
The type that makes this happen is called `Reader`.

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

Clearly our next goal is to apply Reader to our function in `IO`. 
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
explicitly "fire" later.

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

In fact, `ReaderT` isn't the only Reader. There's also `RWST`, a
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

Our library is now maximally polymorphic.

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
