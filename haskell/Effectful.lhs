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

At the same time, you'll want to take advantage of Reader, Writer, State, Maybe,
Either, and fancy libraries offering webservers, http clients, loggers, database
access, and such. Often you'll be using many of these at the same time.

This article will offer a breadcrumb trail to follow in pursuit of
"effectful" programming. To me, "effectful" captures the notions of

1. Actual side-effects (IO)
2. Stuff that seems like side-effects (e.g. State, Writer)
3. A "computational environment" that persists through various function calls (e.g. Reader)
4. Non-local control flow (Maybe, Either). 

This means using types that implement the `Functor` ==> `Applicative`
==> `Monad` typeclass hierarchy, and managing the complexity that can
arise when you use all of these things together.


> import System.IO
> import Control.Monad.Reader
> import Data.Set (Set,singleton)

It all starts with IO
=====================

To write an app, you implement `main :: IO ()`.

> main :: IO ()
> main = putStrLn "Hello World!"

`main` is the gateway drug into `IO`, and the first one's free: "Hello World"
is a one-liner, requiring no imports, and not even a type signature 
if you're really lazy.

Functions with the famous type signature `IO ()` have zero functional mojo.
`()` is the empty type, meaning the function return nothing:
you run them for side-effects only. Pathological code can abuse `IO` to
squander any benefits of strongly-typed functional programming.

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

But "pure", or at least non-effectful, functions have their limits.
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
from those other languages, which can inhabit just about anything:
`()` can never have a value except itself. Thus, `IO ()` is all about
side effects, like `main` or `putStrLn` -- we ignore the empty result.

Most operation with `IO` exploits the fact that it is indeed a `Monad`,
which is why we often see `do` notation in IO functions: `do` is
syntactic sugar designed to make coding with Monads easier. 

Monad and do notation
=====================

`Monad` is a typeclass, meaning any instance of `Monad` will support a number of functions:

```
ghci> :i Monad
class Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  fail :: String -> m a
```

You'll note that `Monad` requires a two-star--kinded type (`* -> *`), so all monads
will have *at least* one extra type parameter. This is also true of `Functor` 
and `Applicative`: all three of these typeclasses expect a two-star kind. 
The "AMP" proposal is coming to a GHC near you soon, enforcing the super-class
hierarchy Functor => Applicative => Monad, so we'll keep referencing the trio
of types often.

You only have to implement two of these functions to be a monad, since the others
are defined in terms of them: `(>>=)` (called "bind"), and `return`. Under AMP, 
you only have to implement "bind" because "return" will come with Applicative,
as "pure". But let's stay focused here: `(>>=)` and `return` are the characteristic
monad functions.

`return` is pretty easy to use, it allows you to lift a value into the monadic 
context.

```
ghci> :t return
return :: Monad m => a -> m a
```

> returnIsEasy :: String -> IO String
> returnIsEasy s = return (s ++ " is in IOooooooo")

You get "return" for free from calling any function with the same underlying type.

> ioNoReturn :: IO String
> ioNoReturn = returnIsEasy "Elvis"

Bind is the real magic of `Monad`, however. Bind is the fundamental operation
of a Monad, by allowing functions to transform values from one underlying type
to another, without ever leaving the monadic context.

```
ghci> :t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

Instead of just grabbing the value via an accessor function and going
to town, "bind" forces you to supply a function `(a -> m b)` to
operate on the value of type `a` and `return` it back to the monadic
context. 

This inversion of control allows the Monad instance to enforce all kinds
of invariants on what that computation is allowed to do: you don't simply 
"call" a monadic function, you "bind" to it, with a function that accepts
the value and "puts it back". 

It's a lot simpler to understand the utility of Monad when we get to the
nifty types like `State` and `Reader` and `Maybe`, where it will become
obvious how `Monad` (and Applicative and Functor) turns pure functional
code into wonderously imperative-looking powerhouses. 

But `IO` on the other hand is pure mystery; Monad is how GHC "labels"
impure IO code, and it's how the runtime can safely move values into
and out of the side-effectful "real world".

Binded by the light
===================

Coding with bind isn't too bad necessarily.

> countCharInFileBind :: Char -> FilePath -> IO Int
> countCharInFileBind c f = 
>   readFile f >>= 
>       \contents -> return (countChar c contents)

Bind is an infix function, so we're taking "readFile f", of type
`IO String`, and binding to get at the contents. Our lambda function
then returns our desired result. 

In fact, bind can make for some beautiful compositions:

> countCharInFileEta :: Char -> FilePath -> IO Int
> countCharInFileEta c f = 
>     readFile f >>= return . countChar c

You can almost see a "pipeline" of `readFile f` flowing into `countChar c`!

Side-effect--only code, where the monadic result is unimportant, is
easily inserted using `>>`.

```
ghci> :t (>>)
(>>) :: Monad m => m a -> m b -> m b
```

> countCharPutStr :: Char -> FilePath -> IO Int
> countCharPutStr c f = 
>    putStrLn ("counting " ++ [c] ++ " in " ++ f) >>
>        readFile f >>= return . countChar c

Assigning variables is a little tricky. `where` gets funny with all of 
the lambda scopes; `let` composes better with the flow of "bind".

> countCharLog :: Char -> FilePath -> IO Int
> countCharLog c f = 
>    readFile f >>= 
>        \contents -> let cc = countChar c contents 
>                     in putStrLn ("Counted " ++ show cc ++ 
>                                  " of " ++ [c] ++ " in " ++ f) >>
>                     return cc

It can get ugly though when you bind up a storm.

Do notation
===========

`do` notation offers nice syntax to "clean up" repeated `>>=`, `>>`
and `let` expressions.

> countCharLogDo :: Char -> FilePath -> IO Int
> countCharLogDo c f = do
>   contents <- readFile f
>   let cc = countChar c contents
>   putStrLn ("Counted " ++ show cc ++ " of " ++ [c] ++ " in " ++ f)
>   return cc



TODO
====


`setProducingAction` shows
how easy it is to lift a pure computation into `IO`: just `return` it.
Bind, however, can get tricky sometimes.

~~~
ghci> :t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
~~~

Generally speaking, "bind" is the only way to operate on a monadic value.
Instead of just grabbing the value via an accessor or something, you 
must use a function `(a -> m b)` to operate on the value and `return` it back
to the monadic context. This allows the Monad instance to enforce all kinds
of invariants on what that computation is allowed to do: you don't simply 
"call" a monadic function, you bind to it with a function that is provided
with the results.

But, code using bind can get a little unweildy. Here's the "desugared"
version of `initLogFileNaive` above:

> initLogFileBind :: AppConfig -> IO Handle 
> initLogFileBind config = 
>     openFile (logfile config) WriteMode >>= 
>         (\handle -> hPutStrLn handle ("Version: " ++ version config) >> 
>             return handle)

`openFile` returns `IO Handle`; to access it, we provide a lambda
function with `handle` as the argument, and use it to write to stdout.
Our last task is to return the handle, so we have to bind to
`hPutStrLn` to return the handle "after" the write occurs. We don't care
about the output of `hPutStrLn` so we use `>>`, a version of bind that
ignores the bound value.

The `do` version above is more legible. 

```haskell
   do
     handle <- openFile (logfile config) WriteMode
     hPutStrLn handle ("Version: " ++ version config)
     return handle
```

Just remember that `do` is syntactic magic. It's important to 
be aware how the bind operations are woven together behind it.


Finally, we also want to look at how we can change this "computational
environment" within functions and without. 





  We can also drop into other monadic or applicative
expressions within IO.  IO, being "enhance" IO with additional
effectful functionality. Mainly this is accomplished with *monad
transformers*,


  



A number of nice modules in

What we're interested in here is using




If you're coming to Haskell looking for a city on a hill where all functions
are referentially transparent, all values are immutable and all computations
are lazy, IO can seem like the slums, and `do` notation an incomprehensible
approximation of imperative programming.

The truth is considerably subtler. 

First, Haskell is a far more permissive
language than it seems, offering power only rivalled by C++: you can manipulate
pointers if you really want to. 

; if you master "effectful" programming,
you can blur the boundaries of code that's "in IO" and code that isn't. 

The good news is, you can always 
refactor computations into pure functions whenever you want, and you should, 
often. This should happen anyway as a "separation of concerns" issue;
you should see pure functions popping up all over the place if your factoring
is halfway decent.




But nothing's stopping you 

And indeed, pure functions are terrific



`IO` is generally all about side-effects, so there's clearly a tension 
between IO 

This is of course "effectful", in a big way. `IO` is one of a small
number of truly non-pure types; any function of type `IO a` is one
where stuff "just happens".

In a pure function, the inputs determine the outputs, which is a great
thing. With purity, immutability, and a great type system, Haskell
enables writing highly safe and stable code. It allows us to lift 
invariants and contracts into our program's type signature, so that
the compiler can prevent truckloads of bugs and other unwelcome runtime surprises. 

With IO, we can subvert all of this. 

--> sendSecret :: IO ()
--> sendSecret = writeFile "/tmp/secret" "Who is Benjamin Disraeli?"
-->
--> andTheAnswerIs :: IO String
--> andTheAnswerIs = readFile "/tmp/secret"

The function types won't help us understand what's going on, and thus
the compiler won't prevent any bugs. But we gotta fire those missles,
so Haskell's approach to safe-ish IO is to prevent us from ever creating
an `IO` instance. `IO` is magically born in `main` and can only be used
in a call stack coming from `main`. 

There's nothing stopping us from using pure functions within IO, and 
of course we should whenever possible, if for no other reason than
simple "separation of concerns". We don't need IO to validate a string,
or perform linear regression, or transform a `Map` into a `Set`, so 
if there's no IO, then don't do it in `IO`. Simple.

However, we're writing an application, and that usually means that we
will want to do IO here and there, and probably not in `main` unless we're
addicted to spaghetti. So we'll have a decent number of functions in `IO`.

Let's look at how we interweave other effects in and out of `IO`.

Use Case: Configuration
=======================

So, we've got some config. We've read it in somehow in `main` and turned into
a config data structure:

> data AppConfig = AppConfig {
>     logfile :: FilePath
>   , version :: String
>   , hostname :: String 
>   , port :: Int
> } deriving (Show)

We have a function in `IO` that wants to read said config. The naive way to
handle this is pass in the config as an argument.

> initLogFileNaive :: AppConfig -> IO Handle 
> initLogFileNaive config = do
>   handle <- openFile (logfile config) WriteMode
>   hPutStrLn handle ("Version: " ++ version config)
>   return handle

This is fine for one or two functions, but it doesn't scale well. If
we're 5 calls deep in our call stack and decide we need config we'll
have to add an `AppConfig` argument to every function signature. What
we'd rather do is model our computations as having a *read-only
environment* they can access at any time.

But we also need to do this in IO. To do so we need to understand a little
more about its type.


ReaderT
=======

This article is all about composing effects, with IO being a big one.
How are we to go about composing this idea of a read-only environment
into IO? Simple: use `ReaderT`, a *monad transformer*. 

> initLogFileReaderT :: ReaderT AppConfig IO Handle
> initLogFileReaderT = do
>   f <- reader logfile
>   v <- reader version
>   handle <- liftIO $ openFile f WriteMode
>   liftIO $ hPutStrLn handle ("Version: " ++ v)
>   return handle




A monad transformer allows us to "decorate" one monad with another,

So how do we make `AppConfig` available to our IO-having functions? 
We use `Reader`, a type that models having an "extra argument" passed
to our function. The intent is to provide a read-only context, or environment,
for our computations. 

```
ghci> :i ReaderT
type role ReaderT representational representational nominal
newtype ReaderT r (m :: * -> *) a
  = ReaderT {runReaderT :: r -> m a}
ghci> :k ReaderT
ReaderT :: * -> (* -> *) -> * -> *
```


or more specifically, `ReaderT`, a *monad transformer*
which will allow us to "decorate" IO with a read-only environment
variable.


Note that we had to add `liftIO` to our invocations of `openFile` and `hPutStrLn`.
Monad transformers "wrap" other monads such that we can "lift" the wrapped
monads into our context. If we were to forget one of those "liftIO" calls, we'd
get a potentially scary-looking error:

```
Effectful.lhs:106:5-39: Couldn't match type ‘IO’ with ‘ReaderT AppConfig IO’ …
    Expected type: ReaderT AppConfig IO ()
      Actual type: IO ()
    In a stmt of a 'do' block: hPutStrLn handle ("Version: " ++ v)
    In the expression:
      do { f <- reader logfile;
           v <- reader version;
           handle <- liftIO $ openFile f WriteMode;
           hPutStrLn handle ("Version: " ++ v);
           .... }
Compilation failed.
```





Let's look at a simple use case: we want to read configuration in 




The upshot is 

Of course, we can, and should, call pure functions from IO.

  an IO
call. All IO initiates in `main`: any function having `IO` in its type
can only execute in a call stack starting with `main`.


The upshot is, we like Haskell, but we want to get stuff done, which means
we'll probably have quite a bit of code running "in IO", but we'll only
use IO functions for genuine missle-firing goodness. 






The big restriction on IO functions is with the restriction that any function running "in IO" can only
do so if it's in a call stack that originated in `main`. 

The `IO` type 

This doesn't mean we want to unnecessarily avoid doing IO. Instead,
we look at a function with `IO` in the type as a clue to how that function 
fits into the larger program.






Why do we like pure functions in the first place? Because we can get the
compiler into our application logic, enforcing invariants, ensuring against
run-time errors, and providing correctness and safety in the large.
Functions  


Configuration
=============

A common requirement of many applications is the need to reference
configuration in some kind of data structure. 
function calls.




 This article seeks to get you going
with basic concepts and recipes. It may be a decent introduction 
to functors, applicative functors and monads too.



you're almost guaranteed to 
there's a decent chance you'll want to use more monads, if the program has any state at all, takes
any kind of configuration, wants to 
something with a `main` method that actually does stuff --
chances are you'll be wanting to not just "fire the missles", but also

Application programming in Haskell almost inescapably involves combining
monadic 

Typeclasses vs Types
====================

