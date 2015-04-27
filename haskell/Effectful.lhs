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
access, and such. Usually, you'll be using many of these at the same time.

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
`()` is the empty type, meaning the function returns nothing:
you run it for side-effects only. Pathological code can abuse `IO` to
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

Most operation with `IO` exploits the fact that it is indeed a `Monad`,
which is why we often see `do` notation in IO functions: `do` is
syntactic sugar designed to make coding with Monads easier. 

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

(Note also the `* -> *` kind constraint. All monads are like 
IO in that they require *at least* one other type).

Until recently, it was convention (and sanity) to also support
`Applicative` and `Functor`; indeed, `pure` from `Applicative` is
identical to `return`.

```
ghci> :t pure
pure :: Applicative f => a -> f a
````

With GHC 7.10, the Functor => Applicative => Monad hierarchy is finally
enforced.

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

Bind is the real magic of `Monad`, however. It is Monad's fundamental
operation, allowing functions to transform monadic values from one 
type to another, without ever leaving the monadic context.

```
ghci> :t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

Instead of just grabbing the value via an accessor function and going
to town, "bind" requires you to supply a function `(a -> m b)` to
obtain the value as an argument, after which you must `return` it
(or some transformation of it) back to the monadic context.

This inversion of control allows the Monad instance to enforce all kinds
of invariants on what that computation is allowed to do. You don't simply 
"call" a monadic function, you "bind" to it, with a function that accepts
the value and "puts it back". 

It's easier to grasp the utility of Monad when we get to nifty types
like `State` and `Reader` and `Maybe`, that turn pure code into
effectful powerhouses. `IO` is more mysterious, in that we're not
using IO to "gain" any functionality. The compiler relies on the
monadic context to clearly "demarcate" the IO code. This is how it
guarantees proper sequencing, and how it can safely move values into
and out of the pure runtime.

Binded by the light
===================

Working with bind isn't hard once you get used to it.

> countCharInFileBind :: Char -> FilePath -> IO Int
> countCharInFileBind c f = 
>   readFile f >>= 
>       \contents -> return (countChar c contents)

Bind is an infix function, so we're taking "readFile f", of type
`IO String`, and binding to get at the `contents`. Our lambda function
uses its argument `contents` in a pure context, calling `countChar`; 
it then `return`s the result back to IO. 

With eta reduction, bind can make for some beautiful compositions:

> countCharInFileEta :: Char -> FilePath -> IO Int
> countCharInFileEta c f = readFile f >>= return . countChar c

You can almost visualize a "pipeline" of the file contents "flowing" into
`countChar` with `>>=`.

As we noted above, functions of type `IO ()` are run just for side effects,
like `putStrLn`. Using normal bind, we'd have an argument capturing
the useless `()` non-value. Instead, we can use `>>` to simply "sequence"
the next operation.

```
ghci> :t (>>)
(>>) :: Monad m => m a -> m b -> m b
```

> countCharMsg :: Char -> FilePath -> IO Int
> countCharMsg c f = 
>    putStrLn ("counting " ++ [c] ++ " in " ++ f) >>
>        readFile f >>= return . countChar c

 
In pure code we create local definitions using `let`/`in` or `where`. In 
monadic code it gets a little tricky with all of the closures
we're creating. Putting `where` at the end of the function is often not
what you want to do, since you can't reference any variables like `contents`
scoped into the inner closures.

`let`/`in` works better, by putting the definition "above" the closures that need
it's result but "below" the bindings it needs.

> countCharLog :: Char -> FilePath -> IO Int
> countCharLog c f = 
>    readFile f >>= \contents -> 
>        let cc = countChar c contents 
>        in putStrLn ("Counted " ++ show cc ++ " of " ++ [c] ++ " in " ++ f) 
>           >> return cc

Here we've used `let` to capture the results of the pure call in order to 
log it with `putStrLn`, before `return` sends it back to the monadic context.

Do notation
===========

Eventually, binding can get unweildy and confusing with lambda after lambda.
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

Do notation is often seen as "a way to write imperative-style code in
Haskell", ostensibly "easier to read" than, say, the right-to-left
sequencing of function composition. I think it's better to see
it as a code-cleaning tool, a way of chaining binds of monadic
functions and lambdas, oriented toward accumulating variables
in scope.

Using "reverse bind" `=<<`, you can write bind code with the same "orientation" as
function composition.

> countCharReverseBind :: Char -> FilePath -> IO Int
> countCharReverseBind c f = return . countChar c =<< readFile f

Unfortunately, it doesn't compose as nicely, so eta-reduction gets
clumsy. 

> countCharReverseBindEta :: Char -> FilePath -> IO Int
> countCharReverseBindEta c = (return . countChar c =<<) . readFile

In the end, "forward bind" works with infix style better, and powers
the imperative-like flow of do notation.

So, do notation is cool, but you should regularly "de-sugar" `do` blocks
into `>>=`, `>>`, `let` and lambdas to stay on top of the syntactic magic.

IO is a Functor 
===============

The other reason not to immediately call `IO` a `Monad` is that it's also
a `Functor` (and `Applicative`) instance. Which, again, all Monads should be,
and shall be so, with GHC 7.10 and onwards.

`Functor` is, like Monad, another typeclass of kind `* -> *`. It's canonical
function is `fmap`.

```
ghci> :i Functor
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
```

`fmap` is basically a pure version of Monad's "reverse bind", `=<<`:

```
ghci> :i (=<<)
(=<<) :: Monad m => (a -> m b) -> m a -> m b
```

Instead of having to `return` values back into the context within our
transformation function, `fmap` just lets us run the pure transformation
"inside" the context. 

The classic operation `map` from every functional-ish language in
existence is `fmap` for lists. (The only reason Functor calls it 
`fmap` is to stay out of the way of `map`, which predates Functor in Haskell by
decades).

> incrementAllBy :: Int -> [Int] -> [Int]
> incrementAllBy i is = fmap (+ i) is 

This maps the function `(+ i)` -- Haskell allows us to
create simple lambdas out of infix expressions like `+` -- over every
element of the list `is`. Many tourists to functional programming
learn `map` (and maybe its cousin `reduce`/`fold`) as an operation applied
to lists or maps only, and stamp FP onto their resume. Haskellers go a lot
further with Functor.

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
`Int`, *within the effectful context:* no `return` required.

List is a Monad
===============

We've talked a lot about `IO` so far, which is strongly associated with 
Monad, making Monad be strongly associated with effectful code. Let's 
break away from that association though by looking at a very straigtforward
type which is also a Monad, the list type: `[a]`.

We saw above how `fmap` works on lists, and indeed the `map` operation
on a list is a "Greatest Hit" of functional programming. It's easy to
understand how a function could be applied one-at-a-time to a list's
values, with `fmap` creating a new list from the results. 

So we understand how list is a `Functor`. How is it a `Monad` though?

`return` is pretty easy to understand, it simply creates a singleton list
out of the value. 

```
ghci> return 1 :: [Int]
[1]
```

Bind is more interesting. Recall that bind takes a function which accepts the
monadic value and returns a transformed value in the monadic context. 

With list, the type of bind is `[a] -> (a -> [b]) -> [b]`. Like
`fmap`, the function will receive every value in a separate
call. Unlike `fmap`, the function has to return the new value as a
list. Clearly, list's implementation of `bind` will need to concatenate 
these list results to form the new list.

We can see therefore how Monad offers more powerful transformations with
"bind" than Functor offers with `fmap`: while fmap can only transform an
individual value "in place", `>>=` can return an empty list, or a list
with more elements than the original. 

To illustrate, we'll bind on a list of `Int` values with a function that
returns a list of the value, repeated "value" times.

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

Lists are an excellent way to "unlearn" the notion that do notation is somehow
akin to imperative programming:

> listBind2Do :: [Int] -> [Int] -> [Int]
> listBind2Do is js = do
>   i <- is
>   j <- js
>   [i,j] 

```
ghci> listBind2Do [1,2,3] [5,6]
[1,5,1,6,2,5,2,6,3,5,3,6]
```

As the result shows, there is no way to see `i <- is` happening "before" `j <- js`
in the `do` code above. The specifics of how list implements Monad determine
how the final result is assembled. After all, we're just composing functions here:
how they *fire* is not related necessarily to how they are *declared*.

Effectful Haskell: Configuration
================================

Now that we have some idea how to work with Monads and Functors, let's start
looking at how we can compose some of the nifty effectful types lying around
in the Haskell libraries.

One "effect" we'd like to have is the ability to read values from an
environment that is present over all of our function calls. An obvious
example is accessing a configuration data structure throughout our program. 

Here's an example config:

> data AppConfig = AppConfig {
>     logfile :: FilePath
>   , version :: String
>   , maxMessageLength :: Int
> } deriving (Show)


The most obvious way to read this config is to pass it to every
function that needs it. One such function might open our logfile and
write a preamble before returning the handle for others to use.

> initLogFileNaive :: String -> AppConfig -> IO Handle 
> initLogFileNaive preamble config = do
>   handle <- openFile (logfile config) WriteMode
>   hPutStrLn handle (preamble ++ "\nVersion: " ++ version config)
>   return handle

A pure example is a function validating message lengths.  It returns
`Either`, the canonical haskell type to indicate failure (constructor
`Left`) or success (constructor `Right`). 

> validateMessageNaive :: String -> AppConfig -> Either String ()
> validateMessageNaive msg config = 
>      if (length msg > maxMessageLength config)
>      then Left ("Message too long: " ++ msg)
>      else Right ()

This is well and good. In an immutable language like Haskell, passing around 
the config manually presents no problem, since no function can modify its
contents. The real problem here is we'd like to *formalize* the concept
that these functions participate in a configuration context, instead of 
having each function specify the config. Not to mention we'd like to
get `AppConfig` out of the argument list since it's not germane to the 
specific problem the function is trying to solve.

One way to formalize our approach is with a type synonym.

> type ConfigReader a = AppConfig -> a

> initLogFileTS :: String -> ConfigReader (IO Handle)
> initLogFileTS = initLogFileNaive
> 
> validateMessageTS :: String -> ConfigReader (Either String ())
> validateMessageTS = validateMessageNaive

Our equations prove that the `ConfigReader` type synonym is just sugar.
It successfully hides `AppConfig` from our signature and standardizes
the config-reading functions. But we still have to deal with a `config`
argument to read any values out, and we have to make sure that argument 
is last, etc. 

To really formalize our approach, we need something like an API that provides
config-reading functions wherever we need them, and implements the "insertion"
of the config into each function signature.

Fortunately, the astonishing generality of the Monad/Applicative/Functor
typeclass family allows us to leverage the "effectful" style to acheive our
goal. The type that implements this magic is called `Reader`.

ReaderT: a monad transformer
============================

A funny thing happened on the way to modern Haskell: some classic monads 
disappeared.

`Reader` is one such classic. It's type is `r -> a`: it literally formalizes
an "extra argument" into a datatype. Such is the power of having functions
as first-class types (not just values): a Reader is a value comprised of
the environment `r` *passed into a computation* `a`. It's in the Monad Hall
of Fame with State, Writer and ... other cool stuff. 

It's also nowhere to be found. All we can find is a type synonym defining 
it in terms of `ReaderT Identity`. Huh?

```
ghci> import Control.Monad.Trans.Reader (Reader)
ghci> :i Reader
type Reader r = ReaderT r Identity
```

What is going on here is that `Reader` has been supplanted by
`ReaderT`, which is more general than `Reader`: it's a *monad
transformer*, meaning that it can be composed with other effectful
types. `Reader` becomes a special case where you don't want to compose
Reader with anything but just a simple value: thus `Identity` in the
type synonym. The same has happened for a number of other pure monads
like `State`: the implementation and functionality surrounds the monad
transformer version.

So what is a monad transformer? It's a way of "stacking" monadic types
so you can build up the exact computational context you need. Our current
example so far runs in IO, but wants to read from an environment.
So, we want to "transform" `IO` with `ReaderT`.

It's a little confusing that we need monad transformers at all. Can't
we just compose monads like functions? Not quite. When you set
a function's type to some Monad, the compiler uses that to select
the correct definitions of `return` and `>>=` to use. Monad
transformers "adapt" this to multiple monads by "lifting" the
appropriate datatype in the monad "stack", based on the type
of the function being bound to.

Writing monad transformers is a little tricky. The good news is
Haskell already has most or all of the monads you'll ever need 
expressed as transformers, so you can simply stack your monads
as you see fit, and go to town.

As we said, our program runs in IO and wants configuration. Our
type is therefore `ReaderT AppConfig IO`. Here's what our functions
will look like in our monad stack:

> initLogFileRTIO :: String -> ReaderT AppConfig IO Handle
> initLogFileRTIO preamble = do
>   f <- reader logfile
>   v <- reader version
>   h <- liftIO $ openFile f WriteMode
>   liftIO $ hPutStrLn h (preamble ++ "\nVersion: " ++ v)
>   return h

Now we have monadic functions for our environment like we have
for IO. `reader` binds the value read by an accessor function applied
to the environment. Another Reader function is `ask`, which would
simply return the entire `AppConfig` value. 

Note however that we had to add `liftIO` to our IO functions. This
is an example of transformer "lifting": the `liftIO` call puts
the `IO` version of bind in scope. Since `ReaderT` is already 
a transformer, its functions (`reader`, `ask`) "lift themselves"
from wherever they are in the stack.

So, we simply use `reader` to grab whatever values from config,
wrap our IO calls in `liftIO`, and voila: effectful Haskell with
IO and config.





The type we want here is `Reader`, which specifies a context 
in which an "extra argument" is available read-only. However, what
we really want is to use Reader with IO: we'd like to have the
effects of Reader at the same time (potentially) as the effects of 
IO. Therefore we use `ReaderT`, which is Reader set up as a
*monad transformer*.

Monad transformers are types  a solution to the difficulty of composing
Monads. Monads are easy to *use*, but not necessarily easy to *write*;
and since they want to take over the "bind" and `return` commands 
as they see fit, it's not that easy to naively compose different 
monads together. Monad transformers solve this by offering a way
to *lift* an individual monad's functionality into a "composite"
context, so that the bind command can effectively "select" which
monad to operate on.

If this sounds like hand-waving, it's only because understanding how
to *write* monad transformers is hard. The good news is understanding
how to *use* monad transformers is easy.

In our use-case, we want to have our functions run in IO (so we can
open log files and such), and have access to a read-only environment.
Our type therefore is `ReaderT AppConfig IO`.

--> initLogFileReaderT :: ReaderT AppConfig IO Handle





> validateMessageReaderT :: String -> ReaderT AppConfig IO (Either String ())
> validateMessageReaderT msg = fmap test (reader maxMessageLength)
>    where test l | length msg > l = Left ("Message too long: " ++ msg)
>                 | otherwise      = Right ()











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



But there's
another Monad that's 100% pure: the list type, `[a]`.

We saw above 






You can imagine it like you want to run some pure
transformations on the "effectful result







 has a single





It allows us to make the captured argument
look like a "normal" variable assignment. `contents` looks like it is assigned to
the results of `readFile f` above, even though it's in fact the argument to
the lambda provided to `readFile`. 

`do` has been compared to imperative notation since you can "read it
down," unlike the right-to-left orientation of function
composition. Since `IO` is focused on sequential execution, `do` code
in IO really does behave imperatively. But other monads can behave quite
differently; and with lazy execution, there's no guarantee of one
bind result executing "before" another.

So it's better to keep bind in mind when using `do`. De-sugar your
code back to straight `>>=`, `>>` and `return` calls often and you'll
be a monad master. 


In the end, it's
just sugar for bind. 
unlike function composition, which composes from right to left.



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

--> data AppConfig = AppConfig {
-->     logfile :: FilePath
-->   , version :: String
-->   , hostname :: String 
-->   , port :: Int
--> } deriving (Show)

We have a function in `IO` that wants to read said config. The naive way to
handle this is pass in the config as an argument.

--> initLogFileNaive :: AppConfig -> IO Handle 
--> initLogFileNaive config = do
-->   handle <- openFile (logfile config) WriteMode
-->   hPutStrLn handle ("Version: " ++ version config)
-->   return handle

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

