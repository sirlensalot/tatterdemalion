---
title: Function Application and Definition
author: Stuart Popejoy
date: November 2014
---

*Author's Note: this article is the first of a series introducing Haskell
to experienced programmers, building up language fundamentals, and unlearning
assumptions from the imperative world.*

In this article, we'll take a close look at functions, namely how we
call them -- function *application* -- and how we define them.

Function application seems like a paltry subject. Indeed, in
imperative languages it's pretty simple. C++ can get hairy regarding
copy-construction on return values, Java manages to confuse some folks
regarding pass-by-value, but in general, it's `foo(bar,baz)` and
that's it.

In Haskell, it gets considerably more interesting. This article will
try to illustrate some of the surprising things that arise from
functions being the fundamental unit of computation. We'll start in
the imperative world, in an attempt to motivate our discussion of why
Haskell does things the way it does.


Variables vs Functions in Imperative Languages
==============================================

Consider the following two statements in Java or C:

~~~~java
int x = 1;
int y() { return 1; }
~~~~

`x` is a variable assigned to 1, `y()` is a *nullary function* (a function
with no arguments) that returns 1. From a usage standpoint, they're identical.

~~~~java
x + 4 == y() + 4 == 5;  // addition
int[] a = {1,2,3};
a[x] == a[y()] == 2;    // list-index
x == y() == 1;          // equality
~~~~

`y()` cannot be redefined in runtime, but `x` can be *re-assigned*. If
somewhere before or during the comparisons above, we write

~~~~java
x = 3;
~~~~

then the comparisons will return `false` (`0` in C), or worse (the array
index will be particularly ugly). Re-assignment means behavior is dictated
by the order in which these statements are written, hence the term "imperative".

Replacing Variables with Function Calls
---------------------------------------

Using functions instead of variables offers some interesting advantages:

1. They can be declared in any (legal) order without affecting
execution -- *where* the function is defined has no impact on its
evaluation.

2. They are un-assignable and therefore *immutable.*

3. They *encapsulate* the value representation. We can change
the implementation without breaking client code.

Of course, this glosses over some details. For one, we'd want our
nullary functions to be **pure**, that is, avoid side-effects such as
IO, system calls, or reading and writing global variables or class
fields. Otherwise we can't really call them immutable.

Another concern is potential performance issues arising from running
functions instead of addressing variables. We will simply note that
there are positive implications that come from the pure functional
approach: for one example, pure code allows for *automatic
memoization* by the runtime, since a pure function always behaves the
same given the same inputs.

Regardless, even if we want to go all-functions in our imperative
code, we're going to hit some roadblocks. First, we'd *lose local
scope entirely,* since C-family languages lack any facility for
declaring functions in local scope. Even functional-friendly
imperative languages have similar restrictions: for example, Common
LISP reserves `defun` for the top level only. But there's a more
pressing concern.

Eager vs Lazy evaluation
------------------------

In the code below, we call `add5` with `y()`. What happens?

~~~~java
int y() { return 2; }

int add5(int i) { return i + 5; }

void callAFunction () {
    add5 ( y() );
}
~~~~

When the call to `add5` is executed, the function `y()` is *evaluated
eagerly* and assigned to the argument variable `i`. Thus within the body
of `add5`, our nullary function is long gone. We're back to variables and
assignment as soon as we invoke a function.

If we wanted to preserve our immutable, pure semantics, we'd need the
compiler to "weave in" the actual definition of `y()` into the code of
`add5`. In other words, when we call functions, we want to
*substitute* our function invocation wherever the argument is
referenced.

This substitution has an interesting consequence, which is that
*immediate evaluation of `y()` is no longer necessary* to invoke
`add5()`. Substitution is really just building up a big computation,
whose evaluation can be deferred until the program needs the
results. This is called **lazy evaluation**, which in practice
generally means that code sequences are only executed when a
world-changing event (in other words, IO) demands the results of the
code.

Lazy evaluation can be tricky to reason about, but it offers
interesting advantages, in that un-needed code branches don't have to
be optimized away by the compiler: they are simply never executed.

So we have to throw in the towel on functionalizing our imperative
languages. But we've learned what we want to see in another language
to get us there:

* We want to avoid referencing variables, preferring functions for
immutability, purity and encapsulation.

* We want to define functions easily in any scope.

* We want argument substitution instead of eager evaluation.

Definitions in Haskell
======================

In Haskell, we can write

> x = 1

and we end up with something altogether different than the options
we had in C or Java. The syntax looks like variable assignment --
`int x = 1;` -- but in fact we've defined something akin to our
nullary function above -- `int x() { return 1; }`.

In truth, it's neither: this is a definition that binds the symbol `x`
to a constant value `1`. Formally, it's a *closed expression*, not a
function. Nonetheless, it gives us everything we were looking for with
our nullary functions above:

* we can use it anywhere we like, independent of where/when we declared it

* We can't reassign or redefine it; it's a permanent, immutable definition

* We can change how we arrive at its value, e.g. `x = 2 + 3 - 4`, without impacting client code.

Informally, we can think of this as a nullary function, and for
imperative programmers, this can be helpful, to "unlearn" our instincts to
see an expression like `x = 1` as variable assignment. As we'll see
below, "real" functions with arguments are defined using the same syntax, so
it's not terribly inaccurate to just see this as a definition of a nullary function.

When you see `=`, think "I'm defining a function" instead of "I'm assigning a variable".

Type Inference
--------------

Haskell is strongly-typed. So what type is `x` above?

A debate has raged pitting typed, compiled languages like C versus
dynamic, untyped languages like Javascript, with the claim that types
slow down development and get in the way. Haskell does an end-run
around this whole debate by allowing us to have our types, but only
specify them when we have to. It does this with *type inference,*
meaning that in many cases, the compiler can figure it the type by
itself.

Let's use the Haskell interpreter to see what type `x` is. If you have
ghci, the Haskell REPL, up and running, you can issue `let x = 1` to
follow along. Alternately, you can [download the
source](https://raw.githubusercontent.com/slpopejoy/tatterdemalion/master/posts/2014-11-27-FunctionApplicationDefinition.lhs) of this article, which is "literate haskell" and
therefore can be loaded up in your interpreter with the ":load"
command, and just reference `x` directly.

We ask ghci using the `:t` command:

~~~~
ghci> :t x
x :: Num a => a
~~~~

Huh? That's more complicated than `int y()`! We'll defer a deep-dive
on Haskell's type system, and summarize that this means `x`'s type
involves `Num`-eric types. Unlike many languages, Haskell doesn't
assume the literal `1` always indicates an integer type!

Type Signatures
---------------

To keep things simple, and because it's generally good practice,
we'll provide a **type signature** for our top-level definitions:

> y :: Int
> y = 2

Here, we've defined `y` to return the constant value `2`, and given
it a type signature that fixes the type to `Int`. When we ask ghci,
we get a much simpler answer:

~~~~
ghci> :t y
y :: Int
~~~~


Invoking functions
------------------

When the time comes to use these expressions, Haskell offers a
much cleaner syntax, with no parentheses required. Below
are the Haskell versions of our imperative code above:

> someTests = (
>              (x + 3) == 4,         -- addition
>              ([1,2,3] !! x) == 2,  -- list-index
>              x /= y                -- (in)equality
>             )

Note: this example is executable Haskell code. To do so, I've
encased the examples in the dummy function `someTests`. I didn't bother
to provide a type signature for it, as the type is not relevant.
The reader can figure out the type as an exercise, or ask ghci.

Local Scope
-----------

We talked about how nice it would be to declare functions in local scope. In
Haskell we have all the same tools for local definition as we do at top level:

> localWithWhere = y * 30 + y ^ 2
>                  where y = 3

Here, we've defined `y` to be used twice in the outer function body.
The `where` [keyword](http://www.haskell.org/haskellwiki/Keywords#where)
starts a block of code for definitions in local scope.

Note that this article's code has already defined of `y` at the top level, above.
Haskell allows definitions with the same name in local scope, which will
override the top-level definition. This is called *shadowing* the top-level definition.

You can also use ["let" and "in"](http://www.haskell.org/haskellwiki/Keywords#let):

> localWithLet = let y = 3
>                in y * 30 + y ^ 2

This is more familiar to the imperative idiom, in that something that
looks like a variable is defined before the consuming code. I
recommend imperative programmers stick with `where`, since it looks
the least like imperative assignment. Otherwise, it's a matter of
style: `where` allows you to put the high-level functionality first,
followed by details; `let`/`in` is maybe clearer in some situations.

Local definitions can have type signatures, too:

> localWithTypeDecl :: Int
> localWithTypeDecl = y * 30 + y ^ 2
>                     where y :: Int
>                           y = 3

Here we start to see some real benefits of type inference. At top
level, leaving off type signatures will generally make your code
*harder* to understand, but at local level, type inference makes your
code *easier* to read. The type signature for `y` above is simply
verbose: it's clear from the top-level type that we're dealing with
`Int` values.


Arguments and Application
=========================

We've happily dispensed with variable assignment, with Haskell providing
us a clean and powerful syntax for defining expressions. It's now time
to look at "real" functions, those things that have arguments, and see
how we define and invoke them.

Unary and Binary functions
--------------------------

In a pure language, a unary (one-argument) function directly maps
input to output. Let's define one:

> unaryFunction :: Int -> Int
> unaryFunction x = x * 3 + 2

The type signature is `Int -> Int`, meaning the function takes an
`Int` argument and returns an `Int`. Thinking of it as mapping input
to output, we can imagine the argument "indexing" the output. Indeed
`Int -> Int` even looks like mapping syntax from languages like
Perl. The arrow dividing the argument from the result type is the
*[function type
constructor](http://www.haskell.org/haskellwiki/Keywords#-.3E)*, which we'll be seeing a lot.

A binary (two-argument) function can also be thought of as a mapping
or indexing operation, but in two dimensions. Here's a binary
function:

> binaryFunction :: Int -> Int -> Int
> binaryFunction x y = x * y + x

This is when the type signatures stop looking like anything you've
seen in an imperative language. The type signature is growing to the
left, with arrows dividing the arguments *and* delimiting the argument from the result type. Here's
an example with different types to make the positions clearer:

> lengthIsDivisible :: String -> Int -> Bool
> lengthIsDivisible s i = (mod (length s) i) == 0

This function takes a `String` and an `Int` and returns a `Bool`,
`True` if the length of the string is divisible by the second
argument.

Let's compare the signatures so far:

~~~~haskell
y              ::               Int
unaryFunction  ::        Int -> Int
binaryFunction :: Int -> Int -> Int
~~~~

Note the consistency and simplicity. It's admirable, beautiful, maybe
confusing at first. But it's no accident that the syntax is unified,
as we'll soon see.

Invoking functions with arguments
---------------------------------

To invoke functions, we simply delimit argument values with whitespace.

> invokeUnary = unaryFunction 8     -- will compute 8 * 3 + 2
> invokeBinary = binaryFunction 2 3 -- will compute 2 * 3 + 2

Again Haskell is simple and beautiful: no parentheses, no dots, just
functions and values. However, once we start calling functions with
other functions, we'll need some more syntax. Let's try calling
`binaryFunction` with `unaryFunction 3` as its second argument:

```
binaryFunction 2 unaryFunction 3  <=== bzzt
```

This doesn't work: the compiler can't distinguish what's an argument
from what's a function invocation. We'll need to clear things up
with parentheses.

> invokeParens = binaryFunction 2 (unaryFunction 3)

That's better. Evaluation looks straightforward: compute
`unaryFunction 3` and pass the results as the second argument for
`binaryFunction 2`, right? Not so fast!

Recall our discussion of *eager evaluation* above, and how we wanted
to avoid assigning variables by *substituting the function call in for
the argument variable*. What it means here is we want to substitute
the entire invocation of `unaryFunction 3` into `binaryFunction`,
looking something like this:

> binaryFunctionSubstituted = 2 * (unaryFunction 3) + 2

`x` becomes 2, and `y` becomes `(unaryFunction 3)`. With this
substitution, we now have a *new expression*, which Haskell will evaluate
lazily when the results become needed.


The Application Operator
------------------------

Parentheses are not the only way to delimit function
arguments. Haskell also has `$`, the *[application
operator](http://hackage.haskell.org/package/base-4.7.0.1/docs/Prelude.html#v:-36-)*.
Here's how we can write the code above without parentheses:

> invokeAppOp = binaryFunction 2 $ unaryFunction 3

This looks nice. Everything after the `$` is "bunched together" as the
second argument to `binaryFunction`. It has the same meaning as
`binaryFunction 2 (unaryFunction 3)` but with a sweet functional
look.

We could leave it at that, and marvel at Haskell's flexible syntax.
But let's look deeper. A common mistake is to think that `$` is a
keyword, or lexical syntax. In reality it's nothing more than a
library function.

Let's ask ghci for its type. To do so, GHCI requires us to put
parentheses around `$`:

~~~~
ghci> :t ($)
($) :: (a -> b) -> a -> b
~~~~

This shows it's a normal function, since you can't ask for the type of
keywords and syntax:

~~~~
ghci> :t (=)
<interactive>:1:2: parse error on input ‘=’
~~~~

`$` is just a function, but it's pretty cool that we can drop it
in instead of parentheses! Most languages don't allow you to
arbitrarily replace argument delimiters with library functions. So
what's going on? What about the type signature?

~~~~
($) :: (a -> b) -> a -> b
~~~~

Uh-oh. Don't worry, we'll explicate all of this in due time. For now
suffice it to say that **($) is a function that takes two arguments: a
unary function to be applied, and its first argument**. "`(a -> b)`"
is the first argument, and you can see it looks like a one-argument
function; "`-> a`" is the second argument, and "`-> b`" is the return
type. The lowercase letters indicate the function is *polymorphic*,
which we'll gloss over for now to say that they take any type.

`$` takes a unary function and it's argument. Let's try it on just a unary function:

> invokeUnaryAppOp = unaryFunction $ 2

Super-boring, function application restated. What's the point?

To spice it up, we'll put parentheses around `$`; in Haskell this
"un-infixes" it and turns it into a "normal", prefix function:

> invokeUnaryAppOpPrefix = ($) unaryFunction 2

That's a little more interesting. Now we can see that `$` is taking a
unary function as its first argument, and the value to apply to that
function as its second. Still hasn't shown us anything though, we've
just used a function to ... call a function. Let's keep digging.

Partial Application
-------------------

Let's use that prefix-notation trick to rewrite our example above:

> invokeAppPrefix = ($) (binaryFunction 2) (unaryFunction 3)

We have to resort to parentheses again to keep GHC happy. What do we
have? `(unaryFunction 3)` is straightforward. But ... something funny
is going on with `(binaryFunction 2)`. The parentheses make it clear
that instead of taking two arguments, it only has one! What kind of
binary function only takes a single argument??

This is our first example of **partial application**, a core concept
in functional programming. The idea is you can "partially invoke" a
function with just one of its arguments, and in so doing you create
*another* function that takes the remaining argument. To illustrate,
let's define integer addition as a function:

> intAdd :: Int -> Int -> Int
> intAdd x y = x + y

We can invoke this the usual way, `intAdd 2 3` will evaluate to
`5`. If we only provide the first argument, though, we'll create a
new, unary function:

> intAddPartial = intAdd 2

This compiles, and as promised, `intAddPartial` is a unary function
that always adds `2` to its value. Let's play with it in GHCI:

~~~~
ghci> :t intAddPartial
intAddPartial :: Int -> Int
ghci> intAddPartial 3
5
~~~~

Partial application shows up in many functional-ish languages, but
it's often presented as "something special", distinct from everyday
function use. For instance, in Clojure, we have to use the `partial`
function to do so, which is similar to other LISPy languages.

In Haskell, partial application is fundamental. When we said there was
more to those elegant type signatures than good looks, this is what we
were getting at: the unity of the type signatures matches the unity of
partial- and full-application. Let's compare `intAdd` and
`intAddPartial`'s signatures:

~~~~haskell
    intAdd        :: Int -> Int -> Int
    intAddPartial ::        Int -> Int
~~~~

We can visually see that first argument being "bundled" into the
partially-applied version. In Haskell, partial application is as
simple as leaving off an argument, resulting in a new function that
takes one less argument. Let's ask GHCI:

~~~~
ghci> :t intAdd 2
intAdd 2 :: Int -> Int
~~~~

Partial application operates hand-in-hand with substitution. Recall
above how supplying `unaryFunction 2` to another function, created a
new function with `unaryFunction 2` stitched into it. The idea here is
that **"full" application is no different from partial
application**:

~~~~
ghci> :t intAdd 2 3
intAdd 2 3 :: Int
~~~~

"intAdd 2 3" *fully applies* `intAdd` to derive a *lazy computation*
that returns 5. Full application is useful in a lazy context, in that you can "load up" a
function with all of its arguments and pass it into another function
to be evaluated later, for instance as an "event" ready to
dispatch.

Partial Application and ($)
---------------------------

Let's return to our investigation of the application operator `$`
above. Our prefix rewrite resulted in `($) (binaryFunction 2)
(unaryFunction 3)`. Hopefully it is now clear what `binaryFunction 2`
by itself means: `binaryFunction` partially-applied to 2, creating a
new function:

~~~~
ghci> :t binaryFunction 2
binaryFunction 2 :: Int -> Int
~~~~

Finally, we've unpacked how `$` does its magic: it simply relies on
normal function application. `binaryFunction 2 $ unaryFunction 3` can
be written entirely with parentheses to make this clear:

> invokePartialParens = (binaryFunction 2) (unaryFunction 3)

And indeed, the definition of `($)` is nothing but [function application
itself](http://hackage.haskell.org/package/base-4.7.0.1/docs/src/GHC-Base.html#%24):

~~~~haskell
    ($)             :: (a -> b) -> a -> b
    f $ x           =  f x
~~~~

There you have it: `f $ x` just calls `f x`. Further up the source file, we also see

~~~~haskell
infixr 0  $
~~~~

which establishes `$` as a right-infix function with precedence
0. It's trivially easy in Haskell to declare new "operators" this way,
that is, infix binary functions with "swear word" characters like
`$@?` etc. As a result, very little of Haskell is defined as syntax or
keywords, but instead is in the library, with source you can look up.

Functions as Values
===================

Recall our use of `($)` on a unary function above:

~~~~
($) unaryFunction 3
~~~~

There's something curious here: `unaryFunction` *looks* like it's
being applied to `3`, but instead it's an argument to the `($)`
function. Thus, `unaryFunction` is acting purely as a **value**, that
is, the value of the first argument to `($)`.

In Haskell, functions are the main attraction, so this isn't too
shocking. There are some surprising implications though. Since
functions are values, then we can use a function "as such" in other
definitions. We can "alias" a function:

> ufAlias :: Int -> Int
> ufAlias = unaryFunction

No problem: we're assigning `ufAlias` to the value `unaryFunction`,
which happens to be of type `Int -> Int`. We can now use it just like
`unaryFunction`:

~~~~
ghci> ufAlias 1
5
~~~~

This seems simple enough, until you notice *we've just defined a unary
function without ever mentioning the argument!* In most languages,
this is impossible: you have to restate your argument definitions over
and over. Here's some equivalent C code:

~~~~java
public int add1(int x) { return x + 1; }

public int aliasAdd1(int x) { return add1(x); }
~~~~

Of course, nothing is stopping us from restating the arguments:

> ufAliasWithArgs :: Int -> Int
> ufAliasWithArgs x = unaryFunction x

Stare at `ufAlias` above and `ufAliasWithArgs` for a second. They do
the same thing, but `ufAlias` accomplishes it with simple
assignment. It makes perfect sense if you think of functions as
values.

Point-Free Style (Eta reduction)
--------------------------------

The ability to define functions without referencing the arguments is a
*major* source of confusion for new Haskell developers. Very few
languages do this: we're accustomed to understanding a function by
looking at the argument list. Taking the arguments away can be very
unsettling.

Hopefully the simple example above makes clear that we're not "hiding"
arguments when we say `ufAlias = unaryFunction`, but simply working
with `unaryFunction` as a value. Armed with this insight, we can
tackle the beast known as "point-free" style, or even more
intimidatingly, "eta reduction", where arguments vanish from the left-
and right-side of a function definition.

Let's take another example from our discussion further up: our partial
application `(binaryFunction 2)`, which created a new function with a
single argument. Well, that new function is just another value. Let's
do the same trick with it:

> bfPartial :: Int -> Int
> bfPartial = binaryFunction 2

Make sense? We could have written `bfPartial y = binaryFunction 2 y`,
but why bother: the value `binaryFunction 2` is usable all by itself,
so our definition of `bfPartial` will reference that value alone.

Point-free code is often presented as a stylistic exercise to look
like a functional wizard. I'm trying to show here is that it is
instead a fundamental consequence of working with functions as
values. The point is not to "hide" arguments, but to ramp up the
expressiveness of the language to fully exploit the power of
functions.


Review
======

Haskell is a *pure* language, meaning that a computation's
output is always determined by its inputs. This implies
that no funny business can go on inside of a function body or
definition: we can't do IO, assign fields
or global variables, or make system calls.

As a consequence of purity, Haskell does not *eagerly evaluate*
function arguments, but instead *substitutes* the code used to make up
the argument into the body of the target function. Substitution means
that in Haskell, *applying a function to an argument creates a new
function*.

Function application happens one argument at a time: each argument
when applied creates a new function which is then applied to the next
argument. A function applied to less arguments than specified in its
definition is *partially applied*, resulting in a new function with
however many arguments are left.

Because Haskell is a *lazy* language, there is no difference between a
fully- or partially-applied function: a fully-applied function is not
necessarily evaluated at the site where the arguments were applied,
meaning you can use a fully-applied function as a event-like nullary
function to be "called" elsewhere.

Functions are defined with the equals operator, and specified using
type signatures. The arrow operator `->` delimits argument types and
the result type. Functions can be defined and specified at top-level,
or locally with `where` or `let`/`in`.

While Haskell is strongly-typed, *type-inference* allows us to elide
the type specification where convenient. Idiomatically, we provide
type signatures for top-level functions, and leave off type signatures
for local functions.

When applying functions, we delimit argument values with whitespace,
and delimit inner function application with parentheses. We can also
use the library function `$` in place of parentheses where convenient.

Since functions are values in Haskell, we can work with them "as
such", meaning we can assign other names to them. As a result, we do
not have to restate the arguments, and can elide them from our
function definitions, a process called "eta reduction", or "point-free
style".
