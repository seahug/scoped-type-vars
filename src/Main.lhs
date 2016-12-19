# `ScopedTypeVariables` language extension

* [Main source][scopedtypevars]
* I'll talk about GHC, though other Haskell compilers may support it too
* [Enables lexical scoping of type variables explicitly introduced with `forall`][scopedtypevarsdoc]
* Interestingly, I don't think I've ever used it with `forall`

## Let's preemptively add the pragma

Here's the top of our program:

~~~ {.haskell}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import System.IO
~~~

## Example: from the users' guide

Consider the following innocent-looking function:

~~~ {.haskell.ignore}
f xs = ys ++ ys
    where
        ys :: [a]
        ys = reverse xs
~~~

Let's try to compile this&hellip;

~~~
• Couldn't match expected type ‘[a1]’ with actual type ‘t’
    because type variable ‘a1’ would escape its scope
  This (rigid, skolem) type variable is bound by
    the type signature for:
      ys :: [a1]
    at Scratch.hs:3:9-17
• In the first argument of ‘reverse’, namely ‘xs’
  In the expression: reverse xs
  In an equation for ‘ys’: ys = reverse xs
• Relevant bindings include
    ys :: [a1] (bound at Scratch.hs:4:9)
    xs :: t (bound at Scratch.hs:1:3)
    f :: t -> [a] (bound at Scratch.hs:1:1)
~~~

* What just happened here?
* Correct me if I'm wrong&hellip;
    * `a` is introduced by the definition of `ys` for some type variable `a`
    * `f` has type `b -> [c]` for type variables `b` and `c`
    * Compiler attempts to unify `c` with `a`
    * This causes `a` to escape the scope of `ys` and this isn't OK
    * My explanation makes more sense than [the standard explanation][scopedtypevarsdoc], even if it's inevitably subtly wrong
* The problem is the type signature `ys :: [a]`
* In fact, this code will compile happily without it
* However, this is a situation in which it's _impossible_ to write a valid type signature

This can be fixed by enabling `ScopedTypeVariables` and adding a type signature for `f`:

~~~ {.haskell}
f :: forall a . [a] -> [a]
f xs = ys ++ ys
    where
        ys :: [a]
        ys = reverse xs
~~~

Well, that's nice and everything. How do _I_ actually use this language extension?

## Example: catching exceptions

This is the simplest real-world example I can think of. It's also why I first learnt about this extension.

Consider the following function which ignores its argument and evaluates to a unit action:

~~~ {.haskell}
doSomethingWithHandle = const (return ())
~~~

We want to read a file and handle `IOException`, which is thrown if the file doesn't exist, for example.

~~~ {.haskell}
processFile0 =
    (withFile "no-such-file" ReadMode doSomethingWithHandle)
        `catch` handleException
    where
        handleException :: IOException -> IO ()
        handleException _ = putStrLn "IOException"
~~~

It's important to note that the type signature `IOException -> IO ()` is required in this context to constrain the type of the exception. Introducing the name `handleException` is annoying and ought to be unnecessary. To shamelessly quote myself: [_Functions are so important in Haskell that we get to refer to them by name or with no name at all_][anonymousfunctions].

Alternatively, one can constrain the type of the exception by binding with a type signature in certain contexts:

~~~ {.haskell}
processFile1 =
    (withFile "no-such-file" ReadMode doSomethingWithHandle)
        `catch` (\e -> let e' = e :: IOException in putStrLn "IOException")
~~~

But both of this and the `handleException` case feel like hacks. They can also be brittle. I would like to fix the type of the exception at the source, ideally in the lambda's argument list. `ScopedTypeVariables` to the rescue:

~~~ {.haskell}
processFile2 =
    (withFile "no-such-file" ReadMode doSomethingWithHandle)
        `catch` (\(_ :: IOException) -> putStrLn "IOException")
~~~

Without this language extension this example would fail to compile. Now it looks the way I want it to look.

Done.

<!--
~~~ {.haskell}
main =
    processFile0 >>
    processFile1 >>
    processFile2 >> (print $ f ([1, 2, 3, 4, 5] :: [Int]))
~~~
-->

## Licence

Released under MIT License

Copyright (c) 2016 Richard Cook

[anonymousfunctions]: https://github.com/rcook/beginning-practical-haskell/blob/master/part02.md#anonymous-functions-and-lambda-abstraction
[scopedtypevars]: https://wiki.haskell.org/Scoped_type_variables
[scopedtypevarsdoc]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#ghc-flag--XScopedTypeVariables
[stack]: https://haskellstack.org/
