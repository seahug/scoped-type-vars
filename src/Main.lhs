# Seattle Area Haskell Users' Group

## GHC's `ScopedTypeVariables` language extension<br/>Presented by Richard Cook

### Example: catching exceptions

This is the simplest example I can think of and how I first learnt about the language extension.

First, let's add the pragma and import a couple of things:

~~~ {.haskell}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import System.IO
~~~

~~~ {.haskell}
doSomethingWithHandle = const (return ())
~~~

We want to read a file and handle `IOException`, which is thrown if the file doesn't exist for example.

~~~ {.haskell}
processFile0 =
    (withFile "no-such-file" ReadMode doSomethingWithHandle)
        `catch` handleException
    where
        handleException :: IOException -> IO ()
        handleException _ = putStrLn "IOException"
~~~

It's important to note that the type annotation `SomeException -> IO ()` is required in this context to constrain the type of the exception. Introducing the name `handleException` is annoying and ought to be unnecessary.

Alternatively, one can constrain the type of the exception by binding with a type annotation in certain contexts:

~~~ {.haskell}
processFile1 =
    (withFile "no-such-file" ReadMode doSomethingWithHandle)
        `catch` (\e -> let e' = e :: IOException in putStrLn "IOException")
~~~

But both of these feel like hacks and can be brittle. I would like to fix the type of the exception at the source, ideally in the lambda's argument list. `ScopedTypeVariables` to the rescue:

~~~ {.haskell}
processFile2 =
    (withFile "no-such-file" ReadMode doSomethingWithHandle)
        `catch` (\(_ :: IOException) -> putStrLn "IOException")
~~~

Without this language extension this example would fail to compile.

<!--
~~~ {.haskell}
main =
    processFile0 >>
    processFile1 >>
    processFile2 >> putStrLn "Done"
~~~
-->

## Licence

Released under MIT License

Copyright (c) 2016 Richard Cook

[stack]: https://haskellstack.org/
