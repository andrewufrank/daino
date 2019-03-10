---
title: 40. HaskellStyle.md
author: auf 
date: Jan. 4, 2019
keywords: Haskell, uniform
abstract: How to write Haskell code which can be maintained. Some style guidelines. 
---


Writing Haskell code is time consuming. Why? I experience often unnecessary complexity and 
compile time errors when using packages from others or from myself not 
adhering to expected standards. Typically:
- confusion about representation of character sequences (aka "Strings"),
- issues with directory and filenames (or url),
- diffferences in the monad used,
- obscure shortcuts and infix function names,
- ...

I try to give here some rules how to write readable Haskell
 
The overall goal is to make functionality easy to understand to assure
that I can read the code again and establish conventions on which a 
user of a package can rely. It is an attempt to reduce the choice of 
the programmer to use a specific solution to write code when Haskell 
allows many different approaches.

**Clarity - achieved by making functionality visible and providing only one way of achieving a goal.**

## Use do notation
There are nice ways to combine monads leading to very short and elegant 
code. In my experience they are hard to read and understand. 

Example: the first lines of a main from `bibtex`: 
```
main :: IO ()
main =
   do bib <- getContents
      case Parsec.parse (Parsec.skipMany Parsec.space >> Parse.file) "stdin" bib of
         Left errMsg -> hPutStrLn stderr (show errMsg)
         Right entries ->
            mapM_ (putStrLn . cite) entries
```

The difficulty is that elegance implies much is not said and must be 
reconstructed by a potential user. 

## write type annotations 
Whenever writing code and you know the type of a symbol, annotate it.
It reduces "ambiguous type error", checks statically against mistakes 
and makes understanding code later when searching for errors or reading 
the code of others easier. 

## document the intention of functions
Between the type of a function and the code insert a comment line (with `-- ^`)  to 
inform yourself and future users what should be achieved and why.

## packages export types and required support functions
Reduce the number of imports by exporting in a package all functions and 
types a user will automatically need to use these functions (and 
annotate the types). 

## isolate seldom used Haskell features  
There are very elegant solutions, for example lenses, to get and set 
values in JSON records with a few inline functions. Isolate such functions in 
a single location and pack into ordinary functions which are documented and 
exported.

In particular, avoid in general code:
- lenses
- type level programming
- template haskell

## handling of directory and filenames 
Shake seems to include a nearly complete set of functions to handle directory and file names; perhaps I should always use this set?

