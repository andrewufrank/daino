---
title: ShakeForTest.md
author: auf 
date: Jan. 29, 2019
keywords: SSGdesign
abstract: The layered architeture for SSG
---

The construction of regression tests is very time consuming; 
- changes in the code 
require adaption,
- naming conventions are constantly confusing,
- sequence of running is difficult to understand.

`shake` could be used to manage regression tests.

Automatic testing algebraic properties --- especially inversion --- 
remains much faster and easier to detect errors. It must have precedence and
anything which can be tested algebraicly should be done so. Example: 
writing and reading to a file. 

## Shake controls sequence of execution

The dependencies between functions to consume and produce data files
necessary in the regression tests is controlled by `shake` and the 
caching method reduces unnecessary recomputations which also 
reduces clutter in the output from functions which are not needed 
to reexecute. 

## Limitation

Changes of code may make parts of the produced values invalid. 
Shake can currently not detect which parts and requires recomputing 
all the files.

## Regression

A regression test compares a newly computed value with the value
previously computed. 

Using `meld` open a directory comparision with a copy of `.SSG` and itself. 
The copy remains till replaced, the `.SSG` directory changes and changes are 
identified.

## File level operations used 

In general, the operations are on the file level and shake is used to 
read and write the files (using the `TypedFile`) class. For the top 
level functions, a test with  
 
TODO 


