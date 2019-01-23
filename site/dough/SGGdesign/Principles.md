---
title: Principles.md
author: auf 
date: Jan. 4, 2019
keywords: SSGdesign
abstract: Why "yet another static site generator".
---

## Some comments on other Static Site Generator
I have tried a few tools to produce an academic homepage. What I disliked: 

- Not easily (i.e. out of the box) working with markdown (WordPress),
- Difficult to insure that all the companies and agencies which are 
trying to observe everything "to provide us with a better service" are exclude 
(WordPress), 
- Extensible, especially to include BibTex for references,
- If the command language (or the GUI) is complex I prefer to work in 
the language I know already and not to learn a new set of obscure quirks (WordPress, sprinkles),
- Small and build from components which are relatively independent (excludes approaches 
"batteries included" like Jekyll and Hakyll)
- I was very impressed with SSG which demonstrated how much functionality is available as packages (e.g. from Hackage).

## Goals
Build a SSG from available packages in Haskell. Demonstrate integration using the "uniform" approach to wrap packages in integratable interfaces.

### Programmable
It is my standard experience when adaptation is needed anything short of a full (and well
designed) language leads to an infinite sequence of special case additions bolted on with
some ugly screws. -> Extensibility and adaption with a program language. 

### Uniform interfaces 
Wrap packages into a small interface layer to locally hide differences between packages 
which hinder integration. 
- use Text as the primary representation and use `uniform-strings` to convert to and from 
other representations (with local deviations from the rule)
- all functions are pure or in the ErrIO monad (`ErrorT Text a IO`); operations in other 
monads are wrapped.
- represent path to files as with `Path` and use `uniform-fileio` for all operations.

### Separate "Theme" and "Content"
The theme (templates, css etc.) and the content should be separated, with a documented interface. Default locations for theme and content can be adapted to needs.


### Performance
SSG10 is mostly a proof of concept and demonstration for small academic homepages, 
not for humongous webpages for large organizations. Performance is not designed in; 
if performance is too slow for actual use, localize the issue and fix it. 

### Character file based to facilitate backup and version management
The storage of content should be in text files which can be versioned with `git` and backed up with ordinary tool. 

This rules out databases like SQL (which are notoriously difficult to integrate). 

### Documentation
Documentation with Sphinx (from markdonw blog like texts) included in SSG10 from the beginning. 




