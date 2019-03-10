---
title: 10. Principles.md
#author: auf 
date: March 5, 2019
keywords: SSGdesign
abstract: 'Why "yet another static site generator"? Pandoc provides nearly everything and 
    gives the desired functionality (code in Haskell, markdown as primary text input, 
    backup with git).' 
---

## Some comments on other Static Site Generator
I have tried a few tools to produce an homepage for an university researcher.
I observed in late 2018 some shortcommings
where I desired   simpler or a bit more flexible for my use case: 

- Not easily (i.e. out of the box) working with markdown (WordPress),
- Hard to exclude commercial interests, 
especially from the companies and agencies which are 
trying to observe everything "to provide us with a better service" 
(WordPress seems to invite Google Analytics and similar by default), 
- Missing integration with BibTex to produce references and a list of 
publlications from BibTex files missing,
- I prefer to work in 
the language I know already and not to learn a new set of obscure quirks 
(WordPress, Sprinkles),
- Small and build from components which are relatively independent is my preference (excludes approaches 
"batteries included" like Jekyll and Hakyll; similar [comment](http://hackage.haskell.org/package/slick-0.2.0.0))
- not extensible and composable - whatever composable means for a site generator.
I was very impressed with static site generators, e.g. [SitePipe](https://github.com/chrispenner/sitepipe) which demonstrated how much functionality is available in packages (e.g. from Hackage)

## Goals
- Build a SSG from available packages in Haskell. 
- Demonstrate integration using the "uniform" approach to wrap packages in integratable interfaces. 
- Streamline the design to use the least amount of packages and to reduce complexity in maintenance.

### Programmable
It is my  experience when adaptation is needed anything short of a full (and well
designed) programming language leads to an infinite sequence of special case additions bolted on with
some ugly screws; I have the impression that this is a limiation of e.g.[Sprinkles](https://github.com/tdammers/sprinkles). 

### Uniform interfaces 
Wrap packages into a small interface layer to locally hide differences between packages 
which hinder integration. 
- use Text as the primary representation and use `uniform-strings` to convert to and from 
other representations (with local deviations from the rule)
- all functions are pure or in the ErrIO monad (`ErrorT Text a IO`); operations in other 
monads are wrapped.
- represent path to files as with `Path` and use `uniform-fileio` for all operations. 
- write top level code   in a basic form of Haskell and eschew use of special features ([see Haskell style](HaskellStyle.html))

### Separate "Theme" and "Content"
The theme (templates, css etc.) and the content should be separated, with a documented interface. Default locations for theme and content can be adapted to needs.


### Performance
SSG is mostly a proof of concept and demonstration for small academic homepages, 
not for humongous webpages for large organizations. Performance is not designed in; 
if performance is too slow for actual use, localize the issue and fix it. 

The use of [twich](http://hackage.haskell.org/package/twitch) and 
[shake](http://hackage.haskell.org/package/shake) gives a nearly dynamic behaviour: 
changes are reflected quickly in locally served pages.

### Character file based to facilitate backup and version management
The storage of content should be in text files which can be versioned with `git` and backed up with ordinary tool. 

Many site generator, especially flexible content management systems, use databases like SQL for storage of date; for small sites, full function databases are too complex and notoriously difficult to integrate, backup and close against intruders. 

### Documentation ***_not yet done_***
Documentation with Sphinx (from markdonw blog like texts) included in SSG10 from the beginning. 
 

## Build Site Generator around [Pandoc](http://hackage.haskell.org/package/pandoc)

Pandoc translates many differnt text file formats into `html`, 
it can handle BitTex references in text and produce publication lists 
(with [pandoc-citeproc](http://hackage.haskell.org/package/pandoc-citeproc). 
It works well with [doctemplates](http://hackage.haskell.org/package/doctemplates), 
which is a small template system with just conditionals and loops. 

Pandoc works with conversion of files into value. Files can include metadata as YAML blocks in the text source. 

### What remains to be designed:
The design fixes the file structure: theme and content (dough) is separated. 
The resulting `html` files served are stored elsewhere. 
([Storage of Site Data](DefaultFileLayout.html))

### What can be improved with pandoc:
Wrapping pandoc functions within the ["uniform" design](HaskellStyle.html). 

Use JSON values extensively to collect the pieces and keep the `html` text produced by 
pandoc in JSON values, from which it is injected into the template.
