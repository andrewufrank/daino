---
title: PackagesUsed.md
author: auf 
date: Jan. 4, 2019
keywords: SSGdesign
abstract: The Pacages from Hackage used.
---

## Pandoc

The central component of any modern site generator seems to be [Pandoc](). At the moment
only markdown is used for content and output is html, with a plan to produce pdf
for print output. 

[Pandoc-citeproc]() allos the inclusion of references and reformat references based on a 
BibTex file, which includes the details. 

Only bake uses pandoc for conversion. 

## Templates
Pandoc includes a template system, but it is not extensible, which makes it tempting 
to use [ginger](), which is a Haskell implementation of [Jinja2](). 

## Watching file change : Twitch
[Twitch]() uses FSnotify to connect programmed actions to activities with files

## Server 
[Scotty]() 

## Caching
[Shake](http://hackage.haskell.org/package/shake) is a Haskell version of `make` and can
be used to convert a static site (idee in [Slick](http://hackage.haskell.org/package/slick)





