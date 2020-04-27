---
title: 30. PackagesUsed.md
author: auf 
date: Jan. 4, 2019
keywords: SSGdesign
abstract: 'The Packages from Hackage used. 
    Primarily pandoc, pandoc-citeproc, doctemplates, 
    but also twich, shake, scotty 
    and aeson, lens, and aeson-lens. '
---

## Pandoc

The central component of any modern site generator seems to be 
[Pandoc](http://hackage.haskell.org/package/pandoc). At the moment
only markdown is used for content and output is html, with a plan to produce pdf
for print output. 

[Pandoc-citeproc](http://hackage.haskell.org/package/pandoc-citeproc) allows the inclusion of references and reformat references based on a 
BibTex file, which includes the details. 


## Templates
Pandoc includes a template system, [doctemplates] (http://hackage.haskell.org/package/doctemplates). Injects text values from a JSON record (based on labels); it allows conditionals (`$if(label)$ .. $endif$) and loops. 


## Watching file change : Twitch
[Twitch](http://hackage.haskell.org/package/twitch) 
uses FSnotify to connect programmed actions to activities with files. It can be used 
to notify the process which bakes files about changes in file content.

## Server 
[Scotty](http://hackage.haskell.org/package/scotty) is a very simple to integrate 
web server. It can be used for local serving during development.

## Caching
[Shake](http://hackage.haskell.org/package/shake) is a Haskell version of `make` and can
be used to convert a static site (idee in [Slick](http://hackage.haskell.org/package/slick)

## JSON 
The [aeson](http://hackage.haskell.org/package/aeson) Haskell implementation of JSON is
used, together with [aeson-lens](http://hackage.haskell.org/package/aeson-lens) for 
getting and setting values in JSON records. 



