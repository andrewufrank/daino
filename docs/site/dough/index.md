---
title: "Static Site Generator Daino: Landing page" 
abstract: The `Example Homepage` produced by `daino` list differences to other SSG and gives rationale for its design. It points to the source and how to use it to run your own web site.
author: Author of Settings
date: 2010-01-29
keywords: homepage
# page-title: ExampleHomepage
version: publish
visibility: public
---

#  Daino: A Static Site Generator 
<!-- copy of the ReadMe.md file shown in github -->

A static site generator designed by an academic to allow: 

- web pages written as (Pandoc) markdown (with YAML header for title and `bibtex` references, etc.),
- page layout inspired by Tufte and using `w3c` framework to adapt to different screen sizes,
- publication list for download produced from `bibtex` database,
- offer printable `pdf` files for all content,
- web site using multiple languages,
- content and appearances (theme) separated,
- a single `yaml` file for setup, and 
- a self-contained result which can be hosted on any web server.

## Software reuse:
Daino uses  `pandoc` and other packages on `Hackage` (e.g. shake, twitch, scotty)^[It was influenced by Chris Penner's [slick](https://github.com/ChrisPenner/slick#readme), newer, and seemingly simpler is [`Ema`](`https://github.com/srid/ema`) by  Sridhar Ratnakumar, but the documentation did not detail its features neither how it is built.].
Relies on `git` for version management.

## Example site
The code includes an example site in the `docs/site` directory^[If cloned from github; installations from hackage do not include large data directories]. [Watch it!](daino.gerastree.at). 

If `daino` is installed from `git clone git@github.com:andrewufrank/daino.git` and installed with `cabal install` or `stack install`^[Compilation and linking brings in a large number of packages, e.g. pandoc, and may take long; on a typically AMD computer 30..60 Minutes, on a ARM64 (e.g. RaspberryPi4) twice as long for the initial installation.] the test site is included in the `site/docs/dough` directory and can be run locally with `daino -qTs`, edited and rendered in a browser as `localhost:3000`.

## Running your own site
Copying the folder `site/docs/douch` to a suitable directory and edit the `settinsNN.yaml` file found there is enough to start your own site with running `daini -qs` in this directory.  

# More information: 

The following pages explain the rationale for "yet another static site generator" and show with examples how it can be used. 