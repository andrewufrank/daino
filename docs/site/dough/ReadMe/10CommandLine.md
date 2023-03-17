---
title: "Start `daino` from the command line."
abstract: |
    Daino is a program to convert a web site in source form (`dough`) to a baked format. It runs once to convert a site completely or watches a web site source to convert files as they are changed. 
 
author: AOS
bibliography: resources/BibTexLatex.bib
date: 2010-07-29
keywords: homepage

 
version: publish
visibility: public
---
# How to run `daino`?

Daino can be compiled and installed with `cabal install daino` (or alternatively `stack install daino`). The compiled program is completely independent from the setup of a web site. 

Daino is run in the directory where the source of the web site is found; to run the test site provided with the program, run it in the directory where the source has been cloned or downloaded.

<!-- todo - only clone the doug, which simplifies  -->

The following switches are provided: 

- `-d`, `--draft` include web pages which are not marked as publish
- `-p`, `--private` include web pages which are not marked as public
- `-t`, `--test` used the material from the test site distributed with the code, start the serrver.
- `-T`, `--testComplete` delete existing baked site, recreate it from the test site and start the server.
- `-q`, `--quick` do not produce the `pdf` files (which takes time)
- `-s`, `--server` start the web server on the port set in the settings file (default 3000)
- `-w`, `--watch` watch for files changed and reconstruct them^[Some limitations: does not reconstruct files changed since last run of daino, and does not reliably include changes in file titles and abstracts, which should be included in an index page].

<!-- todo upload switch -->


<!-- todo example run output -->
 

