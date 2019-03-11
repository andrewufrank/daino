---
title: PublicationList.md
author: auf 
date: Jan. 4, 2019
keywords: SSGdesign
abstract: Explains the production of publication list. 
---

The publications are collected in a `bibtex` file (or latex?). 
The bibtex identifiers are extracted and inserted as nocite 
in markdown pages and processed regularly. 

## Bibtex

The entries in the bibtex file are marked (currently with groups); 
group `authorAF` and some other subgroups ...

## Markdown pages
For each publication list a markdown page exists in the folder 
`PublicationList`. These have special key = value pairs in the YAML block:

bibliography: the name of the bibtex file in resources
bibliographyGroup: the groupname which is used to extract the entries to produce
the publication lists. 

The entries must be prefixed with `@` for pandoc to work. 

## Processing

The processing must be in `markdownToPandoc` and before the 
pandoc processes the citations. It is in two steps:

- extract the bibtex identiiers
- prefix them with `@`
- add them with the nocite key 