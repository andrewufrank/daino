---
title: Daino organizes a site as a directory tree
abstract: The web pages are collected and structured as a directory tree. 
author: AOS
date: 2020-06-18
keywords: SSG
language: en_US
# todo how are the languages parsed. can there be a default
# publish: true
version: publish
visibility: public
# headerShift: one todo 
---

# Principle: Organize site content as a directory tree

Allows reuse of software which organizes files in directories.

Each webpage in a site is written as a markdown file, which the generator transforms to a html file which can be rendered. The structure of the source (`dough`) of the web page is parallel to the directory structure of the `baked` homepage, which can be served by a web server and rendered by a browser.

The directory tree starts with the root (here `daino/docs/site/dough`) which contains all the source text for the web pages. 

## Source files are converted to HTML using Pandoc

The web page sources are translated using Pandoc to HTML and a pdf. At the moment, page sources must be written in the Pandoc markdown language, but essentially any other input Pandoc can read could be used (e.g. `latex`).

## Markdown can include images, reference etc.

Markdown allow the inclusion of images, bibliographic references etc. 

References are always `absolute` with respect to the root^[starting with a "/"] or `relative` to the current page^[not starting with "/"]. Additional files can be collected in `resources` directories^[`resources` is a reserved word; all other directory names are treated as content directories].

<!-- todo - what happens with an directory without an index file? -->

## Theme is separated from content
The instructions for presentation, the so called `theme` is in a separate directory (here `daino/docs/site/theme). It is linked automatically into the baked site. 

## The baked site is self-contained
The files in the `baked` directory includes everything a web server needs to access and is relocatable. It can be copied to become the web root of a server.
