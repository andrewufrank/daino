---
title: Daino organizes a site as a tree
abstract: The web pages are   structured as a tree and collected in a directory tree. 
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

# Principle: The structure of the site and the structure of its stored representation should correspon

A web site is presented as pages of hyper-text with links between the pages. This logical structure is represented as links between files and the whole site is collected under a root directory. 

## Each web page is stored as a markdown file 

The site generator process converts the markdown file (`md` file) to a HTML file a browser can render. 

Each webpage in a site is written as a markdown file, which the generator transforms to a html file which can be rendered. The structure of the source (`dough`) of the web page is parallel to the directory structure of the `baked` homepage, which can be served by a web server and rendered by a browser.

The directory tree starts with the root (here `daino/docs/site/dough`) which contains all the source text for the web pages. 
Allows reuse of software which organizes files in directories.

