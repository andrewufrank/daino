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

# Principle: The structure of the site and the structure of its stored representation should correspond

A web site is presented as pages of hyper-text with links between the pages. This logical structure is represented as files   and the whole site is collected under a root directory. 

The mapping between rendered web pages and the files representing them is crucial in the design:

**Each web page^[i.e. non-additional material page] is stored as a markdown file.** 

The site generator process converts each markdown file (`md` file) to a HTML file a browser can render. 

Each web page in a site is written as a markdown file, which the generator transforms to a html file which can be rendered. The structure of the source (`dough`) of the web page is parallel to the directory structure of the `baked` homepage, which can be served by a web server and rendered by a browser.

A markdown page can call for **additional material** and link to other renderable pages not produced from a markdown page.

## Tree structure

The web site starts with a single page^[Often called `landing page`.] from which all other pages can reached in a tree structure. 


The web pages are stored as files in  directories. The directory tree starts with the root (here `daino/docs/site/dough`) which contains all the source text for the web pages^[It contains an additional file `settingsNN.yaml` for the site].

Directories store only files and additional information for the presentation of the directory as web page is necessary. For each directory an `index.md` file is added which inidcates how the directory information is commented and rendered.

Additional content can be stored in `resources` directories^[Which must be called `resources`!]

## Correspondence between presentation and storage 

The  source for web pages, and the web pages in `HTML` formate are stored in a parallel directory structure and correspond to the structure of the web site visible to the user are parallel. 

