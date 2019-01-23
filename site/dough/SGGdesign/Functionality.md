---
title: Functionality.md
author: auf 
date: Jan. 4, 2019
keywords: SSGdesign
abstract: Functionality of SSG10 .
---

## bake
Converts all content in `dough` and produces the corresponding html pages in `baked`
preserving the folder structure. 

## serversg
Bundles two services: serving and rebaking
### serving baked
The server uses [Scotty]() to serve the html files in `baked` on localhost 3099 till 
the process is stopped. 
### rebaking
Watches are set on the *.md files in `dough` and on all files in `templates`; when a 
markdown file in `dough` changes, the file is baked (only this file) and therefore served
for future request (attention, browser cache may contain an old version). 
When anything in `template` changes, the site is completely baked (i.e. all pages 
are reconverted). 


