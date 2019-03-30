---
title: Functionality.md
author: auf 
date: Jan. 4, 2019
keywords: SSGdesign
abstract: Functionality of SSG10 .
---

## ssgbake
Converts all content in `dough` and produces the corresponding html pages in `baked`
preserving the folder structure. 
Uses a simple shake structure and copies static content wholesale; no watches.
Starts a web server automatically. 

Used to clarify the code. 

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

The command line arguments are a number of switches:

- -p --publish : include the md files marked publish (or not marked)
- -d --draft : include the md files marked draft
- -o --old : include the md files marked old

Only the md files for which the switch is set are included, default is nothing!

There is an additional switch 

- t --test : include the md files marked publish and use the settings.yaml file for using 
the test data in the package (in docs/site/dough).

The publication markings are in the Yaml part of the md files (in the field "publication:"). 
