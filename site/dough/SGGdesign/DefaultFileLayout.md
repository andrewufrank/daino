---
title: DefaultFileLayout.md
author: auf 
date: Jan. 4, 2019
keywords: SSGdesign
abstract: Layout of the SSG10 package.
---

The SGG packages includes a site, ready for deployment, but a production setup 
can separate the program package, the theme and teh content. 

## theme  folder

All the material to fix the appearance of a site are collected here. The Content is 
relatively independent from the theme - only the tags for the pieces of content to 
fill the slots in the templates must correspond. 

## the dough folder (in site) 
The dough is the content which is baked (or shaked) into the static site. 
The `dough` folder includes the `SSGdesign` folder where the documentation for SSG10 is.

## the resources folder (in site) 

## the baked folder (in site)
Baked includes the static site with an index ready to be served; it has the same file structure as `dough`.
 
