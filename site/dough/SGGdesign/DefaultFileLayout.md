---
title: DefaultFileLayout.md
author: auf 
date: Jan. 4, 2019
keywords: SSGdesign
abstract: Layout of the SSG10 package.
---

The SGG packages includes a site, ready for deployment, 
but a production setup 
can separate the program package, the theme and the content; 
the path to three folders can 
be changed with `layout.YAML` file in the current working directory 
when starting with `serversg`

## theme  folder

All the material to fix the appearance of a site are collected here. 
The Content is 
relatively independent from the theme - only 
the tags for the pieces of content to file:///home/frank/Workspace8/SSG/site/dough/SGGdesign/TransformationOfPost.md

fill the slots in the templates must correspond. 

## the dough folder (in site folder) 
The dough is the content which is baked (or shaked) 
into the static site. 
The `dough` folder includes the `SSGdesign` folder 
where the documentation for SSG10 is.

### Menu
The folder structure of the dough folder gives automatically the menu structure;
it is therefore imperative to have in each folder at least one markdown text
to help users along.

### Resources
Resources with a markdown text (e.g. images) must go into the same folder
than the markdown; this makes relative references possible. Relative references 
are just the filename of the resource and are interpreted relative to the 
current page. 


## the baked folder (in site)
Baked includes the static site with an index ready 
to be served; it has the same file structure as `dough`, except that
all static files (from theme and dough) go to a `static` folder 
and are served with `//site-url/static/filename`

## code
The code is in three directories: src, test and Lib. 
Additionally there are files
- SSG.cabal
- stack.yaml
- LICENSE

 
