---
title: 20. DefaultFileLayout.md
#author: auf 
date: March 5, 2019
keywords: SSGdesign
abstract: Layout of the files used to construct a site.
---

The SGG packages includes a site, ready for deployment, 
but a production setup 
can separate the *program package*, the *theme* and the *content*; 
the path to these folders can 
be changed with `settings2.yaml` file in the site directory 
when starting the `serversg`, `serverbake` and the `ssg10` program.

## Site directory
The site directory is the directory in which the `settings2.yaml` file is located.
It is usual to have "dough" (i.e. the content) in subdirectories of the site directory, 
but other locations can be selected (in `settings2.yaml`). 

## theme  folder

All the material to fix the appearance of a site are collected here. 
The content is 
relatively independent from the settings in the theme.
The theme includes the templates which are eventually filled with content; 
the labels which determine where content must be injected must correspond 
to the tags for the pieces of content (see
[How a content is transformed](TransformationOfPost.html).  
The default theme folder is in the same folder as the code because
some keywords in the theme files and in the code must correspond.
 
## Main Menu
The menu structure is in the settings2.yaml file as a YAML structure, with link and text fields.
Links contain relative links to the index.html files in the folders,
the visible explanations.

## the dough   (in the site folder) 
The dough is the content which is baked (or shaked) 
into the static site.

### Landing page 
The `landingPage.md` in the site folder is the text for the title page of the site; 
other pages are in directories (one level only ??).

The landing page typically contains a general introduction and links to the major pieces - possibly with some 
explanation.
 


### Resources
The `resources` folder in the dough directory contains all supporting data
which is used in with a markdown text (e.g. images, BibTex files) must go into the same folder
than the markdown; this makes relative references possible. Relative references 
are just the filename of the resource and are interpreted relative to the 
current page. 

Question: should resources which must be copied to static and resources
which are only used during text preparation be separated?


## the baked folder (the html files served)
The baked folder includes the static site with an index ready 
to be served; it has the same file structure as `dough`, except that
all static files (from theme and dough) go to a `static` folder 
and are served with `//site-url/static/filename`

## code
The code is in three directories: src, test and Lib. 
Additionally there are files
- SSG.cabal
- stack.yaml
- LICENSE

 
