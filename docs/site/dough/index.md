---
title: Landing page for "Example homepage"
abstract: "file: index.md in `dough` directory."
author: Author of Settings
date: 2010-01-29
keywords: homepage
# page-title: ExampleHomepage
version: publish
visibility: public
---
This is the `index page` at the root of the site. This page is opened by a browser by default for the URL of the site; it is often called `landing page`. 

The site is organized in tree of directories, which contain all the markdown files (extension `md`, the `dough`) which the generator converts to HTML files (extension `html`, the baked pages)^[Each `md` file produces exactly one `html` file, with the same directory structure]. The source directory can include other files (e.g. images, html) which a browser can render, typically referenced from an `md` page. 

Every directory and subdirectory must include a descriptive `index.md` file which is automatically completed with the list of subdirectories and files in this directory.

The landing page, in the root directory of the site, gives:

- a  brief description of the site (produced by the site author as markdown text in the file), and
- an automatically produced list of the subdirectories and pages (i.e. `md` files) in the directory as clickable (navigable) links. 

It shows the default images in the banner (the file listed in the `settings` page as `banner`, the site name and the site byline). 
Under the banner image a ribbon gives links to the top level subdirectories (as given in the `menuitems` entry  in the `settings` page) and a link to the top level index. This ribbon is added to each page. 

<!-- todo add a return button -->

At the bottom of each page the name of the `html` file shown, which is typically the same file name as the name of the `md` file from which the `html` is produced (the `html` is in the `baked`, the `md` in the `dough` directory).

The pages under `Blog` give examples for different case of web pages and how they can be expressed in markdown. They are all written in the [Pandoc markup language](https://pandoc.org/MANUAL.html#pandocs-markdown) and include hints how it can be used in conjunction with `daino`, the Static Site Generator (daino).. 