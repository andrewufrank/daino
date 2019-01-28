---
title: TransformationOfPost.md
author: auf 
date: Jan. 27, 2019
keywords: SSGdesign
abstract: Process of transformations of input
---

The transformation of a `examplePage.md` is in several steps.
The body and yaml values extracted from the markdonw text of the *content* 
is injected in a template (a html structure with variables).
The template is produced from two html templates, 
a *master* and a *page* which can be combined 
first and then the body from the post is injected.
Additionally setting YAML values are spliced after the *post* to 
supplement the YAML values of the post. 

# A markdown content page

A page includes the text of the post in markdown format and
some additional information: 


TODO: how to deal with content in other formats? 
 
