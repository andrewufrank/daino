---
title: Markdown as primary input format
abstract: The web pages are written as markdown text, which allows emphasis, titles, references, images, footnotes etc.
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

Markdown is an easy to learn and versatile. The list possible formatting is quite comprehensive:
    
    Markdown may not be the right format for you if you find these elements not enough for your writing: paragraphs, (section) headers, block quotations, code blocks, (numbered and unnumbered) lists, horizontal rules, tables, inline formatting (emphasis, strikeout, superscripts, subscripts, verbatim, and small caps text), LaTeX math expressions, equations, links, images, footnotes, citations, theorems, proofs, and examples. [Bookdown](https://bookdown.org/yihui/rmarkdown/#preface).

and in exceptional circumstances additional formatting tricks can be pulled in as HTML code. 

## YAML header
Markdown allows headers to pass metadata about a file (e.g. title, author) in a [YAML](https://yaml.org/spec/1.2.2/); the format is flexible^[But beware of colons, quotes etc.]. 

## Markdown can include images, reference etc.

Markdown allow the inclusion of images, bibliographic references etc. These additional files are stored in `resources` directories^[`resources` is a reserved word; all other directory names are treated as content directories]. 

<!-- todo explain images references etc -->

References are always `absolute` with respect to the root^[starting with a "/"] or `relative` to the current page^[not starting with "/"].  

<!-- todo - what happens with an directory without an index file? -->


