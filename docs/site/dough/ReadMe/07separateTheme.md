---
title: Separate conent and presentation (aka `theme`)
abstract: The content of the web pages should be indpendent of the presentation.
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

## Presentation can be changed 

A change in the presentation style should not affect the content; for example, it was possible to move from fixed-width presentation to a presentation which adapts to different screen sized and later to use a Tufte-inspired style without touching the web page content. 

Markdown allows to structure the content with abstract hints for the presentation (e.g. title, footnote) but not fixing how these are rendered. 

## Theme directory
The instructions for presentation, the so called `theme` is in a separate directory (here `daino/docs/site/theme). It is linked automatically into the baked site. 

The theme consists of 

- fonts, preferably in the `woff` format,
- images
- cascading style sheets (CSS) in `static` folder. 

The elements are brought together with the content using the [Pandoc templates](https://hackage.haskell.org/package/pandoc-3.1.1/docs/Text-Pandoc-Templates.html).
