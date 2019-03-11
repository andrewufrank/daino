---
title: 70. PageLayout.md
#author: auf 
date: Jan. 4, 2019
keywords: SSGdesign
abstract: The layout of pages and how they adapt to screen size. 
---

Pages must be laid out to adapt to three principle screen width and to a print format. 
The print format should be produced with LaTex to give pdf output (TO DO). The screen
widht could be: 

- desktop (> 1500 pixels) 
- tablet (1500 > x > 800 pixels)
- smartphones

## Goals
Clarity - achieved by making functionality visible and providing only one way of achieving a goal.

## Desktop 
The screen is wider than what is suitable for a line of text - a break in colums necessary. 
The limitation is rather the height of the screen, require frequent scrol for longer text; 
the vertical direction is limiting and height of elements should be carefully managed. 

- menus can be vertical on the side 
- other than (short) dispatch pages no big banner at top

## Text (blog and similar) pages
Minimal top with menu at the side. Use tufte style with the emblem figure for in the margin. 

Give blogs a picture, which serves as an emblem figure and is repeated in menus etc. 

### Tufte style for text pages

Tufte's style was adapted for the web by Liepmann (see [instructions](https://edwardtufte.github.io/tufte-css/)). 
The css is copied from the [github project](https://github.com/edwardtufte/tufte-css) together with 
the font. 

See similar for R [tint](https://cran.r-project.org/web/packages/tint/vignettes/tintHTML.html)


