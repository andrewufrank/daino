---
title: ReservedWords.md
author: auf 
date: Jan. 29, 2019
keywords: SSGdesign
abstract: The layered architeture for SSG
---

A number of words are fixed in the code or in the themes and must be used as given here. 
Changing is possible, but requires careful checking and possibly recompilation of the code.

## Settings page
The settings page must include the location of the files and the port to use; the keys are 
(with examples)

```
storage:
    themeDir:  /home/frank/Workspace8/ssg/theme
    doughDir: /home/frank/Desktop/myHomepageSSG/
    bakedDir: /home/frank/Workspace8/myHomepageSSG/
    reportFile: /home/frank/myhomepageSSGreport.txt
    testDir: /home/frank/SSGtest
localhostPort: 3001
```
Other keys used: 

- for style: `quotes, css, `
- default values: `author, date`
- setting the `sitename` and the `byLine`

The menu is structured as (with examples 
```
menu:
     - link: /Blog/index.html
       text: Blog

```

## File and directory names:
Filenames fixed are: 
- settings2.yaml
- landingPage.md 
- index.md (must be present in any directory (including the root=dough) 
unless the directory has another page which is called from the menu
-resources
- static (used in baked, not in dough)

## Directory names in themes
There must be directories:
- templates
Other names follow from the placeholders in the page templates (`.dtpl` files): 
- menu.css
- style.css
- img directory where the image at the top is held


## filenames
-masater4.dtpl
- settings2.yaml 
- 