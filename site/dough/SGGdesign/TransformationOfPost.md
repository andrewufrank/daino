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

A page includes the text of the post in markdown format ithout a title
and some additional information as YAML key - value pairs. For example: 

```
---
title: postwk.md
author: auf 
date: Jan. 4, 2019
keywords: test
abstract: A silly text not needing an abstract.
---
```

If the author is the author of the blog, it can be filled from the general settins (see later).
The title and abstract is required for the construction a list of pages for overview; 
the date is used to sort content by date. 

The body of the page is put into the YAML key `contentHtml`



TODO: how to deal with content in other formats? 

# The page template

The content is injected into a template, which is combined of two templates: 
- a page template which gives specifics of the style for this page,
- a master template which fixes the common aspects of all pages in the site.

The page template takes the title and the body (in `contentHtml`) from the markdown file. 

To the page template belongs a `pageSetting.md` which includes key-value pairs which are 
injected with the page template into the html output. 

It contains:  TODO

# The master template

The mastertemplate sets the overall appearances of all pages in the site. It includes the 
general `style.css`.
To the master templae belongs a `masterSetting.md` which contains the name of the site etc. 
 
