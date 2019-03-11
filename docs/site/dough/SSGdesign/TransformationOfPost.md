---
title: TransformationOfPost.md
author: auf 
date: Jan 27, 2019
keywords: SSGdesign
abstract: Process of transformations of input
---

The final html formated page is composed from 
- the master page template
- the html formated text content of from a page in markdown format
- the collected JSON data as key-value-pairs, which is injected in
the named slots of the master page template. 

Limitation: the html content from content pages cannot contain slots to be
filled. 

## Master page template
The master template `master.dtpl` sets the overall appearances of 
all pages in the site. It includes the 
general `style.css`, other styles are injected.
The master page includes the information for the search engines 
(author, date, keywords) tken from the key-value date.

The template is the **Master** in which slots for inclusion of values are 
marked with  `$name$` and filled with functions from 
[doctemplate](http://hackage.haskell.org/package/doctemplates).
The Master html page is only the skeleton of a html page and everything
else is injected from the key-value structure which is produced 
when transforming markdown pages. 

The names of the slots must correspond to the keys from the key-value 
pairs read in from markdown pages.


All content is injected into the page at the named slots with 
values extracted from the markdonw pages. The text content from the 
content pages is injected into the master template at the `contentHtml` slot. 
The title and subtitle are taken from the markdown file as well.


## A markdown content page

A page includes the text of the post in markdown format without a title
and some additional information as YAML key - value pairs. For example: 

```
---
title: postwk.md
author: auf 
date: Jan. 4, 2019
keywords: test
abstract: A silly text not needing an abstract.
pageTemplate : Page3
---
```

The title, keywords and abstrac are used for indexing and should therefore be given separately. 

If the author is the author of the blog, it can be filled from the general settings (see later).
The title and abstract is required for the construction a list of pages for overview; 
the date is used to sort content by date. 

The body of the page is put into the YAML key `contentHtml`

TODO: how to deal with content in other formats? 

### index pages
A page with a key `index : True` is added a menu list of the files in the current directory. 




## Repeated content in yaml files

Content, which is repeated can be inserted in the
- settnings, which applies to all pages 
- pagetype yaml pages (e.g. `blog.yaml`) which are included in
all pages which call this page type in their yaml block.

The values in these yaml files are added bottom-up and used at end to 
fill the slots of the page template; in this manner, more specific 
values always have precedence before the more general one 
(page before page type before general settings).

To the page template belongs a `pageSetting.md` which includes key-value pairs which are 
injected with the page template into the html output. 

 
 
