---
title: Example Layout
abstract: Example Layout to show use of header levels.
author: AOS
date: 2022-02-18
keywords: layout
version: publish
visibility: public
---

## goal-section: How to use header levels
This file should show how to use the header levels in a blog. It also demonstrates the placement of title. 
This example is experimental and different from other `md` files. It uses `h1` for the title and `h2` of the sections, corresponding to `##` (and so forth). 

### HTML output
There are two conventions in the Pandoc community, depending on how the source material is initially written. Some start with the title and go to `#` for the sections, other used `#` for the title and go to `##` for the sections. Given that daino assumes that there is a `yml` header, including the title and abstract (which is useful to list the blogs in directories), the suggested standard would be the first and not as shown here. 

The decision is influencing the presentation^[The different header levels introduce different amounts of vertical space for separation] and for the output as pdf.

### PDF output
Using `#` for title gives the expected title^[The title is taken from the `yaml` header!], but the secions are numbered as "0.1 goal-section" etc. 

The alternative, with `#` for sections give regularly numbered section: "1 goal". 

<!-- Footnotes and references will be dealt with later.  -->

## titles-section
The title of is set in the header (with title:) and is used to head the full text. There is only one title text (in the header) which is used both for the html and the pdf version output.

### abstract-subsection
After the title follows the text given as abstract in the yaml header (after: abstract). Again, this is used for html and pdf produced. 

## header levels-section
The header level 1 (with on `#`) is marking sections - first subdivision. 

### subsection
Subsections are marked with two `#`.

#### subsubsections 
marked with 3 `#`. but when necessary,

##### subsubsubsections 
marked with 4 `#` are possible as are 

###### further
a further subdivision is possible with five `#` marks. 

## title numbering
The titles are not numbered. This could be changed - TODO


<!-- ![Alt text](DSC08138.JPG) -->
<!-- the next works -->
<!-- ![Alt text](./resources/DSC08138.JPG) -->
<!-- ![Alt text](resources/DSC08138.JPG) -->

<!-- statt einer relativen `resources/DSC08138.JPG` referenz. Problem in latex.

die absolute "/home/frank/Workspace11/daino/docs/site/dough/Blog/SubBlog/resources/DSC08138.JPG" funktioniert. 
der file ist "/home/frank/Workspace11/daino/docs/site/baked/Blog/SubBlog" in warum die relative nicht?

  An example post sorted at last
  and an image -->