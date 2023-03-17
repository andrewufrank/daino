---
title: "03witRef title: postWithReference"
abstract: |
    02withRef abstract with references from the standard bibliography.  
    Note: 

        - a title with "colon" requires quotes and 
        - how to format a list in abstract.

author: AOS
bibliography: resources/BibTexLatex.bib
nocite: |
  @navratil08
date: 2010-07-29
keywords: homepage
style: resources/chicago-fullnote-bibliography-bb.csl
styleBiber: numeric
version: publish
visibility: public
# headerShift: zero
---

# References from standard biblio

References from a bib text file which is stored in `resources/BibTexLatex.bib`, listed under `bibliography`. 

<!-- todo why no slash at start? -->

References in markdown are in `[..]` and start with `@` followed by the the bibtex-key. Examples

With a reference to [@frank09geo] which is written as "@" followed by "frank09geo".

References which should be inserted in the bibliography but are not cited in the text can be listed as `nocite`, here `@navratil08` (note the need to preceed the bibtex key with `@`). 

<!-- todo unclear what means: Not including the "TUxx9999" addition in brackets. -->

The references to [@frank-machbarkeit] and [@frank09geo] are given in the biblio and will be listed in the bibliography at the end of the text. 

The bibliography style^[Typically a `csl` file.] and the style for reference anchors inserted in the text^[Check the [Biber manual](https://texdoc.org/serve/biber.pdf/0)] can be set in the header.
<!-- not clear where styleBiber numeric comes from . -->

<!-- The references are in "resources/BibTexLatex.bib".
changed to /home/frank/Workspace11/daino/docs/site/dough/resources/BibTexExample.bib -->

