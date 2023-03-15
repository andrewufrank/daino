---
title: Examples of the markdown syntax which can be used in SSG
abstract: "List of available markdown syntax extensions: footnotes, inline notes, fenced code blocks."
author: AOS
date: 2022-02-18
keywords: layout
version: publish
visibility: public
bibliography: resources/BibTexLatex.bib
headerShift: 1
---

# goal
List the special syntax for references. Useful is probably also the description of [MultiMarkdown](https://fletcherpenney.net/multimarkdown/cms/), which are mostly enabled in Pandoc, sometimes with slightly improved syntax.

# Footnotes
A footnote is marked as `[^id]` and it's text as `[^id] : text`. 

Pay attention to the colon[^e] immediately after the closing bracket.

[^e]: This is an example footnote.

Alternatively, a footnote can be written in-line with the text as `^[text]`, for example^[inline footnote example]. Attention: no space between the last word and the `^`!

# Text ornaments
## Super and subscripts

The format is `^2^` and `~2~` for example x^2^ and H~2~O.
<!-- The format is `^2` and `~2` for example x^2 and H~2O. -->

## Strikeout 
To strike out text, surround it with `~~`. For example ~~this is crossed out~~. [^note]

[^note]: Note for coding Pandoc Extensions: 

    Attention to the interaction between strikeout and subscript, probably also between carets for footnotes and superscript.  The current set is: should be wrapped in `.blockquote`
    ```` haskell
        exts = mconcat
            [ Pandoc.extensionsFromList
                [ Pandoc.Ext_yaml_metadata_block
                -- , Pandoc.Ext_fenced_code-block -- code blocks with ~
                , Pandoc.Ext_backtick_code_blocks
                , Pandoc.Ext_fenced_code_attributes  -- eg for haskell code snippets
                , Pandoc.Ext_auto_identifiers
                -- , Pandoc.Ext_raw_html   -- three extension give markdown_strict
                , Pandoc.Ext_raw_tex   --Allow raw TeX (other than math)
                , Pandoc.Ext_shortcut_reference_links
                , Pandoc.Ext_spaced_reference_links
                , Pandoc.Ext_footnotes  -- all footnotes
                , Pandoc.Ext_inline_notes
                , Pandoc.Ext_citations           -- <-- this is the important extension for bibTex
                , Pandoc.Ext_implicit_figures  -- a figure alone in a para will have a caption
                , Pandoc.Ext_header_attributes -- for {.unnumbered}
                , Pandoc.Ext_lists_without_preceding_blankline
                , Pandoc.Ext_superscript  -- start and closing ^
                , Pandoc.Ext_subscript -- start and closing ~
                -- , Pandoc.Ext_short_subsuperscripts  -- only start ^ and ~
                , Pandoc.Ext_strikeout  -- require ~~ two! before and after
                ]
            , Pandoc.githubMarkdownExtensions
            ]
    ````

# Fenced code blocks

Blocks of code can be marked with 4 backquotes, followed by the name of the language which should be used for rendering and closing with 4 backquotes. Example: 

```` haskell
type Pair = (Text, Value)

v1 = ["name" .= t1, "age" .= t2] :: [Pair]
v2 = ["name" .= t1, "age" .= t3] :: [Pair]
````
# References included in text

An example reference to `[@frank09geo]`; are references includes the BibTex-key preceeded by `@` in `[]`. References are translated if the bibliography is included in the YAML head of the file. Example [@frank09geo].

A reference section must be included at the end of the file; it will be automatically filled.


# Reference (produced)
