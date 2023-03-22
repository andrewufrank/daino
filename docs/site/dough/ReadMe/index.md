---
title: Daino 
abstract: "This ReadMe lists the principles which guided the design of `daino`" 
author: AOS
date: Jan. 4, 2019
indexPage: true
# todo is index page true necessary?
indexSort: filename
keywords: test
version: publish
visibility: public
---

Design of `daino` was guided by some principles which are explained here: 

<!-- The index pages list automatically all the directories and the markdown files (`md` extension) in the directory.

The list gives first the subdirectories and then the files. The order for each of the two groups is determined by the value given for `indexSort`. Possible sort orders are:

- `filename` 
todo filename, but actually is title
- `date` or `reversedate`

For each directory and file it gives 

- the title (from the `title` keyword^[The filename does not show in the list and can be used to achieve the desired order of items show by predeeding the filenames with numerals, e.g. 001, 003, 010...])
- the abstract (from the `abstract` keyword)
- the date (from the `date` keyword)
- the author if it is not listed as a surpressed author in the `settings` file. -->

<!-- zuerst: umlautConversion -->

<!--
scrartcl vermeiden leerseiten. Kapitel muss nicht rechts beginnen-->
<!--
pandoc --pdf-engine=lualatex -F pandoc-crossref --citeproc -o part2.pdf 1*.md part2.yaml -V mainfont="Liberation Serif" --toc --number-sections


0*.md gibt ersten teil
1*.md gibt den zweiten

add later --filter pandoc-crossref before citeproc
# to set label @ to ref label -->



<!-- process with bookletimposer 2x1
bookletimposer -a test.pdf

lp -d Kyocera_FS-1030D -o landscape -o sides=two-sided-short-edge test-conv.pdf
ACHTUNG: braucht page 1, flip on short-edge and Landscape!!
nicht mit FS1100 in pressgasse -->

<!-- --metadata date="`date +%D`"
oder -M date="`date "+%B%e, %Y"`"
https://github.com/jgm/pandoc/issüs/2865 -->

