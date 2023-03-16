---
abstract: "Index in blog. Abstract: The directory for experiments."
author: AOS
date: Jan. 4, 2019
indexPage: true
# todo is index page true necessary?
indexSort: filename
keywords: test
title: Index in blog title primary index for Blog
version: publish
visibility: public
---

# The index for the `Blog` directory

The index pages list automatically all the directories and the markdown files (`md` extension) in the directory.

The list gives first the subdirectories and then the files. The order for each of the two groups is determined by the value given for `indexSort`. Possible sort orders are:

- `filename` 
<!-- todo filename, but actually is title -->
- `date` or `reversedate`

For each directory and file it gives 

- the title (from the `title` keyword^[The filename does not show in the list and can be used to achieve the desired order of items show by predeeding the filenames with numerals, e.g. 001, 003, 010...])
- the abstract (from the `abstract` keyword)
- the date (from the `date` keyword)
- the author if it is not listed as a surpressed author in the `settings` file.



