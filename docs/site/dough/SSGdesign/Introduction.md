---
title: 01. Introduction
#author: auf 
date: March 5, 2019
keywords: SSGdesign
abstract: Introduction to my Static Site Generator, installation and start
---

 

## My Goals with SSG
The use case is my own homepage with [requirements](Principles.html) typical for an academic researcher.
- Build a SSG from available packages in Haskell (especially pandoc).
- Reduce maintenance efforts with 
    - Demonstrate integration using the "uniform" approach to wrap packages in integratable interfaces. 
    - Streamline the design to use the least amount of packages.
 
I want a strict separation of content and presentation (dough & theme). 
The use case does not push for performance but rather simple handling and 
long term stability, with options to move the content to other tools.


## Installation and test for functionality
Clone or copy the code from [github](https://github.com/andrewufrank/SSG). 
`git clone https://github.com/andrewufrank/SSG`
Change into the `ssg` directory and install with stack `stack install` which 
produces `serversg`, the program which converts the content into a static site 
and serves it on `localhost:3001`. It watches for changes in the content files
and converts changed files automatically, keeping the static site updated 
(for most simple changes in content).

Open in your browser `localhost:3001` and you should be greeted by 

*missing screenshot*

## Build your own site

Copy the content of the `site` directory and remain it to `myhomepage` (or whatever 
directory name you fancy). In the file `settings2.yaml` adaption is possible:

- the location of folders 
- the localhost port the server is using
- the name and byline as well as the default author for the site
- the names of style files to be used
- the general menu for the site

After adaptation restart `serversg`. 
adapted to your needs
