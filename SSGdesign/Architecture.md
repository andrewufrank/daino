---
title: Architecture.md
author: auf 
date: Jan. 29, 2019
keywords: SSGdesign
abstract: The layered architeture for SSG
---

The architecture, i.e. the combination of implementations of functions to achieve the
overall functionality of SSG, can be cleanly layered:

## Command line processing - missing yet

## Layout

List of the directory names and locations - to give flexibility on different distribution 
of the relevant directories. It is possible to have the code, the content (dough) and 
the directory where the served files are stored in three different locations.

## Watching for changes

The use of twitch to watch for changes in the directories 
where input files exist and triggering 
the shake organized rebuilding process removes all tests for file changes 
in one point. If a change is detected, shake is called; given that shake is
only redoing what is strictly necessary and caches older results, makes false 
positives --- alerts to changes which are not substantiated --- not dangerous; 
the problem can be ignored.

## Shake for rebuilding

Shake is checking for changes in the needed input files with precisin (hask values of content)
and starts redoing what is necessary to update the result - filtering out false alerts from 
watching for changes.

For shake it is important that files wit different semantics have different extensions; 
for example, templates which must be processed by one of the template processors must be 
differentiated. Same for yaml files which have different content.

Shake is managing all filenames and calls functions in the next (sublayer). It checks for 
existence of files and produces error messages when a file is not found --- no further 
error processing for missing files neded. 

### Transformation of filepath to Path

The FilePath typed files are translated to Path type, which differentiates relative and 
absoulute path to files or directories. 

## Processing

Processing layers are split again in two: a layer to read or write files (using typed files and
typed content) before it is passed to the operations actually manipulating the data. 

## Issues : how to organize regression tests

In general, testing algebraic properties is difficult for complex data; I have a method to 
organize regression tests. Results from operations are stored and used for input later. The input
and output of the test functions are typed to avoid problems with confusion in types between 
data written to disk and read from disk. 

TODO : what would be the advantage of Typed over using Read/Write? 

The construction of a test for a function is limited as another tested function must produce the 
input data. 