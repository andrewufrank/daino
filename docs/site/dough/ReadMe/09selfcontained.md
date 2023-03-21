---
title: Produced web site is self-contained
abstract: The Static Site generated is selfcontained. It can be served by any web server.
author: AOS
date: 2020-06-18
keywords: SSG
language: en_US
# todo how are the languages parsed. can there be a default
# publish: true
version: publish
visibility: public
# headerShift: one todo 
---


 
The files in the `baked` directory includes everything a web server needs to access and is relocatable. It can be copied to become the web root of a server.

Any web server to which a user can upload files to the web root can be used^[I currently use a service giving my a `cpanel` to which I can upload with `ftp`; perhaps not the most convenient solution but sufficient.]
