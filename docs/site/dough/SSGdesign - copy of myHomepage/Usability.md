---
title: 45. Usability
author: auf 
date: Jan. 4, 2019
keywords: UI
abstract: What goes into code to make program more usable. 
---


## Test user inputs early

User inputs - in files, on the commando line or direct input - can be wrong; 
proper identification of errors with detailed instruction where and what must 
be changed is makes programs usable. 

What can be done?

### Files
Read and parse the files as early as possible and identify problems clearly. It seems 
easier to check inputs initially and identify all shortcommings at once. If one waits
till a piece of input text is actually needed, the context which is meaningful for
the user, i.e. the file, may not be available there. 
