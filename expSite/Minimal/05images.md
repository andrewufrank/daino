---
title:   050withImage  
abstract: "Image referenced absolute or  relative"
# author: AOS
caption: Tal bei Obertauern
date: Jan. 4, 2019
image:  resources/120-2026_IMG.JPG 
# Blog/resources/120-2026_IMG.JPG
# should be absolute ref (with / starting)
# shown in browser
# error in pdf 
keywords: test
version: publish
# visibility: public
---

todo check how to insert linebreaks
 
The 1st image ![example absolute reference - error in PDF, shown in HTML](/Minimal/resources/1IMG.JPG)  is referenced absolutely with `/Blog/resources/1IMG.JPG`, and produces for `makePDF` an error  but the same ref is shown in HTML (understood as absolute reference relative to web root )  

test 2nd imgage ![2IMG ](Minimal/resources/2IMG.JPG): `Blog/resources/2IMG.JPG` is incorrect relative reference and therefore not shown in HTML, but shown in PDF - interpreted relative to cwd . 

test 3rd image ![relative 3IMG]( resources/3IMG.JPG):   `resources/3IMG.JPG` is  relative reference to position of .md file. shown in HTML but not in PDF . 
