---
title: Layout with multiple pictures
abstract: Example Layout to show how to use include multiple pictures, one in the banner and two included with text.
author: AUF
date: 2022-02-18
keywords: layout
version: publish
visibility: public
image: /resources/templates/img/DSC05127.JPG
caption: Garden in Geras.
---

# goal
The file shows how to include pictures. 

It includes a picture replacing the banner (above) and two pictures 
as added in the text wtih the regular methods of markdown. 

# picture location
The pictures are stored in the global resource section, i.e. `/resources/templates/img/geras4013.JPG`, which is not generally recommend. Pictures should be stored in the *local* resource section in the same directory than the file because this allows to check which resources the files in a directory use and allows to move directories as wholes.

The pictures are in the global resource section, because they are reused in the test site multiple times. 

# a picture in the text. 
The picture in the text is called with the regular markdown syntax `![Street in Vienna](image location)` but is probably better included in a footnote[^pic1] to move to the side; captions can be set automatically[^cap]. 

[^pic1]: ![Street in Vienna](/resources/img/134-3437_IMG.JPG)

[^cap]: A picture alone in a paragraph includes the caption automatically!

# picture in the banner
The picture at the top of the blog is called from the yaml header of the file and replaces the standard banner. It is inserted in the yaml header with the label `image`. 

# another picture in the text
without much text ![Three girls and dog](/resources/img/121-2128_IMG.JPG)

# Picture placement

The intention is that pictures are include in the flow of the text regularly, but if the screen is wide, they should be shown adjacent to the text on the right. 
