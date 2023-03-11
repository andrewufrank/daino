---
title:   06withImage  
abstract: 06withImage   
    here is shown how to include an image in a blog  
    references can be absolute or relative
author: AUF
caption: Tal bei Obertauern
date: Jan. 4, 2019
image: /Blog/resources/120-2026_IMG.JPG
keywords: test
version: publish
visibility: public
---

# 06mimage text with an image in the banner version1234

The image is in /home/frank/Workspace11/ssg/docs/site/dough/Blog/resources/120-2026_IMG.JPG of which `/Blog/resources/120-2026_IMG.JPG` is the part **relative to the `web root`**, which is the directory `dough`. 


# Absolute reference

The image can be references absolutely with `/Blog/resources/120-2026_IMG.JPG`, for example [!example absolute reference](/Blog/resources/120-2026_IMG.JPG).

# Relative reference

The image can be referenced relatively with `./resources/120-2026_IMG.JPG` as in [!relative reference](./resources/120-2026_IMG.JPG).

# Considerations

Absolute references remain valid, even when the source for a web pages is moved to another directory. Relative references are useful, if a web page and the images referenced are in a directory and the directory as whole is move; then the relative relation between reference and referencee remains the same. 

# Images in the banner

An arbitrary image can be inserted in the banner page, as an absolute (possibly relative) reference after the keyword `image:` in the header. 

<!-- todo some hints about formating images and in the margin -->