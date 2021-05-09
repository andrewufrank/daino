# Static Site Generator

A test to construct a static site generator from pandoc and other available packages on Hackage (e.g. shake, twitch, scotty). It uses files to manage data to permit version management with git. Page appearances are directed with YAML and internally data is structured with JSON. 

# Test the result in a browser
Test with the included example site (in the `docs/site` directory) with -t switch (e.g. `cabal run ssgbake -- -t`). The result can be tested with 
- SimpleServer (must be installed from Hackage with `cabal install`) with `simpleserver -p <portnumber>` or 
- `python3 -m http.server <portnumber>`
 running in `ssg/docs/site/baked`.

## Defaults
The markdonw file for each page to produce contains in the yaml header values for title, author, date etc. Missing values are replaced with defaults, which are stored in the same format in a file.  
TODO

## Transformations of pages for the site
- `md`: The each page shown on the site starts as an markdown file with yaml meta information. 
- `docrep`: the pandoc format of the page plus the completed metadata (DocrecJSON meta in json form, Docrep as record)
        DROP ?? docrep and produce directly panrep
    - bakeOneMD2docrep
        - readMarkdown2docrepJSON (the result from pandoc)
        - completeDocRep (complete with defaults, hardcode TODO)
        - addRefs

- `panrep`: Input format for pandoc with metadata as record
- `html`: a page for the browser to show
- `pdf`: a printable page
- `texsnip`: intermediate format of a part of a page
- `tex`: a tex file for a page

### MD -> Docrep
The md files are 
- read with `readMarkdown2docrepJSON`
- `completeDocRep` completes the meta yaml information 
- `addRefs` adds and transforms the bibliographic data 

The page is translated by `bakeOneMD2docrep`



`bakeOneDocrep2panrep` to docrep file by 
    - 
## conversions between file

hint for problems !tests/*, !unusedHaskellSources/*