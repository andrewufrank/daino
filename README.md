# Static Site Generator
A static site generator from pandoc and other available packages on Hackage (e.g. shake, twitch, scotty), influenced by Chris Penner's [slick](https://github.com/ChrisPenner/slick#readme)(todo: look at [Ema](https://github.com/srid/ema) by  Sridhar Ratnakumar). It uses files to manage data to permit version management with git. Page appearances are directed with YAML and internally data is structured with JSON, for each page a PDF file is produced to allow regular looking prints. Index pages are automatically created.


# Test the result in a browser
Test with the included example site (in the `docs/site` directory) with -t switch (e.g. `cabal run ssgbake -- -t`). The result can be tested with 
- SimpleServer (must be installed from Hackage with `cabal install`) with `simpleserver -p <portnumber>` or 
- `python3 -m http.server <portnumber>`
 running in `ssg/docs/site/baked`.

## Defaults
The markdonw file for each page to produce contains in the yaml header values for title, author, date etc. Missing values are replaced with defaults, which are stored in the same format in a file.  
    Missing title is replaced by FILL - which can be searched for and corrected!
TODO

## Processing 
The design is based on Shake which is sort of lazy:

Each markdown file produces a page (correlat: for each page expected include a markdown file, even the index pages!). A markdown page starts Shake with a `need` for the html page. 
To produce html page, a panrep file must be produced, which then ask for a docrep file which is produced from the markdown file. Shake caches the intermediate files and recreates files only if the source changed, which guarantees very fast udates and allow dynamic uupdates of pages. 

### conversion functions:
- main: ssgbake (from app/ssgBake.hs)
- StartSSGprocess
    missing upload automatically TODO 
- shakeAll from Shake2.hs
- convertFiles

The code is in the subdir `ShakeBake`.

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

### MD -> Docrep: md2docrep
The md files are 
- read with `readMarkdown2docrepJSON`
- `completeDocRep` completes the meta yaml information 
- `addRefs` adds and transforms the bibliographic data 

The page is translated by `bakeOneMD2docrep`

### Docrep -> Panrep: docrep2panrep

`bakeOneDocrep2panrep` to docrep file by 
### Panref -> html: panrep2html

### Panref -> Texsnip: panrep2texsnip
-- problem the details of the call to pandoc 
-- gives latex code, but not a full file

### Panref -> Tex: tex2latex
-- wraps the head and tail around the a list of latex snips
peamble code is in ProcessPDF 

### Tex -> PDF: writePDF1


## conversions between file

hint for problems !tests/*, !unusedHaskellSources/*

changed to fourmolu for formatting

problems are 
- building the refs 

# Testing strategy for conversions
Each transformation step identified in Shake2 is used to organize the tests for the conversions. 

The tests are indexed by the transformation AtoB 
<!-- was named by A -->