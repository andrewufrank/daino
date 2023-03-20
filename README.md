#  Daino: A Static Site Generator
A static site generator designed by an academic to allow: 

- web pages written as [(Pandoc) markdown](https://pandoc.org/MANUAL.html#pandocs-markdown) (with YAML header for title etc.),
- use a page layout inspired by [Tufte](https://en.wikipedia.org/wiki/Edward_Tufte),
- create publication list to download copies from `bibtex` database,
- offer printable `pdf` files for all content,
- web site using multiple languages,
- content and appearances (theme) separated,
- a single `yaml` file for setup, and 
- a self-contained result which can be hosted on any web server.

## Software reuse:
Daino is focused on software reuse. It uses  `pandoc` and other available packages on `Hackage` (e.g. shake, twitch, scotty)^[It was influenced by Chris Penner's [slick](https://github.com/ChrisPenner/slick#readme), newer, and seemingly simpler is [`Ema`](`https://github.com/srid/ema`) by  Sridhar Ratnakumar, but the documentation did not detail its features neither how it is built.] 

Relies on git for version management.

# Example site
The code includes an example site in the `docs/site` directory. [Watch it!](daino.gerastree.at). 

If `daino` is installed from `git clone git@github.com:andrewufrank/daino.git` and installed with `cabal install` or `stack install`^[Compilation and linking brings in a large number of packages, e.g. pandoc, and may take long; on a typically AMD computer 30..60 Minutes, on a ARM64 (e.g. RaspberryPi4) twice as long for the initial installation.] the test site is included in the `site/docs/dough` directory and can be run locally with `daino -qTs`, edited and rendered in a browser as `localhost:3000`.

# Running your own site
Copying the folder `site/docs/douch` to a suitable directory and edit the `settinsNN.yaml` file found there is enough to start your own site with running `daini -qs` in this directory.  

