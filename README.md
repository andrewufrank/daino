#  Daino: A Static Site Generator
A static site generator designed by an academic to allow: 

- web pages written as [(Pandoc) markdown](https://pandoc.org/MANUAL.html#pandocs-markdown) (with YAML header for title etc.),
- use a page layout inspired by [Tufte](https://en.wikipedia.org/wiki/Edward_Tufte),
- create publication list to download copies from `bibtex` database,
- offer printable `pdf` files for all content,
- web site using multiple languages with tools to facilitate text input,
- content and appearances (theme) separated,
- a single `yaml` file for setup, and 
- a self-contained result which can be hosted on any web server.

## Software reuse:
Daino is focused on software reuse. It uses  `pandoc` and other available packages on `Hackage` (e.g. shake, twitch, scotty), 

It was influenced by Chris Penner's [slick](https://github.com/ChrisPenner/slick#readme), newer, and seemingly simpler is [`Ema`](`https://github.com/srid/ema`) by  Sridhar Ratnakumar, but the documentation did not detail its features neither how it is built.

Relies on `git` for version management.

## Installation

The code can be installed with cabal or stack from hackage. 

Compilation and linking brings in a large number of packages, e.g. Pandoc, and may take a while; on a typically AMD computer 30..60 Minutes, on a ARM64 (e.g. RaspberryPi4) four times as long for the initial installation. Rebuilding, however, is quick.

# Example site
The example site [shown here](https://daino.gerastree.at) can be downloaded or cloned   from `github` with `git clone https://github.com/andrewufrank/dainoSite.git"`.


# Running your own site
Copying the folder `dainoSite` to a suitable directory and edit the `settinsNN.yaml` file found there is enough to start your own site with running `daino -qs` in this directory. The `ReadMe` shown in a browser with `localhost:3000` includes detailed instructions! 

# Hints for development

`testRun` is set to run the active code to convert the dainoSite texts - either completely (`testNewFlag = True`) or only incrementally (`testNewFlag = False`).

    crepl testRun 
    :r
    main4Tne  

    