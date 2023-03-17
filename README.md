# Static Site Generator
A static site generator focused on software reuse. It uses  pandoc and other available packages on Hackage (e.g. shake, twitch, scotty)/ It was influenced by Chris Penner's [slick](https://github.com/ChrisPenner/slick#readme), newer, and seemingly simpler is `Ema` ^[`https://github.com/srid/ema` by  Sridhar Ratnakumar] but the documentation does not detail its features neither how it is built. 

daino uses files to manage data to permit version management with git. Web sources are written in markdown language and converted with pandoc. Page appearances are directed with YAML. Unlike other Site Generators, for each page a PDF file is produced to guarantee well formatted prints. 

The conversion is managed by shake to avoid unnecessary conversions. Using the `inotify` mechanism, changed pages are detected and reconverted, achieving a quasi dynamic site.
Index pages in subdirectories are automatically formatted, but minimal explanations are recommended.

# Example site
The code includes an example site in the `docs/site` directory. It contains a file `settingsNN.yaml` which describes the layout of the site. 

<!-- The special directory `resources` is used to keep style (`csl`) and bibliography (`bib`) files. In blogs, they are referenced with relative path, but including `resources/...`. For the style, the default `resources/chicago-fullnote-bibliography-bb.csl` is set if nothing else given. -->

# Separation content and theme
in the `docs` directory is the separated `theme` directory, which determines the appearance of the site. There must be a link to the theme directory in the dough directory 

<!-- (TODO remove the needs). -->

# Testing

Install `daino` with `cabal install daino` and run it with the example site included with `daino -ts` for `test` and `server`, which converts the test site and starts a web server.  The result can be viewed in the broswer at `localhost:3000`^[A different port can be set in the settings file.]

# Build your own site!

Copy the contents from the `dough` directory and the `settings` file to a directory (e.g. `myHomepage`). Adapt the directory names in the settings file and run `daino -qs` and open a `localhost:3000`. 

Your skeleton webpage is shown! 

The test site contains under `ReadMe` and `Blog` more detailed instructions with examples.

<!-- Thisselects the settingsfile in the example test site.  -->

# Test site
Test with the included example site (in the `docs/site` directory) and the provided `theme` can be extended to include all troublesome cases. The tests are executed with `-t` switch (e.g. `cabal run daino -- -t` or easier `cabal instal daino && daino -t). A html server (scotty) is started; the result can be viewed in the broswer at `localhost:3000` (the port can be selected in the `settings` file. 

Alternatively, the resulting site can be tested in the browser with 
- SimpleServer (must be installed from Hackage with `cabal install` - not yet on 8.10 with base 4.14) with `simpleserver -p <portnumber>` or 
- `python3 -m http.server <portnumber>`
        running in `daino/docs/site/baked`.

## Defaults
The markdonw file for each page included in the site must  contain in the yaml header values for title, author, date etc. Missing values are replaced with defaults, which are stored in the same format in a file.  
    Missing title and author is replaced by `FILL` - which can be searched for and corrected!


## Processing 
The design is based on Shake which is lazy:

Each markdown file produces a page (correlate: for each page expected include a markdown file, even for the index pages!). A markdown page starts Shake with a `need` for the html page. 
To produce html page, a panrep file must be produced, which then ask for a docrep file which is produced from the markdown file. Shake caches the intermediate files and recreates files only if the source changed, which achieves very fast udates and allow dynamic uupdates of pages. 

From each markdown page a `.pdf` file is produced. The conversion/transformation progresses in **Waves** and code is kept in modules which each cater for a wave.

### Organising Shake:
- main: daino (from app/daino.hs)
- Startdainoprocess
    missing upload automatically TODO 
- shakeAll from Shake2.hs
- convertFiles

The code is in the subdir `ShakeBake`.

## Waves: Transformations of pages for the site
- `md`: The each page shown on the site starts as an markdown file with yaml meta information. 
- `docrep`: the pandoc format of the page plus the completed metadata (DocrecJSON meta in json form, Docrep as record)
        <!-- DROP ?? docrep and produce directly panrep -->
    - bakeOneMD2docrep
        - readMarkdown2docrepJSON (the result from pandoc)
        - completeDocRep (complete with defaults, hardcode TODO)
        - addRefs

- `panrep`: Input format for pandoc with metadata as record; same format as docrep, but index completed
- `html`: a page for the browser to show
- `texsnip`: intermediate format of a part of a page, contains the metapage (yaml metadata)
- `tex`: a tex file for a page
- `pdf`: a printable page

### Wave MD -> Docrep: md2docrep
The md page is translated by `bakeOneMD2docrep`.

The md files are 
- read md file with `readMarkdown2` to pandoc (processing of Metadata in Yaml, conversion of text to pandoc)
- `pandoc2docrep` produces `MetaPage` with all meta data, collects the data for the index
- `addRefs` adds and transforms the bibliographic data 


### Wave Docrep2html
#### Docrep -> Panrep: docrep2panrep
conversion of record (but same content), then complete index.

`bakeOneDocrep2panrep` to docrep file by 
#### Panrep -> html: panrep2html

### Wave Panrep2pdf
### Panrep -> Texsnip: panrep2texsnip
-- problem the details of the call to pandoc 
-- gives latex code, but not a full file

### Texsnip -> Tex: tex2latex
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

# Compilation - Build - Run 
## Compile

run hpack to make sure cabal file is updated
With 
    *cabal build* in the daino directory 
    *cabal run ssbbake -- -t* to run on test data 
    *cabal install* to make it usable in other directories

    uses 
    - daino.cabal
    - *cabal.project*, updated to use the uBase as moved to Hackage

    check with *ghcup tui* which version is installed - currently 8.10.4 (ghcup -h)

    There are changes 
        - uniformBase 
        - uniform-strings
        - uniform-pandoc (only debug output)
## test with testsite
- move to daino
- cabal run daino -- -t (this can be repeated)
- cd docs/site/baked
- python3 -m http.server 3000 (once after reboot)
- in browser: localhost:3000

## run on myhomepage (in the folder)

- there must be a file settingsN.yaml in the folder with the dough 
(except of the test homepage, where the settings3.yaml is in the outer dir *site* and not in *site/dough*)
- in LayoutFlags.hs is the current name as "settings3"
- daino 

# protection

It is possible to protect some parts of the site with passwords. A regex expression can be used to identify the URIs which should require a password. The NGINX config file looks like:

````
#  setup for local web server 
map $uri $realm {
        ~^/Reserved/  "Protected area";
# wildcards only with regex!
#       /Reserved/*     "Username and 11 Password required (heiden.gerastree.at)";
        default                 off;
}

server {
    server_name heiden.gerastree.at;

    root /var/www/html/homepage;

#    auth_basic "Password required";
    auth_basic $realm;
    auth_basic_user_file /etc/nginx/.htpasswd1;

        # Add index.php to the list if you are using PHP
        index index.html index.htm index.nginx-debian.html;

        location /homepage {
                # First attempt to serve request as file, then
                # as directory, then fall back to displaying a 404.
                try_files $uri $uri/ =404;
        }

     access_log /var/log/nginx/heiden.log;

    listen [::]:443 ssl ipv6only=on; # managed by Certbot
    listen 443 ssl; # managed by Certbot
    ssl_certificate /etc/letsencrypt/live/heiden.gerastree.at/fullchain.pem; # managed by Certbot
    ssl_certificate_key /etc/letsencrypt/live/heiden.gerastree.at/privkey.pem; # managed by Certbot
    include /etc/letsencrypt/options-ssl-nginx.conf; # managed by Certbot
    ssl_dhparam /etc/letsencrypt/ssl-dhparams.pem; # managed by Certbot


}
````