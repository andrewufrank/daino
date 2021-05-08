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