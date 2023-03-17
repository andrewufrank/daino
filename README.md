# Daino is a Static Site Generator

It started as a test to construct a static site generator from pandoc and other available packages on Hackage (e.g. shake, twitch, scotty). It uses direcdtory and subdirectories to manage the files and structure the site  (1:1). In addition to the `HTML` files rendered by a browser, for each page a printable 'pdf` is produced. The web pages are written in (Pandoc) markdown and the site version management can be done withh   git. Page appearances are controlled with YAML headers in files. Internally data is structured with JSON. 

The code can be build with `cabal daino install` and execute with `daino -tqs` which builds quickly a test site and starts a web server. Open with `localhost:3000`; it explains the design and how to adapt it with examples. 
