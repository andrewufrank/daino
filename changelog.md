a static site generator
    version 0.0.2.0  used for myhomepage  - fixes
    0.0.2.1 add check for all pages, study content pdf, tex
    0.0.3.0 copy all structure (exclude with DNB)
    0.0.3.1 change to 15.13 (pandoc not resolvable)
    0.0.4   start with hpack, lts-16.0 (8.8.)
 
# to 0.0.4 
-     0.0.4   start with hpack, lts-16.0 (8.8.)
- go to hpack to produce cabal; reduces duplication in dependencies 
## 0.0.4.1
    clean read yaml and check, needs default values
## 0.0.4.2 
    recovering from 2000, rebuild Docrep, restricted use of JSON to docrep processing, moved the uniform-modules for blog inside SSG
# 0.0.4.3 
    clean up, then build test 
# 0.0.4.4
    construct the test environment, 
    generalized the bake any (aka convert any)  
# 0.0.4.5 to fix references 
    with uniformBase 0.1.2, pandoc 0.0.2.3
    pdf produced for every blog 
    made images in banner
  0.0.4.6  fixed pdf production, new flag 
  0.0.4.7  use Pandoc processCitations - for ghc 8.10.7

  0.0.4.8  reduced, removed unused code, cleaned
  0.0.4.9  change for error to use ExceptionT
  0.0.4.10  add the private/public and the completion status
                visibility (private/public) command line flag private includes the privates, otherwise only public 
                version: idea/sketch/draft/nearly/publish
                        include command line flag with these values
                        include what is more than stated 
                landing page is mostly produced from index.md in dough, needed is only top directory structure.
                        must not include the banner image (must be in settingsN.yml): difference in formatting
                        
