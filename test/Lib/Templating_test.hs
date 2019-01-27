-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans #-}

module Lib.Templating_test  -- (openMain, htf_thisModuelsTests)
     where


import           Test.Framework
import Uniform.Test.TestHarness
import Lib.Foundation (progName, templatesPath)
import Uniform.Strings
import Lib.Templating -- (applyTemplate2, convGmaster)
import Uniform.Filenames
import Lib.FileMgt
import qualified Text.Glabrous as G
import Text.Glabrous (Template, insert) -- , insertMany)
import Text.DocTemplates

temp1 = "some start {{template2}} and some more text."
temp2 = "xxx and yyy"

t1 = fromRightNoteString "t1 rwe2" . G.fromText $ temp1
t2 = fromRightNoteString "t2 wwer2" . G.fromText $ temp2

c1 = G.fromList [("template2",temp2)] :: G.Context

res1 = G.process  t1 c1  :: Text
--res2 = G.insert   t1  "template2" t2   :: Maybe G.Template

--test_glab1 = assertEqual "some start xxx and yyy and some more text." ( res1)
--
------------- test with actual doc templates
--
--test2 = do
--    fm   <- readFile  (addDir templatesPath testMaster :: Path Abs File)
--    fp   <- readFile  "/home/frank/Workspace8/SSG/theme/templates/Page2.html"
--
--    let
--        mt = fromRightNoteString "fawer" . G.fromText . s2t $ fm
----        pt = fromRightNoteString "fawer" . G.fromText . s2t $ fp
--        c1 = G.fromList [("body", s2t fp)] :: G.Context
--        res1 = G.process  mt c1  :: Text
--    return res1
--
--test_glab2 = do
--                r <- test2
--                assertEqual  rx2  r
--
--rx2 = "<!DOCTYPE html>\n<!-- a master page for the pandoc templating mechanis -->\n<html lang=\"$lang$\">\n  <head>\n    <meta charset=\"utf-8\" />\n    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0, user-scalable=yes\" />\n    $for(author)$\n      <meta name=\"author\" content=\"$author$\" />\n    $endfor$\n    $if(date)$\n      <meta name=\"dcterms.date\" content=\"$date$\" />\n    $endif$\n    $if(keywords)$\n      <meta name=\"keywords\" content=\"$for(keywords)$$keywords$$sep$, $endfor$\" />\n    $endif$\n    <title>$if(title-prefix)$$title-prefix$ \8211 $endif$$pagetitle$</title>\n  </head>\n \n    <body>\n    <!-- AF - my template for pandoc (from the default) -->\n        $for(include-before)$\n        $include-before$\n        $endfor$\n\n        $if(title)$\n        <header>\n        <h1 class=\"title\">$title$</h1>\n        $if(subtitle)$\n        <p class=\"subtitle\">$subtitle$</p>\n        $endif$\n\n        $for(author)$\n        <p class=\"author\">$author$</p>\n        $endfor$\n        $if(date)$\n        <p class=\"date\">$date$</p>\n        $endif$\n        </header>\n        $endif$\n\n        $if(contentHtml)$\n        $contentHtml$\n        $endif$\n\n        $if(toc)$\n        <nav id=\"$idprefix$TOC\">\n        $table-of-contents$\n        </nav>\n        $endif$\n        \n        $body$\n    \n        $for(include-after)$\n        $include-after$\n        $endfor$\n    </body>\n\n\n \n</html>\n"

--testPage = (makeRelFile "Page3")
--testMaster = makeRelFile "Master3"
--testMaster_Page = makeRelFile "Master2_Page2"
--
--test_convMaster2 = do
--           res <- runErrorVoid $ putPageInMaster templatesPath
--                     testPage2 testMaster
--                    "body" testMaster_Page
--           assertEqual () res

test_convMaster3 = do
           res <- runErrorVoid $ putPageInMaster templatesPath
                     (makeRelFile "Page3") (makeRelFile "Master3")
                    "body" (makeRelFile "page33")
           assertEqual () res

--test_readPage2 = do
--            res <- (runErrorVoid $ do
--                            fp :: Dtemplate <- read7 templatesPath testPage2 dtmplFileType
--                            return ()
--                    )
--            assertEqual () res

--applyTemplate2x :: DocValue -> ErrIO HTMLout
---- apply the template in the file to the text
--applyTemplate2x val =
--     case applyTemplate2 (addDir templatesPath (makeRelFile "page33"))  (unDocValue val) of
--                    Left msg -> throwError  . s2t $ msg
--                    Right val2 -> return  . HTMLout $  (val2 :: Text)
--
--test_templating_11_E_G, test_templating_12_E_G :: IO ()
--test_templating_11_E_G = test1FileIO progName   "resultBE11" "resultEG11"  (applyTemplate0 rx2)
--test_templating_12_E_G = test1FileIO progName   "resultBE12" "resultEG12"  (applyTemplate0 rx2)


applyTemplate2x :: DocValue -> ErrIO HTMLout
applyTemplate2x = applyTemplate2   templatesPath (makeRelFile "page33")
--                        (makeRelFile "pandocDefault.html"::Path Rel File)


test_templating_11_E_F, test_templating_12_E_F :: IO ()
test_templating_11_E_F = test1FileIO progName   "resultBE11" "resultEF11"  applyTemplate2x
test_templating_12_E_F = test1FileIO progName   "resultBE12" "resultEF12" applyTemplate2x

instance  ShowTestHarness DocValue where
instance ShowTestHarness HTMLout
--
--
fromRightNoteString ::   Text -> Either String b -> b
fromRightNoteString msg (Left a) = errorT ["fromRight", showT a, msg]
fromRightNoteString _ (Right a) = a
