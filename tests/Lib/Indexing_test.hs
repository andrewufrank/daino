{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-unused-imports #-}

module Lib.Indexing_test where

import Test.Framework
import Uniform.Test.TestHarness
import Lib.Md2doc_test  -- to have settings (i.e. layout)
 
import Foundational.Filetypes4sites
import Foundational.MetaPage
import Lib.Templating
import Lib.Indexing
import UniformBase
import Uniform.Pandoc
-- import Data.Aeson
import Uniform.Json
test_exampleTrue = assertEqual 0 0

-- read a docrec 
--"/home/frank/Workspace11/ssg/docs/site/baked/Blog/blog1.docrep" 
-- copy file manulally into .ssg

testing_getMeta f =   test1File "ssg"  (f <> "T.docrep") ("meta_"<> f) op        -- dr1 <- read8 inputFn docrepFileType
op :: Docrep -> MetaPage 
op = meta1 

test_getMeta_blog1 = testing_getMeta "blog1"
test_getMeta_index = testing_getMeta "index"


-- test_initializeIx = test1File "ssg" "meta_blog1" "ix1_blo" (initializeIndex )

-- test_completeIndex =  test1FileIO "ssg" "ix1_blog1" "ix2_blog1"
--         (completeIndex NoticeLevel0 bakedP)

-- instance ShowTestHarness Docrep
-- instance ShowTestHarness MetaPage
-- instance ShowTestHarness IndexEntry

-- obj1 = Object (fromList [("abstract",String ""),("author",String "")])

-- docrep1 = Docrep {yam = Object (fromList [("abstract",String ""),("author",String ""),("bibliography",Null),("date",Null),("dirEntries",Array []),("fileEntries",Array []),("fn",String "/home/frank/Workspace11/ssg/docs/site/dough/Blog/blog1.md"),("indexPage",Bool False),("keywords",String ""),("lang",String "DLenglish"),("link",String "Blog/blog1.md"),("publish",Null),("style",Null),("title",String "")]), pan = Pandoc (Meta {unMeta = fromList [("abstract",MetaInlines [Str "Ein",Space,Str "Blog",Space,Str "ohne",Space,Str "Sinn",Space,Str "auf",Space,Str "Deutsch"]),("date",MetaInlines [Str "2020-06-18"]),("keywords",MetaInlines [Str "Blog"]),("title",MetaInlines [Str "Mein",Space,Str "erster",Space,Str "Blog"])]}) [Header 1 ("ein-erster-abschnitt",[],[]) [Str "Ein",Space,Str "erster",Space,Str "Abschnitt"],Para [Str "Ein",Space,Str "Blog",Space,Str "ohne",Space,Str "Sinn",Space,Str "und",Space,Str "dem",Space,Str "einzigen",Space,Str "Zweck,",Space,Str "zu",Space,Str "testen,",Space,Str "wie",Space,Str "ein",Space,Str "Blog",Space,Str "in",SoftBreak,Str "ein",Space,Str "PDF",Space,Str "umgewandelt",Space,Str "wird."],Header 1 ("dies-ist-der-zweite-abschnitt",[],[]) [Str "Dies",Space,Str "ist",Space,Str "der",Space,Str "zweite",Space,Str "Abschnitt"],Para [Str "und",Space,Str "auch",Space,Str "ein",Space,Str "bischen",Space,Str "text."],Header 2 ("mit-einer-unterabschnitt",[],[]) [Str "mit",Space,Str "einer",Space,Str "unterabschnitt"],Para [Str "hier."],Para [Str "das",Space,Str "waers.",Space,Str "es",Space,Str "fehlt",Space,Str "Referenzen,",Space,Str "listen",Space,Str "und",Space,Str "aehnliches"]]} :: Docrep