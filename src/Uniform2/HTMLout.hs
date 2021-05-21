------------------------------------------------------------------------
--
-- Module      :  Uniform.HTMLout
-----------------------------------------------------------------------
-- {-# LANGUAGE BangPatterns                   #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports 
            -fno-warn-unused-imports #-}

module Uniform2.HTMLout (
    module Uniform2.HTMLout,
    -- extHTML,
    -- writeHtml5String,
    -- writerExtensions,
    -- writerHighlightStyle,
    -- WriterOptions,
    -- def,
) where

import Uniform.Json (ErrIO, ToJSON (toJSON))
import UniformBase

import Foundational.Foundation
import Foundational.MetaPage 

-- import Text.Pandoc

import Uniform.Pandoc

-- writeHtml5String2 :: Pandoc -> ErrIO HTMLout
-- writeHtml5String2 pandocRes = do
--     p <- unPandocM $ writeHtml5String html5Options pandocRes
--     return . HTMLout $ p

-- type Dtemplate = Template Text

-- applyTemplate3 :: NoticeLevel -> Path Abs File -> MetaPage -> ErrIO HTMLout

-- -- needed for old ssg lts-13.12 - also changed for 15.13

-- {- | apply the template in the file to the text
--  for help look in ssg master.ptpl as an example
--  the description are in doctemplates (on hackage)
-- -}
-- applyTemplate3 debug templName val = do
--     t1 :: Text <- readFile2 templName
--     when (inform debug) $ putIOwords ["test_readTempl", take' 300 . showT $ t1]
--     -- let t2 = read (t2s t1) :: Template Text
--     -- putIOwords ["test_readTempl Dtemplate", take' 300 . showT $ t2]
--     res2 <- applyTemplate4 (inform debug) t1 [toJSON val]
--     -- temp1 <- liftIO $ DocTemplates.compileTemplate mempty t1
--     -- -- err1 :: Either String (Doc Text) <- liftIO $ DocTemplates.applyTemplate mempty (unwrap7 templText) (unDocValue val)
--     -- let tmp3 = case temp1 of
--     --         Left msg -> error msg
--     --         Right tmp2 -> tmp2
--     -- when False $ putIOwords ["applyTemplate3 temp2", take' 300 $ showT tmp3]
--     -- -- renderTemplate :: (TemplateTarget a, ToContext a b) => Template a -> b -> Doc a
--     -- let res = renderTemplate tmp3 (toJSON val)
--     -- when False $ putIOwords ["applyTemplate3 res", take' 300 $ showT res]
--     -- let res2 = render Nothing res
--     when (inform debug) $ putIOwords ["applyTemplate3 done res2", take' 300 $ showT res2]

--     let res3 = HTMLout res2
--     return (res3 :: HTMLout)

--------------------------------------------------------HTML files

newtype HTMLout = HTMLout {contentHtml :: Text}
    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON HTMLout

-- a wrapper around html ready to publish
unHTMLout (HTMLout a) = a

htmloutFileType = TypedFile5{tpext5 = extHTML} :: TypedFile5 Text HTMLout

instance Zeros HTMLout where
    zero = HTMLout zero

instance TypedFiles7 Text HTMLout where
    wrap7 = HTMLout
    unwrap7 (HTMLout a) = a

extHTML :: Extension
extHTML = Extension "html"

-------------------- fileType --- Dtemplate---------

-- scheint nicht zu funktionieren fuer den type mit parameter?
-- type Dtemplate = Template Text

-- extDtmpl= Extension "dtpl"

-- dtmplFileType =
--   TypedFile5 { tpext5 = extDtmpl } :: TypedFile5 Text Dtemplate

-- -- data Panrep = Panrep {panyam :: Value, panpan :: Pandoc }
-- --     deriving (Eq, Show, Read )
-- -- instance Zeros Panrep where zero = Panrep zero zero

-- instance TypedFiles7 Text Dtemplate  where
--   -- handling Pandoc and read them into PandocText
--   wrap7 =  read . t2s  -- readNote "wrap7 for dtemplate 22443d" .t2s
--   unwrap7   = showT -- id -- showT
