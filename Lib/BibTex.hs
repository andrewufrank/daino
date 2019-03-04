-----------------------------------------------------------------------------
--
-- Module      : reading bibtex and producing the list for nocite
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}


module Lib.BibTex  -- (openMain, htf_thisModuelsTests)
     where

import Uniform.Strings
import Uniform.Error
import Uniform.FileIO

import Text.BibTeX.Parse
import Text.BibTeX.Entry as T
import qualified Text.BibTeX.Parse as Parse
import qualified Text.BibTeX.Entry as Entry
--import qualified Text.ParserCombinators.Parsec as Parsec

import qualified Data.Char as Char
--import System.IO (hPutStrLn, stderr, )


readBibTex :: FilePath ->  IO String
-- reads the bibtex file
readBibTex fp = do
    --      bib <- getContents
    bib <- readFile fp
--    putIOwords ["readBibTex", showT bib]
    return bib

parseBibTex :: String -> IO [Entry.T]
parseBibTex bib = case  parse (skippingLeadingSpace   Parse.file) "stdin" bib of
--parseBibTex bib = case Parsec.parse (Parsec.skipMany Parsec.space >> Parse.file) "stdin" bib of
         Left errMsg -> error  (show errMsg)
         Right entries -> return entries

filterByGroup :: String -> [Entry.T] -> [Entry.T]
 -- select the entries in a group
filterByGroup groupname entries = filter (elem groupname .getGroup) entries

getGroup :: Entry.T -> [String]
getGroup e = map snd groupFields
    where
        fs = fields e  :: [(String, String)]
        groupFields = filter (("groups" ==).fst) fs

getBibIdentifier :: [Entry.T] -> [String]
-- extract the bib identifier from the entries
--getBibIdentifier es = map T.entryType es
getBibIdentifier es = map T.identifier es

bibIdentifierFromBibTex :: Path Abs File -> String -> ErrIO [String]
-- combine the process
bibIdentifierFromBibTex bibfn group = callIO $
    do
        bib <- readBibTex (toFilePath bibfn)
        entries <- parseBibTex bib
        let es = filterByGroup group entries
            ids = getBibIdentifier es
        return ids


--typeTable :: [((String, Maybe String), String)]
--typeTable =
--   (("article", Just "reviewed"), "reviewedjournal") :
--   (("article", Just "popular"), "popular") :
--   (("article", Nothing), "journal") :
--   (("inproceedings", Just "reviewed"), "reviewedconference") :
--   (("inproceedings", Nothing), "conference") :
--   (("techreport", Nothing), "techreport") :
--   (("inbook", Just "program"), "program") :
--   (("misc", Just "program"), "program") :
--   (("misc", Just "talk"), "talk") :
--   (("mastersthesis", Nothing), "thesis") :
--   (("phdthesis", Nothing), "thesis") :
--   []
--
--
--cite :: Entry.T -> String
--cite entry =
--   maybe
--      "% \\nocite"
--      ("\\nocite" ++ )
--      (lookup
--          (map Char.toLower (Entry.entryType entry),
--           lookup "subtype" (Entry.fields (Entry.lowerCaseFieldNames entry)))
--          typeTable) ++
--   "{" ++ Entry.identifier entry ++ "}"
--




