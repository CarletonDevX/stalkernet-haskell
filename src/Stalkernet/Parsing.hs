{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}

module Stalkernet.Parsing ( people
                          ) where

import           Stalkernet.Types
import           Data.Either.Extra

import           Data.Functor
import           Control.Applicative
import           Control.Lens
import           Data.Attoparsec.Text
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import           Text.XML
import           Text.XML.Lens

-- TODO refactor

-- | Search yielded no results
badSearch :: Document -> Bool
badSearch doc = not (null (dre none)) || any (== "Note:") (dre suggestion)
  where
    dre = (doc ^..) . (.) (root . entire)
    suggestion = hasClass "searchDescription" . entire . el "strong" . text
    none = attributeIs "style" "color:red;"

-- | Number of results
results :: Document -> Either String Int
results doc = case texts of
    [] -> Left (xml ++ "Parsing.hs: results")
    ls -> mapLeft (xml ++) $ parseOnly parser (last ls)
  where
    texts = doc ^.. root . entire . hasClass "searchDescription" . text 
    xml = L.unpack (renderText def doc)
    parser =  (string " found " *> decimal <* string " matches")
          <|> (1 <$ string " found one match")

-- | Passes iff we got the right amount of people
people :: Document -> Either String [Person]
people doc =
    if badSearch doc
    then Right []
    else do
      ppl <- mapM person $ doc ^.. root . entire . hasClass "person"
      res <- results doc
      if | res == 100        -> Left "res == 100"
         | length ppl /= res -> Left "ppl != res"
         | True              -> Right ppl

-- | Convenience
hasClass :: T.Text -> Traversal' Element Element
hasClass = attributeIs "class"

-- TODO: make a monad for the folling two functions

-- | Parse a person element.
person :: Element -> Either String Person
person elt = do
    fn <- e "fn" $ elt ^? entire . el "img" . attr "alt"
    return Person
        { fullname = fn
        , picture  = ee $ el "img" . attr "src"
        , email    = ee $ attributeSatisfies "href" (T.isPrefixOf "mailto:") . attr "href"
        , role     = eitherToMaybe (roleOf elt)
        , housing  = ee $ attributeIs "title" "Find on campus map" . text
        , home     = fmap (catMaybes . map (^? _Content) . elementNodes) $ ee (hasClass "homeAddress")
        , phone    = ee $ hasClass "tel" . text
        }
  where
    e str = maybe (Left $ show elt ++ str) Right
    ee = (elt ^?) . (entire .)

-- | Parse role from person element
roleOf :: Element -> Either String Role
roleOf elt = do
    aff <- maybe (Left "no aff") Right $ elt ^? entire . hasClass "affiliation" . text
    if | aff == "Staff"   -> return $ Staff   (classText "title") (classText "dept")
       | aff == "Faculty" -> return $ Faculty (classText "title") (classText "dept")
       | otherwise -> flip Student (elt ^.. entire . hasClass "major" . text)
                       <$> parseOnly decimal aff
  where
    classText txt = elt ^? entire . hasClass txt . text

