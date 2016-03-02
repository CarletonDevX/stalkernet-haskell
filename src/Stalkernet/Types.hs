{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module Stalkernet.Types ( Person(..)
                        , Role(..)
                        , lamePerson
                        ) where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.List
import           Data.Serialize
import           Data.Serialize.Text
import           Data.Maybe
import qualified Data.Text as T
import           GHC.Generics
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

data Person = Person
    { fullname :: T.Text
    , picture  :: Maybe T.Text
    , email    :: Maybe T.Text
    , role     :: Maybe Role -- TODO
    , housing  :: Maybe T.Text
    , home     :: Maybe [T.Text] -- lines in address
    , phone    :: Maybe T.Text
    } deriving (Show, Eq, Generic)

data Role = Student Int [T.Text] -- class, major
          | Staff (Maybe T.Text) (Maybe T.Text) -- title, department
          | Faculty (Maybe T.Text) (Maybe T.Text) -- title, department
          deriving (Show, Eq, Generic)

-- DEFAULT

lamePerson :: T.Text -> Person
lamePerson name = Person name Nothing Nothing Nothing Nothing Nothing Nothing 

---------------
-- INSTANCES --
---------------

-- CEREAL

instance Serialize Person
instance Serialize Role

-- PRETTY

ttext :: T.Text -> Doc
ttext = text . T.unpack

tstring :: T.Text -> Doc
tstring = string . T.unpack

instance Pretty Person where
    pretty (Person {..}) = red (ttext fullname) <$$> indent 1 body <> hardline
      where
        body = sep . catMaybes $ map (uncurry row) pairs
        row txt = fmap (magenta (ttext txt <> colon) <+>)
        pairs = [ ("picture", fmap ttext picture)
                , ("email"  , fmap ttext email)
                , ("role"   , fmap pretty role)
                , ("housing", fmap ttext housing)
                , ("home"   , fmap (indent 1 . vsep . map tstring) home)
                , ("phone"  , fmap ttext phone)
                ]
    prettyList = sep . map pretty

instance Pretty Role where
    pretty (Staff   t f) = "Staff"   <> stafulty t f
    pretty (Faculty t f) = "Faculty" <> stafulty t f
    pretty (Student c ms) = int c <> if null ms then y else empty
      where
        y = space <> encloseSep lparen rparen (comma <> space) (map ttext ms)

stafulty Nothing  Nothing  = empty
stafulty (Just x) Nothing  = space <> ttext x
stafulty Nothing  (Just y) = space <> ttext y
stafulty (Just x) (Just y) = space <> ttext x <+> char '-' <+> ttext y

-- AESON

traverseMaybes :: [(a, Maybe b)] -> [(a, b)]
traverseMaybes = catMaybes . map sequenceA

fj :: ToJSON a => Maybe a -> Maybe Value
fj = fmap toJSON

instance ToJSON Person where
    toJSON (Person{..}) = object $
        ("fullname", toJSON fullname) : traverseMaybes pairs
      where
        pairs = [ ("picture", fj picture)
                , ("email"  , fj email)
                , ("role"   , fj role)
                , ("housing", fj housing)
                , ("home"   , fj home)
                , ("phone"  , fj phone)
                ]

instance ToJSON Role where
    toJSON role = object [("kind", kind), ("info", object info)]
      where
        similar t d =  traverseMaybes [("title", fj t), ("department", fj d)]
        (kind, info) = case role of
            Staff   t d  -> ("Staff", similar t d)
            Faculty t d  -> ("Faculty", similar t d)
            Student c ms -> ("Student", [("class", toJSON c), ("major", toJSON ms)])

instance FromJSON Person where
    parseJSON = withObject "wat" $ \obj ->
        let at :: FromJSON a => T.Text -> Parser (Maybe a)
            at = (obj .:?)
        in Person <$> obj .: "fullname"
                  <*> at "picture"
                  <*> at "email"
                  <*> at "role"
                  <*> at "housing"
                  <*> at "home"
                  <*> at "phone"

instance FromJSON Role where
    parseJSON = withObject "taw" $ \obj ->
        let at :: FromJSON a => T.Text -> Parser (Maybe a)
            at = (obj .:?)
        in do kind' <- obj .: "kind"
              info' <- obj .: "info"
              flip (withText "ffd") kind' $ \kind ->
                flip (withObject "fdd") info' $ \info ->
                  if | kind == "Staff"   -> Staff   <$> at "title" <*> at "department"
                     | kind == "Faculty" -> Faculty <$> at "title" <*> at "department"
                     | kind == "Student" -> Student <$> (info .: "class" >>= parseJSON)
                                                    <*> (info .: "major" >>= parseJSON)
                     | otherwise         -> fail "here"
