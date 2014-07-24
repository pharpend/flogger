{-# LANGUAGE OverloadedStrings #-}

{- |
Module       : Text.Flogger.Types
Description  : Types interface for flogger
Copyright    : 2014, Peter Harpending.
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : archlinux
-}

module Text.Flogger.Types where

import           Control.Applicative
import           Data.Aeson
import qualified Data.Text as Ts
import qualified Data.Text.Lazy as Tl
import           Data.Time

-- |Data type for a blog
data Blog = Blog { blogName :: Ts.Text
                 , posts    :: [Post]
                 }

-- |Data type for a post
data Post = Post { title     :: Ts.Text
                 , date      :: UTCTime
                 , authors   :: [Author]
                 , body      :: Tl.Text
                 , published :: Bool
                 , tags      :: [Ts.Text]
                 }
  deriving (Eq, Show)

-- |Data type for an author
data Author = Author { name  :: Ts.Text
                     , email :: Ts.Text
                     }
  deriving (Eq, Show, Read)



instance ToJSON Blog where
  toJSON (Blog nom ps) = object $ [ "name" .= nom
                                  ] ++
                                  case ps of
                                    [] -> []
                                    _  -> ["posts" .= ps]

instance ToJSON Post where
  toJSON (Post ttl dt auth txt pub tgs) = object $ [ "title" .= ttl
                                                   , "date" .= show dt
                                                   , "authors" .= auth
                                                   , "text" .= txt
                                                   , "published" .= pub
                                                   ] ++
                                                   case tgs of
                                                     [] -> []
                                                     ts -> ["tags" .= ts]

instance FromJSON Author where
  parseJSON (Object v) = Author <$> v .: "name"
                                <*> v .: "email"
  parseJSON _          = fail "Author needs to be an object."

instance ToJSON Author where
  toJSON (Author nom eml) = object [ "name" .= nom
                                   , "email" .= eml
                                   ]
