{-# LANGUAGE OverloadedStrings #-}

{- |
Module       : Text.Flogger.JSON
Description  : JSON interface to flogger
Copyright    : 2014, Peter Harpending.
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : archlinux
-}

module Text.Flogger.JSON where

import           Control.Applicative
import           Data.Aeson
import           Text.Flogger.Pure


instance FromJSON Blog where
  parseJSON (Object v) = Blog <$> v .: "name"
                              <*> v .:? "posts" .!= []
  parseJSON _          = fail "Blog needs to be an object."

instance ToJSON Blog where
  toJSON (Blog nom ps) = object $ [ "name" .= nom
                                  ] ++
                                  case ps of
                                    [] -> []
                                    _  -> ["posts" .= ps]


instance FromJSON Post where
  parseJSON (Object v) = Post <$> v .:  "title"
                              <*> (read <$> v .:  "date")
                              <*> v .:  "authors"
                              <*> v .:  "text"
                              <*> v .:? "tags" .!= []
  parseJSON _          = fail "Post needs to be an object."

instance ToJSON Post where
  toJSON (Post ttl dt auth txt tgs) = object $ [ "title" .= ttl
                                               , "date" .= show dt
                                               , "authors" .= auth
                                               , "text" .= txt
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
