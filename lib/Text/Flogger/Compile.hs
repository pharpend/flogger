{- |
Module       : Text.Flogger.Compile
Description  : Compiles stuff
Copyright    : 2014, Peter Harpending.
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : archlinux

-}

module Text.Flogger.Compile where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as Bl
import           Text.Flogger.Types

-- |Compile a 'Blog' into JSON
compileBlog :: Blog                 -- ^The blog to compile
            -> Bool                 -- ^Should the result be human-readable?
            -> Bl.ByteString        -- ^The result
compileBlog blog pretty = jsonEncode blog
  where
    jsonEncode :: Blog -> Bl.ByteString
    jsonEncode = if pretty
                   then encodePretty' (defConfig {confIndent=2})
                   else encode
