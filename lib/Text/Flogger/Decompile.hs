{-# LANGUAGE OverloadedStrings #-}

{- |
Module       : Text.Flogger.Decompile
Description  : Compiles stuff
Copyright    : 2014, Peter Harpending.
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : archlinux

-}

module Text.Flogger.Decompile where

import           Control.Applicative
import           Data.Aeson
import qualified Data.ByteString.Lazy as Bl
import qualified Data.Text as Ts
import qualified Data.Text.Lazy.IO as TlIO
import           Data.Time
import           Data.Yaml
import           Text.Blaze.Html.Renderer.Text
import           Text.Flogger.Types
import           Text.Markdown

decodeEitherLazy :: FromJSON a => Bl.ByteString -> Either String a
decodeEitherLazy = decodeEither . Bl.toStrict

-- |Given a lazy 'Bl.ByteString' of YAML (or JSON), decompile it
-- into a 'Blog'. There is a lot that can go wrong, hence the monad
-- shitstorm.
decompileBlog :: Bl.ByteString           -- ^The YAML/JSON to decode
              -> IO (Either String Blog) -- ^Either the blog, or an err message.
decompileBlog bytes = do
  let eitherBlog = decodeEitherLazy bytes
  case eitherBlog of
    Left err -> return $ Left err
    Right almostBlog -> do
      postsLbs <- Bl.readFile (postsFile almostBlog)
      let eitherPosts = decodeEitherLazy postsLbs :: Either String [AlmostPost]
          mkBlog      = Blog (aname almostBlog)
      case eitherPosts of
        Left err        -> return $ Left err
        Right almostPosts -> do
          tposts <- mapM mkPost almostPosts
          return $ Right $ mkBlog tposts

mkPost :: AlmostPost -> IO Post
mkPost (AlmostPost t as d mf p ts) = do
  postMd <- TlIO.readFile mf
  let postHtml = markdown def postMd
      postText = renderHtml postHtml
  return $ Post t as d postText p ts

data AlmostBlog = AlmostBlog { aname      :: Ts.Text
                             , postsFile :: FilePath
                             }
  deriving (Eq, Read, Show)

data AlmostPost = AlmostPost { atitle     :: Ts.Text
                             , adate      :: UTCTime
                             , aauthors   :: [Author]
                             , mdFile    :: FilePath
                             , apublished :: Bool
                             , atags      :: [Ts.Text]
                             }
  deriving (Eq, Read, Show)

instance FromJSON AlmostBlog where
  parseJSON (Object v) = AlmostBlog <$> v .: "name"
                                    <*> v .:? "posts-file" .!= "posts.yml"
  parseJSON _          = fail "Blog must be an object."

instance FromJSON AlmostPost where
  parseJSON (Object v) = AlmostPost <$> v .: "title"
                                    <*> v .: "published-at"
                                    <*> v .: "authors"
                                    <*> v .: "text-file"
                                    <*> v .:? "publish" .!= False
                                    <*> v .:? "tags" .!= []
  parseJSON _          = fail "Post must be an object."
