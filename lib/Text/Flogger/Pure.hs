{- |
Module       : Text.Flogger.Pure
Description  : Pure interface to flogger
Copyright    : 2014, Peter Harpending.
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : archlinux
-}

module Text.Flogger.Pure where

import           Data.Monoid
import qualified Data.Text as Ts
import qualified Data.Text.Lazy as Tl
import           Data.Time

-- |Data type for a blog
data Blog = Blog { blogName :: Ts.Text
                 , posts    :: [Post]
                 }

-- |Data type for a post
data Post = Post { title   :: Ts.Text
                 , date    :: UTCTime
                 , authors :: [Author]
                 , body    :: Tl.Text
                 , tags    :: [Ts.Text]
                 }
  deriving (Eq, Show)

-- |Data type for an author
data Author = Author { name  :: Ts.Text
                     , email :: Ts.Text
                     }
  deriving (Eq, Show)

-- |Dummy Post value
nullPost :: Post
nullPost = Post mempty nullTime [] mempty []

-- |Dummy Time value
nullTime :: UTCTime
nullTime = UTCTime (ModifiedJulianDay 0) 0

-- |Dummy Author value
nullAuthor :: Author
nullAuthor = Author mempty mempty

nullBlog :: Blog
nullBlog = Blog mempty mempty
