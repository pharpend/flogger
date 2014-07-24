{-# LANGUAGE OverloadedStrings #-}

{- |
Module       : Main
Description  : Runs the client
Copyright    : 2014, Peter Harpending.
License      : BSD3
Maintainer   : Peter Harpending <pharpend2@gmail.com>
Stability    : experimental
Portability  : archlinux

-}

module Main where

import qualified Data.ByteString as Bs
import qualified Data.ByteString.Lazy as Bl
import           Data.Monoid
import qualified Data.Text as Ts
import           Options.Applicative
import           System.Exit
import qualified System.IO as Io

data LfInput = LfInput { compile    :: Bool
                       , license    :: Bool
                       , inputFile  :: FilePath
                       , inputForm  :: String
                       , outputFile :: FilePath
                       }
  deriving (Eq, Read, Show)

parseInput :: Parser LfInput
parseInput = LfInput
  <$> switch (  short 'c'
             <> long "compile"
             <> help "Compile the input."
             )
  <*> switch (  short 'l'
             <> long "license"
             <> help "Print the license."
             )
  <*> option ( short 'i'
             <> long "input-file"
             <> help "Input file for compile."
             <> metavar "FILE"
             <> action "file"
             <> value "STDIN"
             <> showDefault
             )
  <*> option ( short 'f'
             <> long "input-format"
             <> help "Format of the output. Either 'json' or 'yaml'"
             <> metavar "FORMAT"
             <> completeWith ["json", "yaml"]
             <> value "yaml"
             <> showDefault
             )
  <*> option ( short 'o'
             <> long "output-file"
             <> help "File from which to take output."
             <> metavar "FILE"
             <> action "file"
             <> value "STDOUT"
             <> showDefault
             )

main :: IO ()
main = do
    input <- execParser opts
    putStrLn $ show input
  where
    opts = info (helper <*> parseInput)
      ( fullDesc
     <> header "localflog - local client for flogger."
      )
