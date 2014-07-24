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

import           Codec.Compression.GZip
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as Bl
import           Data.Monoid
import           Options.Applicative
import           Paths_flogger
import qualified System.IO as Io
import           Text.Flogger

data LfInput = LfInput { compile        :: Bool
                       , license        :: Bool
                       , compressOutput :: Bool
                       }
  deriving (Eq, Read, Show)


readLbs :: IO Bl.ByteString
readLbs = do
  Io.hSetBinaryMode Io.stdin True
  Bl.hGetContents Io.stdin

runInput :: LfInput -> IO ()
runInput lfi
    | license lfi = do
        licenseFile <- getDataFileName "LICENSE"
        licenseBytes <- Bl.readFile licenseFile
        printLbs licenseBytes
    | compile lfi = do
        inputBytes <- readLbs
        dcb <- decompileBlog inputBytes
        case dcb of
          Left err -> fail err
          Right bg -> printLbs (jsonEncode bg)
    | otherwise = fail $ "Unacceptable input " <> show lfi

  where
    jsonEncode :: ToJSON t0 => t0 -> Bl.ByteString
    jsonEncode = if (compressOutput lfi)
                   then compress . encode
                   else encodePretty' (defConfig {confIndent=2})
    printLbs :: Bl.ByteString -> IO ()
    printLbs = Bl.hPut Io.stdout

main :: IO ()
main = do
    input <- execParser opts
    runInput input
  where
    opts = info (helper <*> parseInput)
      ( fullDesc
     <> header "localflog - local client for flogger."
      )
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
      <*> switch (  short 'z'
                 <> long "compress"
                 <> help "Compress the output with GZip."
                 )
