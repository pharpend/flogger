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

import           Data.Aeson
import qualified Data.ByteString.Lazy as Bl
import           Data.Monoid
import qualified Data.Text as Ts
import           Data.Yaml hiding (Parser, encode)
import           Options.Applicative
import           Paths_flogger
import qualified System.IO as Io
import           Text.Flogger

data LfInput = LfInput { compile    :: Bool
                       , license    :: Bool
                       , inputFile  :: FilePath
                       , inputForm  :: Ts.Text
                       , outputFile :: FilePath
                       }
  deriving (Eq, Read, Show)

runInput :: LfInput -> IO ()
runInput lfi
    | license lfi = do
        licenseFile <- getDataFileName "LICENSE"
        licenseBytes <- Bl.readFile licenseFile
        outh <- outputHandle
        Bl.hPut outh licenseBytes
    | compile lfi = do
        inh <- inputHandle
        Io.hSetBinaryMode inh False
        inputBytes <- Bl.hGetContents inh
        outh <- outputHandle
        case readInput inputBytes of
          Left err -> fail err
          Right bg -> Bl.hPut outh (encode bg)
    | otherwise = return ()
  where
    outputHandle :: IO Io.Handle
    outputHandle = case (outputFile lfi) of
                     "STDOUT" -> return Io.stdout
                     outFile  -> Io.openFile outFile Io.WriteMode
    inputHandle :: IO Io.Handle
    inputHandle  = case (inputFile lfi) of
                     "STDIN" -> return Io.stdin
                     inFile  -> Io.openFile inFile Io.ReadMode
    readInput :: Bl.ByteString -> Either String Blog
    readInput    = case (Ts.toLower $ inputForm lfi) of
                     "yaml" -> decodeEither . Bl.toStrict
                     "json" -> eitherDecode
                     format -> fail $ "Unknown format: " <> show format

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
      <*> option ( short 'i'
                 <> long "input-file"
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
                 <> metavar "FILE"
                 <> action "file"
                 <> value "STDOUT"
                 <> showDefault
                 )
