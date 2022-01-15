module Main where

import OptParse
import HsMarkup

import System.Exit (exitFailure)
import System.Directory (doesFileExist)
import System.IO

main :: IO ()
main = do
  options <- parse
  case options of
    ConvertDir input output ->
      HsMarkup.convertDirectory input output

    ConvertSingle input output -> do
      (title, inputHandle) <-
        case input of
          Stdin ->
            pure ("", stdin)
          InputFile file ->
            (,) file <$> openFile file ReadMode

      outputHandle <-
        case output of
          Stdout -> pure stdout
          OutputFile file -> do
            exists <- doesFileExist file
            shouldOpenFile <-
              if exists
                then confirm "File is exist. Are you shure ?"
                else pure True
            if shouldOpenFile
              then
                openFile file WriteMode
              else
                exitFailure

      HsMarkup.convertSingle title inputHandle outputHandle
      hClose inputHandle
      hClose outputHandle

confirm :: String -> IO Bool
confirm message = do
  putStrLn $ message <> " (y/n)"
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _ -> do
       putStrLn "Invalid response. use y or n"
       confirm message