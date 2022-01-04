module Main where

import qualified Markup
import qualified Html
import qualified Convert

import System.Directory (doesFileExist)
import System.Environment (getArgs)

import System.IO (getContents, readFile, writeFile)


main :: IO ()
main = getArgs >>= 
  \args -> case args of
    [] -> processWithoutArgs
    ["--help"] -> putHelpMessage
    ["--h"] -> putHelpMessage
    [input, output] -> processWithArgs 
    _ -> putStrLn "Incorrect format. Call with --help or --h."

putHelpMessage :: IO ()
putHelpMessage = 
  putStrLn "--help or --h: help\n
            <input path> <output path>: working with files\n
            no args: working with std IO\n"

processWithoutArgs :: IO ()
processWithoutArgs = getContents >>= \contents -> putString $ process "Empty title" contents

processWithArgs :: String -> String -> IO ()
processWithArgs inputPath outputPath = 
  doesFileExist outputPath >>= \isExist ->
    let tryProcess = readFile inputPath >>= \input -> writeFile outputPath (process inputPath input)
    in
      if isExist
        then whenIO confirm (tryProcess inputPath outputPath)
        else tryProcess inputPath outputPath  

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action =
  cond >>= \result ->
    if result
      then action
      else pure ()

confirm :: IO Bool
confirm =
  putStrLn "Are you sure? (y/n)" *>
    getLine >>= \answer ->
      case answer of
        "y" -> pure True
        "n" -> pure False
        _ ->
          putStrLn "Invalid response. use y or n" *>
            confirm
            
process :: Html.Title -> String -> String
process title input = Html.render . Convert.convert title . Markup.parse