module HsMarkup
  ( convertSingle
  , process
  , convertDirectory
  , buildIndex
  )
  where

import qualified HsMarkup.Markup as Markup
import qualified HsMarkup.Html as Html
import HsMarkup.Convert (convert)
import HsMarkup.Env (defaultEnv)
import HsMarkup.Directory (convertDirectory, buildIndex)

import System.IO

convertSingle :: String -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

process :: String -> String -> String
process title = Html.render . convert defaultEnv title . Markup.parse