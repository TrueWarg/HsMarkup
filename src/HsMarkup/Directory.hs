module HsBlog.Directory
  ( convertDirectory
  , buildIndex
  , confirm
  )
  where

import qualified HsMarkup.Markup as Markup
import qualified HsMarkup.Html as Html
import HsMarkup.Convert (convert, convertStructure)

import Data.List (partition)
import Data.Traversable (for)
import Control.Monad (void, when)

import System.IO (hPutStrLn, stderr)
import Control.Exception (catch, displayException, SomeException(..))
import System.Exit (exitFailure)
import System.FilePath
  ( takeExtension
  , takeBaseName
  , (<.>)
  , (</>)
  , takeFileName
  )
import System.Directory
  ( createDirectory
  , removeDirectoryRecursive
  , listDirectory
  , doesDirectoryExist
  , copyFile
  )

data DitContents 
  = DirContents
  {
    dcFilesToProcess :: [(FilePath, String)]
  , dcFilesToCopy :: [FilePath]
  }

convertDirectory :: FilePath -> FilePath -> IO()
convertDirectory inputDir outputDir = do
    DirContents filesToProcess filesToCopy <- getDirFilesAndContent inputDir
    createOutputDirectoryOrExit outputDir
    let
      outputHtmls = txtsToRenderedHtml filesToProcess
    copyFiles outputDir filesToCopy
    writeFiles outputDir outputHtmls
    putStrLn "Done."

buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
buildIndex files =
    let 
        previews =
            map( \ (path, doc) ->
                case doc of
                    Markup.Header 1 header : article ->
                        Html.h 3 (Html.link path (Html.txtContent header))
                        <> foldMap Convert.convertStructure (take 3 article)
                        <> Html.p (Html.link path (Html.txtContent "..."))
                    _ -> Html.h 3 (Html.link path (Html.txtContent path))

            )
            files
    in
        Html.html
            "Blog"
            (
                Html.h1 (Html.link "index.html" (Html.txtContent "Blog"))
                <> Html.h 2 (Html.txtContent "Posts")
                <> mconcat previews
            )

getDirFilesAndContent :: FilePath -> IO DirContents
getDirFilesAndContent inputDir = do
  files <- map (inputDir </>) <$> listDirectory inputDir
  let
    (txts, others) = partition (\path -> takeExtension path == ".txt") paths
  txtsWithContent <- applyIoOnList readFile txtFiles >>= filterAndReportFailures
  pure $ DirContents 
        { 
          dcFilesToProcess = txtsWithContent
        , dcFilesToCopy = others
        }   

applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
applyIoOnList action files = do
  for files $ \file -> do
          content <- catch
              (Right <$> action file)
              ( \(SomeException e) -> do
                 pure $ Left (displayException e)
              )

          pure (file, content)


filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
filterAndReportFailures =
  foldMap $ \ (file, contentOrErr) ->
    case contentOrErr of
      Left err -> do
        hPutStrLn stderr err
        pure []
      Right content ->
        pure [(file, content)]

createOutputDirectoryOrExit :: FilePath -> IO()
createOutputDirectoryOrExit path = 
  whenIO
     (not <$> createOutputDirectory path)
     (hPutStrLn stderr "Terminate" *> exitFailure)

createOutputDirectory :: FilePath -> IO Bool
createOutputDirectory path = do
  isExist <- doesDirectoryExist path
  create <- 
      if isExist
        then do
          override <- confirm "Output directory exists. Override?"
          when override (removeDirectoryRecursive path)
          pure override
        else
          pure True

  when create (removeDirectoryRecursive path)
  pure create

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

txtsToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]
txtsToRenderedHtml txts =
  let
    docs = map toOutputMarkupFile txts
    index = ("index.html", buildIndex docs)
  in
    map (fmap Html.render) (index : map convertFile txtOutputFiles)

toOutputMarkupFile :: (FilePath, String) -> (FilePath, Markup.Document)
toOutputMarkupFile (path, content) = 
  (takeBaseName file <.> "html", Markup.parse content)

convertFile :: (FilePath, Markup.Document) -> (FilePath, Html.Html)
convertFile (file, doc) = (file, convert file doc)

copyFiles :: FilePath -> [FilePath] -> IO ()
copyFiles outputDir files =
  let
    copyFromTo file = copyFile file (outputDir </> takeFileName file)
  in
    void $ applyIoOnList copyFromTo files >>= filterAndReportFailures

writeFiles :: FilePath -> [(FilePath, String)] -> IO ()
writeFiles outputDir files = do
  let
    writeFileContent (file, content) =
      writeFile (outputDir </> file) content
  void $ applyIoOnList writeFileContent files >>= filterAndReportFailures