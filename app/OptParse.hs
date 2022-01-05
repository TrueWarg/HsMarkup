module OptParse
  ( Options(..)
  , SingleInput(..)
  , SingleOutput(..)
  , parse
  )
  where

import Data.Maybe (fromMaybe)
import Options.Applicative

data Options
    = ConvertSingle SingleInput SingleOutput
    | ConvertDir FilePath FilePath
    deriving (Show)

data SingleInput
    = Stdin
    | InputFile FilePath
    deriving (Show)

data SingleOutput
    = Stdout
    | OutputFile FilePath
    deriving (Show)

-- -------------------------------
parse :: IO Options
parse = execParser opts

opts :: ParserInfo Options
opts = 
    info (helper <*> pOptions)
    (
        fullDesc
        <> header "hs-markup-gen - a markap genetator"
        <> progDesc "Convert markup files or dirs to html"
    )

pOptions :: Parser Options
pOptions = 
    subparser
       (
           command
           "convert"
            ( info
               (helper <*> pConvertSingle)
               (progDesc "Convert a single markup source to html")
            )
            
            <> command
              "convert-dir"
              ( info
                 (helper <*> pConvertDir)
                 (progDesc "Convert a directory of markup files to html")
              ) 
       )

pConvertSingle :: Parser Options
pConvertSingle =
    ConvertSingle <$> pSingleInput <*> pSingleOutput

pSingleInput :: Parser SingleInput
pSingleInput = 
    fromMaybe Stdin <$> optional pInputFile

pSingleOutput :: Parser SingleOutput
pSingleOutput = 
    fromMaybe Stdout <$> optional pOutputFile

pInputFile :: Parser SingleInput
pInputFile = fmap InputFile parser
    where 
        parser = strOption
                    (
                        long "input"
                        <> short 'i'
                        <> metavar "FILE"
                        <> help "input file"
                    )

pOutputFile :: Parser SingleOutput
pOutputFile = fmap OutputFile parser
    where 
        parser = strOption
                    (
                        long "output"
                        <> short 'o'
                        <> metavar "FILE"
                        <> help "output file"
                    )

pConvertDir :: Parser Options
pConvertDir = 
    ConvertDir <$> pInputDir <*> pOutputDir

pInputDir :: Parser FilePath
pInputDir =
    strOption
        (
            long "input"
            <> short 'i'
            <> metavar "DIRECTORY"
            <> help "input dir path"
        )

pOutputDir :: Parser FilePath
pOutputDir =
    strOption
        (
            long "output"
            <> short 'o'
            <> metavar "DIRECTORY"
            <> help "output dir path"
        )
