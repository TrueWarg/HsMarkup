module HsMarkup.Env where

data Env 
    = Env
    { markupName :: String
    , stylesheetPath :: FilePath
    }

    deriving Show

defaultEnv :: Env
defaultEnv = Env "My Markup" "style.css"
