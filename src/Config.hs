{-# LANGUAGE DeriveGeneric, DuplicateRecordFields #-}

-- Read the configuration file and provide config data type

module Config
  ( Config(..)
  , readConfig
  )
where

import           Data.Aeson                     ( FromJSON
                                                , eitherDecode
                                                )
import qualified Data.ByteString.Lazy          as B
import           Data.Maybe                     ( fromMaybe )
import           GHC.Generics
import           System.Directory               ( getHomeDirectory )
import           System.FilePath                ( FilePath )

import qualified Defaults                      as Def
import           CLOpts                         ( CLOpts(..) )
import           Tilde                          ( tildeExpand )

-- configuration options read from the config file
data ConfigFromJSON = ConfigFromJSON {
  outputDir:: Maybe String,
  maxVideos:: Maybe Int,
  defaultFormat:: Maybe String
} deriving (Generic, Show)

instance FromJSON ConfigFromJSON

-- full set of config options from config file, command line options and defaults
data Config = Config {
  ytdlCommand   :: String,
  channelsFile  :: String,
  maxVideos     :: Int,
  defaultFormat :: String,
  outputDir     :: Maybe String,
  matchName     :: Maybe String,
  matchTag      :: Maybe String,
  simulate      :: Bool,
  echo          :: Bool
}

combine :: CLOpts -> ConfigFromJSON -> Config
combine clopts configFromJSON = Config
  { ytdlCommand   = fromMaybe Def.ytdlCommand
                      $ (ytdlCommand :: CLOpts -> Maybe String) clopts
  , channelsFile  = fromMaybe Def.channelsFile (CLOpts.channelsFile clopts)
  , maxVideos     = fromMaybe
                      Def.maxVideos
                      ((maxVideos :: ConfigFromJSON -> Maybe Int) configFromJSON)
  , defaultFormat = fromMaybe
                      Def.videoFormat
                      ((defaultFormat :: ConfigFromJSON -> Maybe String)
                        configFromJSON
                      )
  , outputDir     = (outputDir :: ConfigFromJSON -> Maybe String) configFromJSON
  , matchName     = (matchName :: CLOpts -> Maybe String) clopts
  , matchTag      = (matchTag :: CLOpts -> Maybe String) clopts
  , simulate      = (simulate :: CLOpts -> Bool) clopts
  , echo          = (echo :: CLOpts -> Bool) clopts
  }

readConfig :: CLOpts -> IO (Either String Config)
readConfig clopts = do
  let f = fromMaybe Def.configFile (configFile clopts)
  f'                   <- tildeExpand f
  configFromJSONResult <- eitherDecode <$> B.readFile f'
  case configFromJSONResult of
    Left  err            -> return (Left err)
    Right configFromJSON -> return (Right (combine clopts configFromJSON))
