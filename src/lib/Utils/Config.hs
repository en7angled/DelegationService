module Utils.Config where

import Data.Aeson
import Data.ByteString.Char8 qualified as BS8
import System.Directory.Extra
import System.Environment
import Utils.Terminal (yellowColorString)

------------------------------------------------------------------------------------------------

-- * Configuration File Utilities

------------------------------------------------------------------------------------------------

decodeConfigFile :: (FromJSON a) => FilePath -> IO (Maybe a)
decodeConfigFile path = do
  fileExist <- doesFileExist path
  if fileExist
    then decodeFileStrict path
    else do
      putStrLn $ yellowColorString $ "File " <> path <> " does not exist"
      return Nothing

decodeConfigEnv :: (FromJSON a) => String -> IO (Maybe a)
decodeConfigEnv envName = do
  mVal <- lookupEnv envName
  case mVal of
    Just raw -> do
      putStrLn $ yellowColorString $ "Parsing config from env var " <> show envName
      case eitherDecodeStrict (BS8.pack raw) of
        Right a -> return (Just a)
        Left err -> error $ "Decoding env var " <> envName <> " failed: " <> err
    Nothing -> return Nothing 


-- | Try to read a JSON-encoded configuration from an environment variable.
-- If the variable is present, decode its contents; otherwise, fall back to the given file path.
decodeConfigEnvOrFile :: (FromJSON a) => String -> FilePath -> IO (Maybe a)
decodeConfigEnvOrFile envName filePath = do
  mVal <- decodeConfigEnv envName
  case mVal of
    Just a -> return (Just a)
    Nothing -> decodeConfigFile filePath

