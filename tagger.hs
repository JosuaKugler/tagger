{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B

jsonFile :: FilePath
jsonFile = "tagger.json"


data File = 
    File {
        fileName :: String,
        tags :: [String]
    } deriving (Generic, Show)

instance Eq File where
    x == y = fileName x == fileName y

instance ToJSON File where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON File


getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

main :: IO ()
main = do
 d <- (eitherDecode <$> getJSON) :: IO (Either String [File])
 case d of
  Left err -> putStrLn err
  Right f -> print f