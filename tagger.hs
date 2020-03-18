{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import System.Environment
import System.IO
import System.Directory
import Data.List.Split

jsonFile :: FilePath
jsonFile = "/home/josua/repos/tagger/tagger.json"

internalDirectory :: FilePath
internalDirectory = "/home/josua/repos/tagger/.tagger_internal"

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


save fileargs = do
    let fileName = head $ reverse $ splitOn "/" $ head fileargs --get pure filename without path
    d <- (eitherDecode <$> getJSON) :: IO (Either String [File])
    case d of 
        Left err -> let prev = if err == "Error in $: not enough input" then Right [] else Left err
        Right a  -> let prev = Right a
    case prev of
        Left err    -> putStrLn err
        Right prev  -> do 
            renameFile (head fileargs) (internalDirectory ++ "/" ++ fileName) --mv to internal dir
            let newfile = File {fileName = fileName, tags = tail fileargs}
            let next = newfile : prev
            B.writeFile jsonFile (encode next)

open fileargs = do
    d <- (eitherDecode <$> getJSON) :: IO (Either String [File])
    case d of 
        Left err -> putStrLn err
        Right prev -> do
            let newfile = File {fileName = (head fileargs), tags = tail fileargs}
            let next = newfile : prev
            B.writeFile jsonFile (encode next)



main :: IO ()
main = do
    args <- getArgs
    let action:actargs = args
    case action of
        "save" -> save $ actargs
        "open" -> open $ actargs
    putStrLn $ show args
    d <- (eitherDecode <$> getJSON) :: IO (Either String [File])
    case d of
        Left err -> putStrLn err
        Right f -> print f