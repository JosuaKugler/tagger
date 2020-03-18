{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import System.Environment
import System.IO
import System.Directory
import System.Posix.Files
import Data.List.Split
import Data.List

jsonFile :: FilePath
jsonFile = "/home/josua/repos/tagger/.tagger.json"

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

matchIndex :: File -> [String] -> ([Bool], String)
matchIndex (File {fileName = fname, tags = ftags}) tags = ((map (`elem` tags) ftags), fname)

getRanking :: [File] -> [String] -> [([Bool], String)]
getRanking content tags = sortBy sortR $ foldl (\acc file -> (matchIndex file tags) : acc ) [] content

compareBools :: [Bool] -> [Bool] -> Ordering
compareBools [] _ = EQ
compareBools _ [] = EQ
compareBools a b = let res = compare (head b) (head a) 
    in if res == EQ then compareBools (tail a) (tail b) else res

sortR :: ([Bool], String) -> ([Bool], String) -> Ordering --first: more tags, second: earlier tags
sortR (a1,b1) (a2,b2) = 
    let sum = foldl (\acc bool -> if bool then acc + 1 else acc) 0 in
    if (sum a1) /= (sum a2) then compare (sum a2) (sum a1) else compareBools a1 a2

displayRanking :: [([Bool], String)] -> String
displayRanking ranking = let withNumberRanking = zip [0..] ranking
    in unlines $ map displayOneRanking withNumberRanking
    where displayOneRanking (number, entry) = (show number) ++ ".: " ++ (snd entry) ++ "   " ++ (show $ fst entry)

save :: [String] -> IO ()
save fileargs = do
    let fileName = head $ reverse $ splitOn "/" $ head fileargs --get pure filename without path
    d <- (eitherDecode <$> getJSON) :: IO (Either String [File])
    case d of 
        Left err -> putStrLn err
        Right prev -> do
            renameFile (head fileargs) (internalDirectory ++ "/" ++ fileName) --mv to internal dir
            let newfile = File {fileName = fileName, tags = tail fileargs}
            let next = newfile : prev
            B.writeFile jsonFile (encode next)

unsave :: [String] -> IO ()
unsave fileargs = do
    let fileName = head fileargs
    renameFile (internalDirectory ++ "/" ++ fileName) fileName
    d <- (eitherDecode <$> getJSON) :: IO (Either String [File])
    case d of 
        Left err -> putStrLn err
        Right content -> do 
            let next = foldr (\file@(File {fileName = fname, tags = ftags}) acc -> if fname /= fileName then file:acc else acc) [] content
            B.writeFile jsonFile (encode next)



open :: [String] -> IO ()
open fileargs = do
    d <- (eitherDecode <$> getJSON) :: IO (Either String [File])
    case d of 
        Left err -> putStrLn err
        Right content -> do 
            let ranking = getRanking content fileargs 
            putStrLn $ displayRanking ranking
            putStrLn "Enter a number to select a file"
            n <- getLine
            let fileName = snd (ranking !! (read n))
            createSymbolicLink (internalDirectory ++ "/" ++ fileName) fileName

newtag :: [String] -> IO ()
newtag fileargs = do
    d <- (eitherDecode <$> getJSON) :: IO (Either String [File])
    case d of
        Left err -> putStrLn err
        Right content -> do
            let fileName:newtags = fileargs
            let next = foldr (\file@(File {fileName = fname, tags = ftags}) acc -> if fname /= fileName then file:acc else (File {fileName = fileName, tags = newtags}):acc) [] content
            B.writeFile jsonFile (encode next)

addtag :: [String] -> IO ()
addtag fileargs = do
    d <- (eitherDecode <$> getJSON) :: IO (Either String [File])
    case d of
        Left err -> putStrLn err
        Right content -> do
            let fileName:addtags = fileargs
            let next = foldr (\file@(File {fileName = fname, tags = ftags}) acc -> if fname /= fileName then file:acc else (File {fileName = fileName, tags = concat [addtags,ftags]}):acc) [] content
            B.writeFile jsonFile (encode next)

rmtag :: [String] -> IO ()
rmtag fileargs = do 
    d <- (eitherDecode <$> getJSON) :: IO (Either String [File])
    case d of
        Left err -> putStrLn err
        Right content -> do
            let fileName:rmtags = fileargs
            let next = foldr (\file@(File {fileName = fname, tags = ftags}) acc -> if fname /= fileName then file:acc else (File {fileName = fileName, tags = ftags \\ rmtags}):acc) [] content
            B.writeFile jsonFile (encode next)

main :: IO ()
main = do
    args <- getArgs
    let action:actargs = args
    case action of
        "save" -> save $ actargs --example: ./tagger save test2.txt text tagger test more tags
        "unsave" -> unsave $ actargs --example: ./tagger unsave test2.txt
        "open" -> open $ actargs --example: ./tagger open tagger test random
        "newtag" -> newtag $ actargs --example ./tagger newtag test2.txt four completely new tags
        "addtag" -> addtag $ actargs --example ./tagger addtag test2.txt three additional tags
        "rmtag" -> rmtag $ actargs --example ./tagger rmtag test 2.txt remove these four tags