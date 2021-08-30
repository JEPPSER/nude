module Main where

import Data.List
import Data.List.Utils
import System.IO
import Data.Foldable ( forM_, mapM_ )
import Data.List.Split
import System.Environment
import System.Environment.Blank (getArgs)
import System.Process
import GHC.IO.Handle.Text

main :: IO ()
main = do
    -- Get argument
    args <- getArgs
    let package = head args

    -- Read file
    defFile <- readFile' "package.json"
    let oldVersions = map (\s -> splitOn "\": " (filter (/= ',') s) !! 1) (filter (isInfixOf package) (lines (getDependencies defFile)))

    let deps = map formatDependencies (filter (isInfixOf package) (lines (getDependencies defFile)))
    versions <- mapM getLatestVersion deps

    putStrLn "Outdated packages:"

    -- Check versions
    forM_ [0..length versions - 1] $ \i -> do
        let latest = filter (\s -> (s /= '\n') && (s /= '\r')) (versions !! i)
        let parts = splitOn "," (deps !! i)
        let name = head parts
        let version = parts !! 1

        if latest /= version
            then putStrLn (name ++ ": " ++ version ++ " -> " ++ latest)
            else putStr ""

    putStrLn "Update all? [Y/N]"
    ans <- getLine
    if ans == "Y" || ans == "y"
        then updatePackageFile versions deps oldVersions
        else putStr ""
    
    if ans == "Y" || ans == "y"
        then callCommand "npm i"
        else putStr ""

updatePackageFile :: [String] -> [String] -> [String] -> IO ()
updatePackageFile versions deps oldVersions = do
    forM_ [0..length versions - 1] $ \i -> do
        let latest = filter (\s -> (s /= '\n') && (s /= '\r')) (versions !! i)
        let parts = splitOn "," (deps !! i)
        let name = "\"" ++ head parts ++ "\": "
        let version = parts !! 1
        let oldStr = name ++ (oldVersions !! i)
        let newStr = name ++ "\"" ++ latest ++ "\""
        if latest /= version
            then writeToPackageFile oldStr newStr
            else putStr ""

writeToPackageFile :: String -> String -> IO ()
writeToPackageFile oldStr newStr = do
    file <- readFile' "package.json"
    let newContent = replace oldStr newStr file
    writeFile "package.json" newContent

getLatestVersion :: String -> IO String
getLatestVersion s = do
    let parts = splitOn "," s
    let name = head parts
    let version = parts !! 1
    (_,Just ho1, _, hp1) <- createProcess (shell ("npm show " ++ name ++ " version")) {std_out=CreatePipe}
    hGetContents ho1

getDependencies :: String -> String
getDependencies str = head (splitOn "}," (splitOn "\"dependencies\": {" str !! 1))

formatDependencies :: String -> String
formatDependencies s = name ++ "," ++ version
    where
        parts = splitOn "\": \"" s
        name = filter (/= '"') (filter (/= ' ') (head parts))
        version = filter (\s -> (s /= '"') && (s /= ',') && (s /= '~') && (s /= '^') && (s /= '<') && (s /= '>') && (s /= '=') && (s /= '*')) (parts !! 1)
    