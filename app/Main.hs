module Main where

import Data.List
import System.IO
import Data.Foldable ( forM_ )
import Data.List.Split
import System.Environment
import System.Environment.Blank (getArgs)

main :: IO ()
main = do
    -- Get argument
    args <- getArgs
    let package = head args

    -- Read file
    defFile <- readFile "package.json"
    let deps = filter (isInfixOf package) (lines (getDependencies defFile))
    forM_ deps $ \s -> do
        putStrLn s

getDependencies :: String -> String
getDependencies str = head (splitOn "}," (splitOn "\"dependencies\": {" str !! 1))
