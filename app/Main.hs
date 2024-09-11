module Main where

import Lib (processFile, getCreateTableLines)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath, numCores] -> processFile filePath (read numCores)
        [filePath] -> do
            lines <- getCreateTableLines filePath
            print lines
        _ -> putStrLn "Usage: stack run <file_path> [<num_cores>]"

