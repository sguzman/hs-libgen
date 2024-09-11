module Main (main) where

import Lib (processFile, getCreateTableLines)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath, numCores] -> processFile filePath (read numCores)
        [filePath] -> do
            tableLines <- getCreateTableLines filePath
            print tableLines
        _ -> putStrLn "Usage: stack run <file_path> [<num_cores>]"

