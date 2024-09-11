module Lib
    ( processFile
    , getCreateTableLines
    ) where

import System.IO
import Data.List (isPrefixOf)
import Control.Concurrent.Async
import Control.Monad (forM_, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Control.Concurrent (getNumCapabilities, setNumCapabilities)
import System.Directory (doesFileExist)
import Data.Maybe (catMaybes)

cacheFile :: FilePath -> FilePath
cacheFile filePath = filePath ++ ".cache"

processFile :: FilePath -> Int -> IO ()
processFile filePath numCores = do
    createTableLines <- getCreateTableLines filePath
    originalCaps <- getNumCapabilities
    setNumCapabilities numCores
    forM_ createTableLines $ \lineNum -> do
        line <- getSpecificLine filePath lineNum  -- Changed from getLine to getSpecificLine
        putStrLn $ formatOutput lineNum line
    setNumCapabilities originalCaps

getCreateTableLines :: FilePath -> IO [Int]
getCreateTableLines filePath = do
    let cache = cacheFile filePath
    cacheExists <- doesFileExist cache
    if cacheExists
        then readCache cache
        else do
            tableLines <- processFileAndCache filePath
            writeCache cache tableLines
            return tableLines

processFileAndCache :: FilePath -> IO [Int]
processFileAndCache filePath = withFile filePath ReadMode $ \handle -> do
    let chunkSize = 1024 * 1024  -- 1 MB chunks
    processChunks handle 1 chunkSize []

processChunks :: Handle -> Int -> Int -> [Int] -> IO [Int]
processChunks handle startLine chunkSize acc = do
    chunk <- BS.hGet handle chunkSize
    if BS.null chunk
        then return (reverse acc)
        else do
            let chunkLines = C8.lines chunk
            results <- mapConcurrently processLine (zip [startLine..] chunkLines)
            let newAcc = (catMaybes results) ++ acc
            processChunks handle (startLine + length chunkLines) chunkSize newAcc

processLine :: (Int, BS.ByteString) -> IO (Maybe Int)
processLine (lineNum, line)
    | isCreateTableStatement (C8.unpack line) = return (Just lineNum)
    | otherwise = return Nothing

isCreateTableStatement :: String -> Bool
isCreateTableStatement = isPrefixOf "CREATE TABLE"

formatOutput :: Int -> BS.ByteString -> String
formatOutput lineNum line = "Line " ++ show lineNum ++ ": " ++ C8.unpack line

getSpecificLine :: FilePath -> Int -> IO BS.ByteString  -- Renamed from getLine to getSpecificLine
getSpecificLine filePath lineNum = withFile filePath ReadMode $ \handle -> do
    hSeek handle AbsoluteSeek (fromIntegral (lineNum - 1))
    BS.hGetLine handle

readCache :: FilePath -> IO [Int]
readCache cache = do
    content <- readFile cache
    return $ map read (lines content)

writeCache :: FilePath -> [Int] -> IO ()
writeCache cache lineNumbers = writeFile cache (unlines (map show lineNumbers))
