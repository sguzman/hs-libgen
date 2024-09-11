module FileProcessing
    ( processFile
    , getCreateTableLines
    , processFileAndCache
    , processChunks
    ) where

import System.IO
import Control.Concurrent.Async (mapConcurrently)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import System.Directory (doesFileExist)
import Data.Maybe (catMaybes)
import LineProcessing (processLine, formatOutput)
import CacheManagement (cacheFile, readCache, writeCache)

processFile :: FilePath -> IO ()
processFile filePath = do
    ranges <- processFileAndCache filePath
    mapM_ (putStrLn . formatOutput) ranges

processChunks :: Handle -> Int -> Int -> [(Int, Int)] -> IO [(Int, Int)]
processChunks handle startLine chunkSize acc = do
    chunk <- BS.hGet handle chunkSize
    if BS.null chunk
        then return (reverse acc)
        else do
            let chunkLines = C8.lines chunk
            results <- mapConcurrently processLine (zip [startLine ..] chunkLines)
            let newAcc = catMaybes results ++ acc
            processChunks handle (startLine + length chunkLines) chunkSize newAcc

getCreateTableLines :: FilePath -> IO [Int]
getCreateTableLines filePath = do
    let cache = cacheFile filePath
    cacheExists <- doesFileExist cache
    if cacheExists
        then readCache cache
        else do
            tableRanges <- processFileAndCache filePath
            let tableLines = concatMap (\(start, end) -> [start, end]) tableRanges
            writeCache cache tableLines
            return tableLines

processFileAndCache :: FilePath -> IO [(Int, Int)]
processFileAndCache filePath = withFile filePath ReadMode $ \handle -> do
    let chunkSize = 1024 * 1024  -- 1 MB chunks
    processChunks handle 1 chunkSize []
