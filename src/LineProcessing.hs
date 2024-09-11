module LineProcessing
    ( processLine
    , isCreateTableStatement
    , formatOutput
    , getSpecificLine
    , findCreateTableEnd
    ) where

import System.IO
import Data.List (isPrefixOf)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import System.IO.Unsafe (unsafePerformIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

processLine :: (Int, BS.ByteString) -> IO (Maybe (Int, Int))
processLine (lineNum, line)
    | isCreateTableStatement (C8.unpack line) = do
        endLine <- findCreateTableEnd "scimag.sql" lineNum
        return (Just (lineNum, endLine))
    | otherwise = return Nothing

isCreateTableStatement :: String -> Bool
isCreateTableStatement = isPrefixOf "CREATE TABLE"

formatOutput :: (Int, Int) -> String
formatOutput (startLine, endLine) = show startLine ++ "-" ++ show endLine

getSpecificLine :: FilePath -> Int -> IO BS.ByteString
getSpecificLine filePath lineNum = withFile filePath ReadMode $ \handle -> do
    hSeek handle AbsoluteSeek (fromIntegral (lineNum - 1))
    BS.hGetLine handle

-- Global cache for storing end line numbers
endLineCache :: IORef (Map Int Int)
endLineCache = unsafePerformIO $ newIORef Map.empty

findCreateTableEnd :: FilePath -> Int -> IO Int
findCreateTableEnd filePath startLine = do
    cache <- readIORef endLineCache
    case Map.lookup startLine cache of
        Just endLine -> return endLine
        Nothing -> do
            endLine <- findEndLine filePath startLine
            modifyIORef' endLineCache (Map.insert startLine endLine)
            return endLine

findEndLine :: FilePath -> Int -> IO Int
findEndLine filePath startLine = do
    let checkLine lineNum = do
            line <- getSpecificLine filePath lineNum
            if C8.isPrefixOf (C8.pack ");") line
                then return lineNum
                else checkLine (lineNum + 1)
    checkLine startLine