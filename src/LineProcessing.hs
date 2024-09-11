module LineProcessing
    ( processLine
    , isCreateTableStatement
    , formatOutput
    , getSpecificLine
    ) where

import System.IO
import Data.List (isPrefixOf)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

-- Implement the functions listed above

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