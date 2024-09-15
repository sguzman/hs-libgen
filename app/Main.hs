module Main (main) where

import System.IO (Handle, IOMode(ReadMode),
    openFile, hClose, hTell, hSeek, SeekMode(AbsoluteSeek), hGetLine)
import Data.List (isPrefixOf)

-- Type for range
type Range = (Int, Int)
type SkippedFile = (IO Handle, Int)
type CreateReturn = (SkippedFile, Int)

inputFile :: () -> FilePath
inputFile _ = "data.sql"

-- Return a handle that does nothing
nullHandle :: IO Handle
nullHandle = undefined


-- Given a IO handle, skip n lines and return a new IO handle
skipLines :: IO Handle -> Int -> IO Handle
skipLines ioHandle n = do
    handle <- ioHandle
    mapM_ (\_ -> hGetLine handle) [1..n]
    return handle


copyIOHandle :: IO Handle -> IO Handle
copyIOHandle ioHandle = do
    originalHandle <- ioHandle
    position <- hTell originalHandle
    newHandle <- openFile (inputFile ()) ReadMode
    hSeek newHandle AbsoluteSeek position
    return newHandle

openReadableFile :: FilePath -> IO Handle
openReadableFile path = do
    handle <- openFile path ReadMode
    return handle

skip :: FilePath -> Int -> SkippedFile
skip path n = (skipLines (openReadableFile path) n, n)


skipOnSkip :: SkippedFile -> Int -> SkippedFile
skipOnSkip (ioHandle, n) m = (skipLines ioHandle (m + n), m + n)

table :: CreateReturn -> String
table _ = ""

lns :: FilePath -> [Int]
lns _ = []

rng :: FilePath -> Range
rng _ = (0, 0)

stmt :: FilePath -> Range -> [String]
stmt _ _ = []

stmts :: FilePath -> [Range] -> [String]
stmts _ _ = []

join :: [String] -> String
join _ = ""

insertln :: FilePath -> String -> Int
insertln _ _ = 0

insertlns :: FilePath -> String -> [Int]
insertlns _ _ = []

columns :: FilePath -> Int -> String -> [String]
columns _ _ _ = []

valueSet :: FilePath -> Int -> String -> [String]
valueSet _ _ _ = []

valueSets :: FilePath -> Int -> String -> [[String]]
valueSets _ _ _ = []


main :: IO ()
main = putStrLn "Hello, World!"
