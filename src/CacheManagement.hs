module CacheManagement
    ( cacheFile
    , readCache
    , writeCache
    ) where

-- Implement the functions listed above

cacheFile :: FilePath -> FilePath
cacheFile filePath = filePath ++ ".cache"

readCache :: FilePath -> IO [Int]
readCache cache = do
    content <- readFile cache
    return $ map read (lines content)

writeCache :: FilePath -> [Int] -> IO ()
writeCache cache lineNumbers = writeFile cache (unlines (map show lineNumbers))