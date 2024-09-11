module Lib
    ( processFile
    , FP.getCreateTableLines
    ) where

import qualified FileProcessing as FP

processFile :: FilePath -> Int -> IO ()
processFile filePath numCores = do
    tableLines <- FP.getCreateTableLines filePath
    print tableLines