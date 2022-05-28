{-# LANGUAGE OverloadedStrings #-}
module Persistence where

import Data.Aeson
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Maybe (mapMaybe)
import System.Directory (doesFileExist)
import Types

read :: FilePath -> IO [(TransactionId, Unique Transaction)]
read path = do
  exists <- doesFileExist path
  if exists
    then do
      contents <- readFile path
      pure $ mapMaybe (decode . pack) $ lines contents
    else pure []

write :: FilePath -> (TransactionId, Unique Transaction) -> IO ()
-- avoid reading the file otherwise concurrent inserts could delete
-- each other
write path = appendFile path . ("\n"<>) . unpack . encode
