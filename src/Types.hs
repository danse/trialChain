{-# LANGUAGE TemplateHaskell #-}
module Types where

import Data.Aeson
import Data.Aeson.TH
import Data.ByteString hiding (pack, unpack, intercalate)
import Data.Char (isNumber)
import Data.List (intercalate)
import Data.List.NonEmpty hiding (intercalate)
import Data.Text (pack, unpack)
import Data.Time
import Data.Word (Word64, Word8)
import GHC.Conc (TVar)
import Text.Read (readPrec)
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec (lift)

type Address = Int
type Amount = Double
type Signature = String
newtype TransactionId = TransactionId [Word8] deriving (Eq)

sep = '-'

instance Show TransactionId where
  show (TransactionId ww) = intercalate [sep] $ show <$> ww

instance Read TransactionId where
  readPrec = lift (TransactionId . fmap read <$> parse)
    where
      parse :: ReadP [String]
      parse = many1 (satisfy isNumber) `sepBy1` char sep

instance ToJSON TransactionId where
  toJSON = String . pack . show

instance FromJSON TransactionId where
  parseJSON (String s) = pure $ read $ unpack s

data Transaction = Transaction {
  sources      :: NonEmpty (Address, Signature),
  destinations :: NonEmpty Address,
  amount       :: Amount
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Transaction)

data Unique t = Unique {
  content   :: t,
  timestamp :: UTCTime,
  salt      :: Word64
  }

$(deriveJSON defaultOptions ''Unique)

data Memory = Memory {
  transactions :: [(TransactionId, Unique Transaction)],
  amounts      :: [(Address, Amount)]
  }
