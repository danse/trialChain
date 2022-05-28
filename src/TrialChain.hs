{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module TrialChain where

import Crypto.Hash.MD5 (hash)
import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import Data.List (intercalate, foldl')
import Data.Maybe (isNothing, mapMaybe)
import Data.Time
import Data.Word (Word64, Word8)
import Types

import qualified Data.ByteString
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set

broadcast
  :: UTCTime
  -> Word64
  -> Memory
  -> Transaction
  -> Either TransactionError ((TransactionId, Unique Transaction), Memory)
broadcast time salt memory transaction =
  maybe (Right ((transactionId, unique), updated)) Left $
  validateTransaction (amounts memory) transaction
  where unique = Unique transaction time salt
        transactionId = TransactionId $ Data.ByteString.unpack
                        $ hash $ toStrict $ encode unique
        updated =
          (\ m -> m { transactions = (transactionId, unique) : transactions m })
          $ transact memory transaction

retrieve
  :: TransactionId
  -> [(TransactionId, Unique Transaction)]
  -> Maybe Transaction
retrieve i = fmap content . lookup i

verifySignature :: Signature -> Address -> Bool
verifySignature s = (== read s)

validateTransaction
  :: [(Address, Amount)] -> Transaction -> Maybe TransactionError
validateTransaction amounts t@(Transaction {..})
  | amount < 1                   = Just $ Invalid amount
  | not $ Set.null sameAddresses = Just $ Duplicated $ Set.toList sameAddresses
  | not $ null invalidSignatures = Just $ InvalidSignatures invalidSignatures
  | not $ null unavailables      = Just $ Insufficient unavailables (total t)
  | not $ null inexistents       = Just $ Inexistent inexistents
  | otherwise                    = Nothing
  where
    sourcesSet      = Set.fromList $ fst <$> NonEmpty.toList sources
    destinationsSet = Set.fromList $ NonEmpty.toList destinations
    sameAddresses = Set.intersection sourcesSet destinationsSet
    invalidSignatures :: [(Address, Signature)]
    invalidSignatures =
      let invalid (address, signature) =
            not $ verifySignature signature address
      in filter invalid $ NonEmpty.toList sources
    unavailable (address, _) = (address, ) <$> case lookup address amounts of
      Nothing -> Just 0.0
      Just availability ->
        if (availability < total t)
        then Just availability
        else Nothing
    unavailables = mapMaybe unavailable $ NonEmpty.toList sources
    addresses = (fst <$> sources) <> destinations
    inexistents =
      filter (isNothing . flip lookup amounts) $ NonEmpty.toList addresses
  
data TransactionError
  = InvalidSignatures [(Address, Signature)]
  | Insufficient [(Address, Amount)] Amount
  | Invalid Amount
  | Inexistent [Address]
  | Duplicated [Address]

instance Show TransactionError where
  show (InvalidSignatures i) =
    "Invalid signatures for " <> show i
  show (Insufficient details total) =
    let summary (address, amount) =
          "user " <> show address <> " has " <> show amount <> " coins left"
    in "Cannot transfer " <> show total <> ": " <> (intercalate ", " $ summary <$> details)
  show (Invalid amo) =
    "Cannot transfer " <> show amo <> " coins"
  show (Inexistent a) =
    "There is no user " <> (intercalate ", " $ show <$> a) <> " on the chain"
  show (Duplicated a) =
    "Transfering from " <> show a <> " to themselves is not allowed"

transact :: Memory -> Transaction -> Memory
transact m t@(Transaction {..}) =
  let update :: (Address, Amount) -> (Address, Amount)
      update (address, previous) =
        let operate f = f previous
        in (address, maybe previous operate $ lookup address operations)
      operations :: [(Address, Amount -> Amount)]
      operations =
        (subtract <$> NonEmpty.toList sources) <>
        (add <$> NonEmpty.toList destinations)
      add addr = (addr, (+ total t))
      subtract (addr, _) = (addr, \a -> a - total t)
  in m { amounts = update <$> amounts m }

chain :: Memory -> Memory
chain m = foldl' transact m (content . snd <$> transactions m)

total :: Transaction -> Amount
total t = amount t * (fromIntegral $ length $ destinations t)
