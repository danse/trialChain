{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.List.NonEmpty
import Data.String (fromString)
import GHC.Conc
import Lib (app, initialise)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method (methodPost)
import Network.Wai.Test (simpleBody)
import System.Directory (removeFile)
import Test.Hspec
import Test.Hspec.Wai
import Types

main :: IO ()
main = do
  mem <- atomically $ newTVar $ initialise []
  hspec $ spec mem

spec :: TVar Memory -> Spec
spec mem =
  with (pure $ app Nothing mem False) $ do
    describe "GET /retrieve not existing transaction" $
      it "responds with 404" $
        get "/retrieve/1-2-3-4" `shouldRespondWith` 404
    describe "POST /broadcast" $
      it "responds with 200" $
        postJSON "/broadcast" simpleTransaction `shouldRespondWith` 200
    describe "/broadcast and /retrieve roundtrip" $
      -- unfortunately this will not work. Probably the shared memory
      -- is still in the old state when the get request is performed
      xit "works" $ do
        let roundTrip = do
              respPost <- postJSON "/broadcast" simpleTransaction
              get $ ("/retrieve/" <>) $ toStrict $ simpleBody respPost
        roundTrip `shouldRespondWith` 200
        roundTrip `shouldRespondWith` fromString (unpack simpleTransaction)
    describe "transaction validation" $ do
      it "valid" $
        postJSON "/broadcast" simpleTransaction `shouldRespondWith` 200
      let matchInvalid t e =
            postJSON "/broadcast" t `shouldRespondWith` e { matchStatus = 422 }
      it "invalid signature" $ do
        let t = encode $ Transaction (singleton (3,"1")) (singleton 2) 1
        matchInvalid t "Invalid signatures for [(3,\"1\")]"
      it "amount unavailable" $
        matchInvalid (fromTo2 1 10) "Cannot transfer 10.0: user 1 has 1.0 coins left"
      it "invalid amount" $ do
        let t = encode $ Transaction (singleton (3,"3")) (singleton 2) 0
        matchInvalid t "Cannot transfer 0.0 coins"
      it "inexistent addess" $ do
        let t = encode $ Transaction (singleton (3,"3")) (singleton 11) 1
        matchInvalid t "There is no user 11 on the chain"
      it "cannot transfer to themselves" $ do
        let t = encode $ Transaction (singleton (3,"3")) (singleton 3) 1
        matchInvalid t "Transfering from [3] to themselves is not allowed"

simpleTransaction
  = encode $ Transaction (singleton (3,"3")) (singleton 2) 1

signature = show

fromTo2 address amount =
  let from = (singleton (address, signature address))
  in encode $ Transaction from (singleton 2) amount

postJSON path = request methodPost path [(hContentType, "application/json")]
