{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib (app, startApp, initialise) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import Data.ByteString (unpack)
import Data.Char (ord)
import Data.List (intercalate)
import Data.Time
import Data.Text (splitOn, unpack)
import Data.Word (Word64)
import GHC.Conc
import GHC.Float (rationalToDouble)
import GHC.Num.Integer (integerFromInt)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant hiding (Unique)
import System.IO (hPutStrLn, stderr)
import System.Random
import Types

import qualified Data.ByteString.Lazy.Char8
import qualified Persistence
import qualified TrialChain

type Broadcast =
  "broadcast" :> ReqBody '[JSON] Transaction :> Post '[JSON] TransactionId

type Retrieve =
  "retrieve" :> Capture "transactionId" TransactionId :> Get '[JSON] Transaction

type API = Broadcast :<|> Retrieve

broadcast
  :: Maybe FilePath -> TVar Memory -> Transaction -> Handler TransactionId
broadcast path var transaction = do
  time <- liftIO getCurrentTime
  gen <- initStdGen
  let (salt, _) = genWord64 gen 
  newTransaction <- liftIO $ do
    newTransaction <- atomically $ do
      memory <- readTVar var
      case TrialChain.broadcast time salt memory transaction of
        Right (newTransaction, newMemory) -> do
            writeTVar var newMemory
            pure $ Right newTransaction
        Left e -> pure $ Left e
    let writeId w p t = w p t >> pure t
    case newTransaction of
      Right n -> do
        maybe pure (writeId Persistence.write) path $ n
        pure $ Right n
      Left e ->
        pure $ Left e
  let withDetails err e =
        err { errBody = Data.ByteString.Lazy.Char8.pack $ show e }
  either (throwError . withDetails err422) (pure . fst) newTransaction

retrieve :: TVar Memory -> Bool -> TransactionId -> Handler Transaction
retrieve var verbose transactionId = do
  t <- transactions <$> (liftIO $ atomically $ readTVar var)
  maybe (notFound t) pure $ TrialChain.retrieve transactionId t
  where
    showIds all =
      intercalate ", " $ fmap show $ fst <$> all
    error all =
      show transactionId <> " not found in " <> showIds all
    notFound :: [(TransactionId, Unique Transaction)] -> Handler Transaction
    notFound all = do
      when verbose $ liftIO $ hPutStrLn stderr $ error all
      throwError err404

instance FromHttpApiData TransactionId where
  parseUrlPiece = Right . read . Data.Text.unpack

server :: Maybe FilePath -> TVar Memory -> Bool -> Server API
server path mem verbose = broadcast path mem :<|> retrieve mem verbose

api :: Proxy API
api = Proxy

app :: Maybe FilePath -> TVar Memory -> Bool -> Application
app path mem verbose = serve api $ server path mem verbose

initialise t =
  Memory {
    transactions = t,
    amounts = (\ a -> (a, rationalToDouble (integerFromInt a) 1)) <$> [1..10]
    }

startApp :: IO ()
startApp = do
  let path = "data"
  mem <- atomically . newTVar . TrialChain.chain . initialise
         =<< Persistence.read path
  run 8080 $ app (Just path) mem False
