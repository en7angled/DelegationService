{-# LANGUAGE DataKinds #-}

module DelegationAppMonad where

import Control.Monad.Catch (catch)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask, local), ReaderT, runReaderT)
import GeniusYield.TxBuilder
import GeniusYield.Types (GYTx, GYTxId, PlutusVersion (PlutusV3))
import MGServantUtils.Auth (AuthContext)
import MGServantUtils.Handlers (handleException)
import Servant
import TxBuilding.Interactions
import TxBuilding.MonadInteraction
import TxBuilding.Operations (DelegationAction (..), TxBuildingContext (..))
import TxBuilding.Types

data DelegationAppContext = DelegationAppContext
  { authContext :: AuthContext,
    txBuildingContext :: TxBuildingContext
  }

newtype DelegationAppMonad a = DelegationAppMonad {unDelegationAppMonad :: ReaderT DelegationAppContext Servant.Handler a}
  deriving (Functor, Applicative, Monad)

runDelegationAppMonad :: DelegationAppContext -> DelegationAppMonad a -> Servant.Handler a
runDelegationAppMonad ctx app = runReaderT (unDelegationAppMonad app) ctx `catch` handleException @GYTxMonadException

instance MonadIO DelegationAppMonad where
  liftIO :: IO a -> DelegationAppMonad a
  liftIO = DelegationAppMonad . liftIO

instance MonadReader DelegationAppContext DelegationAppMonad where
  ask :: DelegationAppMonad DelegationAppContext
  ask = DelegationAppMonad ask

  local :: (DelegationAppContext -> DelegationAppContext) -> DelegationAppMonad a -> DelegationAppMonad a
  local f (DelegationAppMonad app) = DelegationAppMonad (local f app)

buildTxApp :: Interaction DelegationAction -> DelegationAppMonad String
buildTxApp = mkInteractionHexEncodedCBOR

submitTxApp :: GYTx -> DelegationAppMonad GYTxId
submitTxApp = submitTransactionAndWaitForConfirmation

instance MonadInteraction DelegationAction TxBuildingContext DelegationAppMonad where
  getProviderCtx :: DelegationAppMonad ProviderCtx
  getProviderCtx = DelegationAppMonad $ do
    DelegationAppContext {txBuildingContext} <- ask
    pure (providerCtx txBuildingContext)

  getTxBuildingContext :: DelegationAppMonad TxBuildingContext
  getTxBuildingContext = DelegationAppMonad $ do
    DelegationAppContext {txBuildingContext} <- ask
    pure txBuildingContext

  mkInteractionSkeleton :: (GYTxUserQueryMonad n) => Interaction DelegationAction -> TxBuildingContext -> DelegationAppMonad (n (GYTxSkeleton PlutusV3))
  mkInteractionSkeleton i a = return $ interactionToTxSkeleton i a
