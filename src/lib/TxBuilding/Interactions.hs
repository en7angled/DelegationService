{-# LANGUAGE FlexibleInstances #-}

module TxBuilding.Interactions where

import GHC.Stack
import GeniusYield.TxBuilder
import GeniusYield.Types
import Safe
import TxBuilding.Exceptions (TxBuildingAppException (..))
import TxBuilding.Operations
import TxBuilding.Types

interactionToTxSkeleton ::
  (HasCallStack, GYTxUserQueryMonad m) =>
  Interaction DelegationAction ->
  TxBuildingContext ->
  m (GYTxSkeleton 'PlutusV3)
interactionToTxSkeleton Interaction {..} txBuildingContext = do
  -- let changeAddr = changeAddress userAddresses
  -- let usedAddrs = usedAddresses userAddresses
  -- let receiveAddr = fromMaybe changeAddr recipient
  let maybeStakeAddr = headMay $ stakeAddresses userAddresses
  case maybeStakeAddr of
    (Just stakeAddr) -> delegationActionSkeleton txBuildingContext action stakeAddr
    _ -> throwError (GYApplicationException NoStakeAddressProvided)
