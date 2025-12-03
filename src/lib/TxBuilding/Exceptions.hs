module TxBuilding.Exceptions where

import Control.Exception
import GHC.Generics (Generic)
import Prelude qualified
import GeniusYield.HTTP.Errors (IsGYApiError)

------------------------------------------------------------------------------------------------

-- * Custom Exceptions

------------------------------------------------------------------------------------------------

-- | Custom exception for profile operations
data TxBuildingAppException
  = NoStakeAddressProvided
  deriving stock (Generic, Prelude.Show, Prelude.Eq)

instance Exception TxBuildingAppException where
  displayException :: TxBuildingAppException -> Prelude.String
  displayException NoStakeAddressProvided = "No stake address provided"

instance IsGYApiError TxBuildingAppException 