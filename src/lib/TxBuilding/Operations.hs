module TxBuilding.Operations where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), object, withObject, (.=))
import Data.Maybe (isJust)
import Data.String
import Data.Swagger (ToSchema (..))
import qualified Data.Text as Text
import Deriving.Aeson
import GeniusYield.TxBuilder
import GeniusYield.Types
import TxBuilding.Types (ProviderCtx)

-- entangledPoolId :: GYStakePoolId
-- entangledPoolId = unsafeStakePoolIdFromText "pool129n0d9zrla7ntfjlwhqrtmn7halem0shcjd5mz5zhfym2auyu05" --- "pool1sj3gnahsms73uxxu43rgwczdw596en7dtsfcqf6297vzgcedquv"

-- myDRepId :: GYCredential GYKeyRoleDRep
-- myDRepId = GYCredentialByKey (keyHashFromApi @'GYKeyRoleDRep "6bec808ca4fae34548a6384e79bf18877914a04b57d5022d56007d1b") -- "70687a06149aafc3c89492b06e743a10a051327371d14f8f95e3c605")

newtype TxBuildingContext = TxBuildingContext
  { providerCtx :: ProviderCtx
  }
  deriving stock (Generic)

data DelegationAction
  = PoolDelegation
      {poolId :: String}
  | DRepDelegation {dRepHash :: String}
  | PoolAndDRepDelegation {poolId :: String, dRepHash :: String}
  deriving (Show, Generic)
  deriving (ToSchema)

instance FromJSON DelegationAction where
  parseJSON = withObject "DelegationAction" $ \obj -> do
    tag <- obj .: "tag"
    contents <- obj .: "contents"
    case tag of
      "PoolDelegation" -> do
        poolId <- contents .: "poolId"
        return $ PoolDelegation poolId
      "DRepDelegation" -> do
        dRepHash <- contents .: "dRepHash"
        return $ DRepDelegation dRepHash
      "PoolAndDRepDelegation" -> do
        poolId <- contents .: "poolId"
        dRepHash <- contents .: "dRepHash"
        return $ PoolAndDRepDelegation poolId dRepHash
      _ -> fail $ "Unknown DelegationAction tag: " ++ Text.unpack tag

instance ToJSON DelegationAction where
  toJSON (PoolDelegation poolId) =
    object ["tag" .= ("PoolDelegation" :: Text.Text), "contents" .= object ["poolId" .= poolId]]
  toJSON (DRepDelegation dRepHash) =
    object ["tag" .= ("DRepDelegation" :: Text.Text), "contents" .= object ["dRepHash" .= dRepHash]]
  toJSON (PoolAndDRepDelegation poolId dRepHash) =
    object
      [ "tag" .= ("PoolAndDRepDelegation" :: Text.Text),
        "contents" .= object ["poolId" .= poolId, "dRepHash" .= dRepHash]
      ]

--- >>> toJSON (DRepDelegation "6bec808ca4fae34548a6384e79bf18877914a04b57d5022d56007d1b")
-- Object (fromList [("dRepHash",String "6bec808ca4fae34548a6384e79bf18877914a04b57d5022d56007d1b"),("tag",String "DRepDelegation")])

---- >>> fromJSON "{\"tag\":\"DRepDelegation\",\"contents\":{\"dRepHash\":\"6bec808ca4fae34548a6384e79bf18877914a04b57d5022d56007d1b\"}}"
-- Success ()

delegationActiontoGY :: DelegationAction -> GYDelegatee
delegationActiontoGY (PoolDelegation poolId) = GYDelegStake (unsafeStakePoolIdFromText (Text.pack poolId))
delegationActiontoGY (DRepDelegation dRepId) = GYDelegVote (GYDRepCredential (GYCredentialByKey (keyHashFromApi @'GYKeyRoleDRep (fromString dRepId))))
delegationActiontoGY (PoolAndDRepDelegation poolId dRepId) =
  GYDelegStakeVote
    (unsafeStakePoolIdFromText (Text.pack poolId))
    (GYDRepCredential (GYCredentialByKey (keyHashFromApi @'GYKeyRoleDRep (fromString dRepId))))

------------------------------------------------------------------------------------------------

-- * Operations

------------------------------------------------------------------------------------------------

delegationActionSkeleton :: (GYTxUserQueryMonad m) => TxBuildingContext -> DelegationAction -> GYStakeAddress -> m (GYTxSkeleton 'PlutusV3)
delegationActionSkeleton _ delegationAction stakeAddress = do
  mStakingInfo <- stakeAddressInfo stakeAddress
  let isRegistered = isJust mStakingInfo
  let registrationCert = mkStakeAddressRegistrationCertificate (stakeAddressToCredential stakeAddress) GYTxBuildWitnessKey
  let delegationCert = mkStakeAddressDelegationCertificate (stakeAddressToCredential stakeAddress) (delegationActiontoGY delegationAction) GYTxBuildWitnessKey
  return $
    mconcat
      [ if isRegistered then mempty else mustHaveCertificate registrationCert,
        mustHaveCertificate delegationCert
      ]
