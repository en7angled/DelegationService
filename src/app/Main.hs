import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Maybe
import Data.String (IsString (..))
import GeniusYield.GYConfig
import GeniusYield.Types
import DelegationAppMonad (DelegationAppContext (..))
import Network.Wai.Handler.Warp
import RestAPI (apiSwagger, mkDelegationApp)
import MGServantUtils.Utils (getPortFromEnvOrDefault)
import Utils.Config  
import MGServantUtils.Auth (getBasicAuthFromEnv)
import System.Directory 
import TxBuilding.Types (ProviderCtx(..))
import TxBuilding.Operations (TxBuildingContext(..))

defaultAtlasCoreConfig :: FilePath
defaultAtlasCoreConfig = "config/config_atlas.json"

-- entangledPoolId :: GYStakePoolId
-- entangledPoolId = unsafeStakePoolIdFromText "pool129n0d9zrla7ntfjlwhqrtmn7halem0shcjd5mz5zhfym2auyu05" --- "pool1sj3gnahsms73uxxu43rgwczdw596en7dtsfcqf6297vzgcedquv"

-- myDRepId :: GYCredential GYKeyRoleDRep
-- myDRepId = GYCredentialByKey (keyHashFromApi @'GYKeyRoleDRep "6bec808ca4fae34548a6384e79bf18877914a04b57d5022d56007d1b") -- "70687a06149aafc3c89492b06e743a10a051327371d14f8f95e3c605")



main :: IO ()
main = do
  putStrLn "Writing Swagger file ..."
  createDirectoryIfMissing True "./docs/swagger/"
  BL8.writeFile "./docs/swagger/delegation-swagger-api.json" (encodePretty apiSwagger)

  atlasConfig <- Data.Maybe.fromMaybe (error "Atlas configuration failed") <$> decodeConfigEnvOrFile "ATLAS_CORE_CONFIG" defaultAtlasCoreConfig
  withCfgProviders atlasConfig (read @GYLogNamespace "DelegationApp") $ \providers -> do
    let providersContext = ProviderCtx atlasConfig providers
    let txBuildingContext = TxBuildingContext providersContext
    authContext <- getBasicAuthFromEnv
    let appContext = DelegationAppContext authContext txBuildingContext

    let host = "0.0.0.0"
    port <- getPortFromEnvOrDefault 8082

    let settings = setHost (fromString host :: HostPreference) $ setPort port defaultSettings
    let delegationApp = mkDelegationApp appContext

    putStrLn $ "Started Delegation Service at " <> host <> " " <> show port
    putStrLn $ "Atlas config: " <> show atlasConfig
    putStrLn $ "Swagger-UI available at : http://" <> host <> ":" <> show port <> "/swagger-ui"

    runSettings settings delegationApp