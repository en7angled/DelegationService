{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fewer imports" #-}

module RestAPI where

import Control.Lens hiding (Context)
import Data.Swagger
import Data.Text hiding (length)
import DelegationAppMonad
import GeniusYield.Imports
import GeniusYield.Types hiding (title)
import Network.Wai
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Servant
import Servant.Swagger
import Servant.Swagger.UI
import MGServantUtils.Auth
import MGServantUtils.CORS
import MGServantUtils.ServiceProbe
import TxBuilding.Operations (DelegationAction (..))
import TxBuilding.Types

-- Auth code moved to MGServantUtils.Auth

------------------------------------------------------------------------------------------------

--  Transactions API

------------------------------------------------------------------------------------------------

type Transactions =
  -- Build tx endpoint
  ( Summary "Build Transaction"
      :> Description "Builds Transaction for Interaction and returns it as a hex encoded CBOR"
      :> "build-tx"
      :> ReqBody '[JSON] (Interaction DelegationAction)
      :> Post '[JSON] String
  )
    :<|>
    -- Submit tx endpoint
    ( Summary "Submits Signed Transaction"
        :> Description "Submits Signed Transaction and returns the transaction id"
        :> "submit-tx"
        :> ReqBody '[JSON] AddWitAndSubmitParams
        :> Post '[JSON] GYTxId
    )

-- Health handlers moved to MGServantUtils.ServiceProbe

handleBuildTx :: Interaction DelegationAction -> DelegationAppMonad String
handleBuildTx = buildTxApp

handleSubmitTx :: AddWitAndSubmitParams -> DelegationAppMonad GYTxId
handleSubmitTx AddWitAndSubmitParams {..} = do
  let txBody = getTxBody awasTxUnsigned
  let signedTx = makeSignedTransaction awasTxWit txBody
  submitTxApp signedTx

transactionsServer :: ServerT Transactions DelegationAppMonad
transactionsServer = handleBuildTx :<|> handleSubmitTx

------------------------------------------------------------------------------------------------

--  Probe API

------------------------------------------------------------------------------------------------

type ProbeAPI = ServiceProbe Text Text

interactionProbeServer :: ServerT ProbeAPI DelegationAppMonad
interactionProbeServer = alwaysHealthy "delegation-service" :<|> alwaysReady "delegation-service"

------------------------------------------------------------------------------------------------

--  Swagger API

------------------------------------------------------------------------------------------------

type PublicAPI = ProbeAPI :<|> Transactions

proxyPublicAPI :: Proxy PublicAPI
proxyPublicAPI = Proxy

apiSwagger :: Swagger
apiSwagger =
  toSwagger proxyPublicAPI
    & info . title .~ "Delegation Service API"
    & info . Data.Swagger.version .~ "1.0"
    & info . Data.Swagger.description ?~ "This is the Delegation Service API - handles transaction building and submission"
    & info
      . license
      ?~ "Apache-2.0 license"
    & host .~ Nothing

swaggerServer :: ServerT (SwaggerSchemaUI "swagger-ui" "swagger-api.json") DelegationAppMonad
swaggerServer = swaggerSchemaUIServerT apiSwagger

------------------------------------------------------------------------------------------------

--  Combined API

------------------------------------------------------------------------------------------------

-- | Adding Basic Auth to the Rest API
type PrivateTransactions =
  BasicAuth "user-realm" AuthUser :> Transactions

proxyPrivateTransactions :: Proxy PrivateTransactions
proxyPrivateTransactions = Proxy

privateTransactionsServer :: ServerT PrivateTransactions DelegationAppMonad
privateTransactionsServer = const transactionsServer

-- | Adding Swagger UI on top of Private Rest API
type FullAPI =
  SwaggerSchemaUI "swagger-ui" "swagger-api.json"
    :<|> ProbeAPI
    :<|> PrivateTransactions

proxyFullAPI :: Proxy FullAPI
proxyFullAPI = Proxy

fullServer :: ServerT FullAPI DelegationAppMonad
fullServer = swaggerServer :<|> interactionProbeServer :<|> privateTransactionsServer

------------------------------------------------------------------------------------------------

-- | Servant Application

------------------------------------------------------------------------------------------------

mkDelegationApp :: DelegationAppContext -> Application
mkDelegationApp ctx =
  MGServantUtils.CORS.setupCors $
    provideOptions proxyPublicAPI $
      serveWithContext proxyFullAPI basicCtx hoistedServer
  where
    basicCtx = basicAuthServerContext (authContext ctx)
    hoistedServer = hoistServerWithContext proxyFullAPI proxyBasicAuthContext (runDelegationAppMonad ctx) fullServer
