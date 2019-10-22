{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Explorer.Node
  ( ExplorerNodeParams (..),
    GenesisFile (..),
    NodeLayer (..),
    SocketPath (..),
    initializeAllFeatures,
  )
where

import Cardano.BM.Data.Tracer (ToLogObject (..), nullTracer)
import Cardano.BM.Trace (Trace, appendName, logInfo)
import Cardano.Config.CommonCLI (CommonCLI (..))
import qualified Cardano.Config.CommonCLI as Config
import Cardano.Config.Logging
  ( LoggingCLIArguments,
    LoggingLayer,
    createLoggingFeature,
    llAppendName,
    llBasicTrace,
  )
import qualified Cardano.Config.Partial as Config
import qualified Cardano.Config.Presets as Config
import Cardano.Config.Types
  ( CardanoConfiguration,
    CardanoEnvironment (..),
    RequireNetworkMagic (..),
    ccCore,
    coRequiresNetworkMagic,
  )
import Cardano.Prelude hiding ((%), Nat, atomically, option)
import Cardano.Shell.Lib (GeneralException (ConfigurationError))
import Cardano.Shell.Types
  ( CardanoFeature (..),
    CardanoFeatureInit (..),
    featureCleanup,
    featureInit,
    featureShutdown,
    featureStart,
    featureType,
  )
import qualified Cardano.Wallet.Jormungandr.Network as J
import qualified Codec.Serialise as Serialise
import Control.Exception (throw)
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadSTM.Strict
  ( MonadSTM,
    StrictTMVar,
    atomically,
    newEmptyTMVarM,
    readTMVar,
  )
import Control.Monad.Class.MonadTimer (MonadTimer)
import Crypto.Hash (digestFromByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Functor.Contravariant (contramap)
import Data.Reflection (give)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.Socket
  ( AddrInfo (..),
    Family (..),
    SockAddr (..),
    SocketType (..),
    defaultProtocol,
  )
import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge
import Prelude (String)
import qualified Prelude

data Peer = Peer SockAddr SockAddr deriving (Show)

-- | The product type of all command line arguments
data ExplorerNodeParams
  = ExplorerNodeParams
      { backendConfiguration :: !J.JormungandrBackend
      }

newtype GenesisFile
  = GenesisFile
      { unGenesisFile :: FilePath
      }

newtype SocketPath
  = SocketPath
      { unSocketPath :: FilePath
      }

newtype NodeLayer
  = NodeLayer
      { nlRunNode :: forall m. MonadIO m => m ()
      }

type NodeCardanoFeature =
  CardanoFeatureInit CardanoEnvironment LoggingLayer CardanoConfiguration ExplorerNodeParams NodeLayer

initializeAllFeatures :: ExplorerNodeParams -> IO ([CardanoFeature], NodeLayer)
initializeAllFeatures enp = do
  DB.runMigrations Prelude.id True (enpMigrationDir enp) (LogFileDir "/tmp")
  let fcc = Config.mkCardanoConfiguration $ Config.mergeConfiguration Config.mainnetConfiguration commonCli (enpCommonCLIAdvanced enp)
  finalConfig <- case fcc of
    Left err -> throwIO err
    --TODO: if we're using exceptions for this, then we should use a local
    -- excption type, local to this app, that enumerates all the ones we
    -- are reporting, and has proper formatting of the result.
    -- It would also require catching at the top level and printing.
    Right x -> pure x
  (loggingLayer, loggingFeature) <- createLoggingFeature NoEnvironment finalConfig (enpLogging enp)
  (nodeLayer, nodeFeature) <- createNodeFeature loggingLayer enp finalConfig
  pure ([loggingFeature, nodeFeature], nodeLayer)

-- This is a bit of a pain in the neck but is needed for using cardano-cli.
commonCli :: CommonCLI
commonCli =
  CommonCLI
    { Config.cliSocketDir = Last Nothing,
      Config.cliGenesisFile = Last Nothing,
      Config.cliGenesisHash = Last Nothing,
      Config.cliStaticKeySigningKeyFile = Last Nothing,
      Config.cliStaticKeyDlgCertFile = Last Nothing,
      Config.cliDBPath = Last Nothing
    }

createNodeFeature :: LoggingLayer -> ExplorerNodeParams -> CardanoConfiguration -> IO (NodeLayer, CardanoFeature)
createNodeFeature loggingLayer enp cardanoConfiguration = do
  -- we parse any additional configuration if there is any
  -- We don't know where the user wants to fetch the additional configuration from, it could be from
  -- the filesystem, so we give him the most flexible/powerful context, @IO@.

  -- we construct the layer
  nodeLayer <- featureInit nodeCardanoFeatureInit NoEnvironment loggingLayer cardanoConfiguration enp
  -- Return both
  pure (nodeLayer, nodeCardanoFeature nodeCardanoFeatureInit nodeLayer)

nodeCardanoFeatureInit :: NodeCardanoFeature
nodeCardanoFeatureInit =
  CardanoFeatureInit
    { featureType = "NodeFeature",
      featureInit = featureStart',
      featureCleanup = featureCleanup'
    }
  where
    featureStart' :: CardanoEnvironment -> LoggingLayer -> CardanoConfiguration -> ExplorerNodeParams -> IO NodeLayer
    featureStart' _ loggingLayer cc enp =
      pure $ NodeLayer {nlRunNode = liftIO $ runClient enp (mkTracer loggingLayer) cc}
    featureCleanup' :: NodeLayer -> IO ()
    featureCleanup' _ = pure ()
    mkTracer :: LoggingLayer -> Trace IO Text
    mkTracer loggingLayer = llAppendName loggingLayer "explorer-db-node" (llBasicTrace loggingLayer)

nodeCardanoFeature :: NodeCardanoFeature -> NodeLayer -> CardanoFeature
nodeCardanoFeature nodeCardanoFeature' nodeLayer =
  CardanoFeature
    { featureName = featureType nodeCardanoFeature',
      featureStart = pure (),
      featureShutdown = liftIO $ (featureCleanup nodeCardanoFeature') nodeLayer
    }

runClient :: ExplorerNodeParams -> Trace IO Text -> CardanoConfiguration -> IO ()
runClient enp trce cc = do
  gc <- readGenesisConfig enp cc
  -- If the DB is empty it will be inserted, otherwise it will be validated (to make
  -- sure we are on the right chain).
  insertValidateGenesisDistribution trce gc
  give (Genesis.configEpochSlots gc)
    $ give (Genesis.gdProtocolMagicId $ Genesis.configGenesisData gc)
    $ runExplorerNodeClient (mkProtocolId gc) trce (unSocketPath $ enpSocketPath enp)

followChain
  :: Trace IO TextIO ()
  -> J.JormungandrBackend
  -> IO ()
followChain tracer backend = J.withNetworkLayer tracer backend $ \case
  Left err -> throwIO err
  Right (_connParams, networkLayer) -> do
