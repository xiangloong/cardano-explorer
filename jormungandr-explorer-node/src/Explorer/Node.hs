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
    runClient,
  )
where

import Cardano.BM.Data.Tracer (ToLogObject (..), nullTracer)
import Cardano.BM.Trace (Trace, appendName, logInfo)
import Cardano.Config.CommonCLI (CommonCLI (..))
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
import qualified Cardano.Wallet.Network as J
import qualified Codec.Serialise as Serialise
import Control.Exception (throw)
import Control.Monad.Class.MonadST (MonadST)
import Data.Text (Text)
import Prelude (String)
import qualified Prelude

-- | The product type of all command line arguments
data ExplorerNodeParams
  = ExplorerNodeParams
      { backendConfiguration :: !J.JormungandrBackend
      }

runClient :: ExplorerNodeParams -> Trace IO Text -> IO ()
runClient enp trce =
  followChain trce $ backendConfiguration enp

followChain ::
  Trace IO Text ->
  J.JormungandrBackend ->
  IO ()
followChain tracer backend = J.withNetworkLayer tracer backend $ \case
  Left err -> throwIO err
  Right (_connParams, networkLayer) -> void . runExceptT $ do
    tip <- J.networkTip networkLayer
    print tip
