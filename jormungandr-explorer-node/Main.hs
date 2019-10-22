{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.BM.Data.Tracer (contramap, debugTracer, nullTracer, toLogObject)
import Data.Text.Class (fromText)
import Explorer.Node
import Explorer.Node.Launch (withConfig)
import qualified Cardano.Wallet.Jormungandr.Network as J
import System.Process (StdStream(..))
import Cardano.BM.Data.Severity (Severity(..))
import Cardano.Wallet.Primitive.Types (Hash(..))

main :: IO ()
main = withConfig $ \config ->
  let enp = ExplorerNodeParams $ J.Launch config
  in runClient enp (contramap show debugTracer)
