{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Provides a function to launch the jormungandr node backend for integration
-- tests.
--
-- Copied temporarily from https://github.com/input-output-hk/cardano-wallet/blob/master/lib/jormungandr/test/integration/Cardano/Wallet/Jormungandr/Launch.hs
module Explorer.Node.Launch
  ( withConfig,
  )
where

import Cardano.BM.Data.Severity
  ( Severity (..),
  )
import Cardano.Wallet.Jormungandr.Network
  ( JormungandrConfig (..),
  )
import Cardano.Wallet.Network.Ports
  ( PortNumber,
    getRandomPort,
  )
import Control.Exception
  ( bracket,
  )
import Data.Aeson
  ( (.=),
    Value (..),
    object,
  )
import qualified Data.Aeson as Aeson
import Data.Function
  ( (&),
  )
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import System.Directory
  ( doesDirectoryExist,
    removeDirectoryRecursive,
  )
import System.Environment
  ( lookupEnv,
  )
import System.FilePath
  ( (</>),
    FilePath,
  )
import System.IO
  ( IOMode (..),
    hClose,
    openFile,
  )
import System.IO.Temp
  ( createTempDirectory,
    getCanonicalTemporaryDirectory,
  )
import System.Process
  ( StdStream (..),
  )
import Prelude

-- | Starts jormungandr on a random port using the integration tests config.
-- The data directory will be stored in a unique location under the system
-- temporary directory.
withConfig :: (JormungandrConfig -> IO a) -> IO a
withConfig = bracket setupConfig teardownConfig

setupConfig :: IO JormungandrConfig
setupConfig = do
  let dir = "../cardano-wallet/lib/jormungandr/test/data/jormungandr"
  tmp <- getCanonicalTemporaryDirectory
  configDir <- createTempDirectory tmp "cardano-wallet-jormungandr"
  logFile <- openFile (configDir </> "jormungandr.log") WriteMode
  let cfg =
        JormungandrConfig
          configDir
          (Right $ dir </> "block0.bin")
          Nothing
          (UseHandle logFile)
          ["--secret", dir </> "secret.yaml"]
  genConfigYaml cfg
  pure cfg

teardownConfig :: JormungandrConfig -> IO ()
teardownConfig (JormungandrConfig d _ _ output _) = do
  case output of
    UseHandle h -> hClose h
    _ -> pure ()
  override <- maybe False (not . null) <$> lookupEnv "NO_CLEANUP"
  exists <- doesDirectoryExist d
  case (override, exists) of
    (True, _) -> putStrLn $ "Not cleaning up temporary directory " ++ d
    (_, True) -> removeDirectoryRecursive d
    _ -> pure ()

{-------------------------------------------------------------------------------
                           Generate YAML config file
-------------------------------------------------------------------------------}

genConfigYaml :: JormungandrConfig -> IO ()
genConfigYaml (JormungandrConfig stateDir _ _ _ _) = do
  p2pPort <- getRandomPort
  genConfigFile stateDir p2pPort
    & Yaml.encodeFile nodeConfigFile
  where
    nodeConfigFile = stateDir </> "jormungandr-config.yaml"

-- | Generate a configuration file for Jörmungandr@0.3.999
genConfigFile ::
  FilePath ->
  PortNumber ->
  Aeson.Value
genConfigFile stateDir addressPort =
  object
    [ "storage" .= (stateDir </> "chain"),
      "p2p"
        .= object
          [ "trusted_peers" .= ([] :: [()]),
            "topics_of_interest"
              .= object
                [ "messages" .= String "low",
                  "blocks" .= String "normal"
                ],
            "public_address" .= String publicAddress
          ]
    ]
  where
    publicAddress = T.pack $ mconcat ["/ip4/127.0.0.1/tcp/", show addressPort]
