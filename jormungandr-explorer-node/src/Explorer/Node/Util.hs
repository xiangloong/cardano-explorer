{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Explorer.Node.Util
  ( leftPanic,
    mkSlotLeader,
    textShow,
  )
where

import Cardano.Prelude
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.Text as Text
import qualified Explorer.DB as DB

textShow :: Show a => a -> Text
textShow = Text.pack . show

leftPanic :: Text -> Either DB.LookupFail a -> a
leftPanic msg =
  \case
    Left err -> panic $ msg <> DB.renderLookupFail err
    Right val -> val

mkSlotLeader :: Maybe W.PoolId -> DB.SlotLeader
mkSlotLeader Nothing = DB.SlotLeader "12345" "No Praos leader for this slot"
mkSlotLeader (Just (W.PoolId pid)) = DB.SlotLeader pid "Pool ID"
