{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Explorer.Node.Insert
  ( insertBlockList,
  )
where

import Cardano.BM.Trace (Trace, logDebug, logInfo)
import Cardano.Prelude
import qualified Cardano.Wallet.Jormungandr.Binary as J
import qualified Cardano.Wallet.Jormungandr.Primitive.Types as J
import qualified Cardano.Wallet.Primitive.Types as W
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Text.Class (toText)
import Database.Persist.Sql (SqlBackend)
import qualified Explorer.DB as DB
import Explorer.Node.Util

insertBlockList ::
  Trace IO Text ->
  [(J.Block, Word64)] ->
  IO ()
insertBlockList tracer blks =
  -- Setting this to True will log all 'Persistent' operations which is great
  -- for debugging, but otherwise *way* too chatty.
  if False
    then DB.runDbIohkLogging tracer $ mapM_ (uncurry $ insertBlock tracer) blks
    else DB.runDbNoLogging $ mapM_ (uncurry $ insertBlock tracer) blks

insertBlock ::
  MonadIO m =>
  Trace IO Text ->
  J.Block ->
  -- | Tip block number
  Word64 ->
  ReaderT SqlBackend m ()
insertBlock tracer blk tipBlockNo = do
  meta <- leftPanic "insertBlock: " <$> DB.queryMeta
  pbid <-
    leftPanic "insertBlock: "
      <$> DB.queryBlockId (W.getHash $ J.parentHeaderHash hdr)
  let flatSlot =
        W.flatSlot
          (W.EpochLength . fromIntegral $ DB.metaSlotDuration meta)
          (J.slot hdr)
  slid <- DB.insertSlotLeader $ mkSlotLeader (J.producedBy hdr)
  blkId <- DB.insertBlock $
    DB.Block
      { DB.blockHash = W.getHash $ J.headerHash hdr,
        DB.blockEpochNo = Just $ W.unEpochNo . W.epochNumber . J.slot $ hdr,
        DB.blockSlotNo =
          Just
            $ fromIntegral
              . W.unSlotNo
              . W.slotNumber
              . J.slot
            $ hdr,
        DB.blockBlockNo = Just . fromIntegral $ J.chainLength hdr,
        DB.blockPrevious = Just pbid,
        DB.blockMerkelRoot = Just $ W.getHash $ J.contentHash hdr,
        DB.blockSlotLeader = slid,
        DB.blockSize = fromIntegral $ J.contentSize hdr,
        DB.blockTime = DB.slotUtcTime meta flatSlot
      }
  mapM_ (insertMessage tracer blkId) $ J.messages blk
  liftIO $ logger tracer $
    mconcat
      [ "insertBlock: slot ",
        textShow (J.slot hdr),
        ", block ",
        textShow (J.chainLength hdr),
        ", hash ",
        toText (J.headerHash hdr)
      ]
  where
    hdr = J.header blk
    logger :: Trace IO a -> a -> IO ()
    logger =
      if tipBlockNo - fromIntegral (J.chainLength hdr) < 20
        then logInfo
        else logDebug

insertMessage ::
  MonadIO m =>
  Trace IO Text ->
  DB.BlockId ->
  J.Message ->
  ReaderT SqlBackend m ()
insertMessage tracer blkId = \case
  J.Initial _ -> liftIO . logDebug tracer $ "Initial configuration not added"
  J.UnimplementedMessage i ->
    liftIO . logDebug tracer $
      mconcat
        [ "insertMessage: unimplemented message ",
          textShow i
        ]
  J.Transaction (tx, _) -> insertTx tracer blkId tx

insertTx ::
  MonadIO m =>
  Trace IO Text ->
  DB.BlockId ->
  J.Tx ->
  ReaderT SqlBackend m ()
insertTx tracer blkId tx = do
  fee <- calculateTxFee tx
  txId <- DB.insertTx $
    DB.Tx
      { DB.txHash = W.getHash $ J.txid tx,
        DB.txBlock = blkId,
        DB.txFee = fee
      }
  -- Insert outputs for a transaction before inputs in case the inputs for this transaction
  -- references the output (noit sure this can even happen).
  zipWithM_ (insertTxOut tracer txId) [0 ..] (J.outputs tx)
  mapM_ (insertTxIn tracer txId) (fst <$> J.inputs tx)

insertTxOut ::
  MonadIO m =>
  Trace IO Text ->
  DB.TxId ->
  Word32 ->
  W.TxOut ->
  ReaderT SqlBackend m ()
insertTxOut _tracer txId index txout =
  void . DB.insertTxOut $
    DB.TxOut
      { DB.txOutTxId = txId,
        DB.txOutIndex = fromIntegral index,
        DB.txOutAddress = toText $ W.address txout,
        DB.txOutValue = W.getCoin $ W.coin txout
      }

insertTxIn ::
  MonadIO m =>
  Trace IO Text ->
  DB.TxId ->
  W.TxIn ->
  ReaderT SqlBackend m ()
insertTxIn _tracer txInId (W.TxIn txHash inIndex) = do
  txOutId <-
    leftPanic "insertTxIn: "
      <$> DB.queryTxId (W.getHash txHash)
  void $ DB.insertTxIn $
    DB.TxIn
      { DB.txInTxInId = txInId,
        DB.txInTxOutId = txOutId,
        DB.txInTxOutIndex = fromIntegral inIndex
      }

--------------------------------------------------------------------------------

calculateTxFee :: MonadIO m => J.Tx -> ReaderT SqlBackend m Word64
calculateTxFee tx =
  if outval > inval
    then panic $ "calculateTxFee: " <> textShow (outval, inval)
    else pure $ inval - outval
  where
    inval :: Word64
    inval = sum $ (W.getCoin . snd) <$> J.inputs tx
    outval :: Word64
    outval = sum $ map (W.getCoin . W.coin) $ J.outputs tx
