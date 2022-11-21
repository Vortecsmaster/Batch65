{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Batch65SignedMinting where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

--ON-CHAIN RELATED CODE

{-# INLINABLE signedMintingPolicy  #-}
signedMintingPolicy :: PaymentPubKeyHash -> () -> ScriptContext -> Bool
signedMintingPolicy pkh () sContext = txSignedBy (scriptContextTxInfo sContext) $ unPaymentPubKeyHash pkh

policy :: PaymentPubKeyHash -> Scripts.MintingPolicy
policy pkh = mkMintingPolicyScript $ 
           $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . signedMintingPolicy  ||])
           `PlutusTx.applyCode`
           PlutusTx.liftCode pkh

curSymbol :: PaymentPubKeyHash -> CurrencySymbol
curSysmbol = scriptCurrencySymbol . policy


--OFF-CHAIN
data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmout     :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema) 

type FreeSchema = Endpoint "mint" MintParams


mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do
    pkh <- Contract.ownPaymentPubKeyHash
    let val     = Value.singleton (curSymbol pkh) (mpTokenName mp) (mpAmout mp)
        lookups = Constraints.mintingPolicy $ policy pkh
        tx      = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "Forged %s" (show val)

{-
cardano-cli transaction build \
$era \
--tx-in $utxoin \
--tx-out $address+$output+"$tokenamount $policyid.$tokenname" \
--change-address $address \
--mint="$tokenamount $policyid.$tokenname" \
--minting-script-file $script \
--metadata-json-file metadata.json  \
--invalid-hereafter $slotnumber \
--witness-override 2 \
--out-file matx.raw $TESTNET
-}



endpoints :: Contract () FreeSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

mkSchemaDefinitions ''FreeSchema
mkKnownCurrencies []