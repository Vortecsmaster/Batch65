{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveAnyClass              #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeApplications            #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE DerivingStrategies          #-}
{-# LANGUAGE ImportQualifiedPost         #-}
{-# LANGUAGE TupleSections               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module HourControl where

--OnChain code related (PlutusTx)
import PlutusTx                  qualified
import PlutusTx.Prelude                                 hiding (pure, (<$>), Semigroup (..))
--OffChain code related
import Plutus.Contract
--Types and fucntions
import Ledger                                           (Address, Datum (Datum), ScriptContext, Validator, Value, getCardanoTxId)
import qualified Ledger                    
import qualified Ledger.Ada                             as Ada
import qualified Ledger.Value                           as Value
import Ledger.Constraints        qualified as           Constraints
import Ledger.Tx                                        (ChainIndexTxOut (..))
import Ledger.Typed.Scripts      qualified as           Scripts
--Normal Haskell
import Control.Monad (void)
import Data.ByteString.Char8     qualified as           C
import Data.Text                                        (Text)
import Data.Void
import Data.Map                                         (Map)
import Data.Map                                         qualified as Map
import Data.Maybe                                       (catMaybes)
import Prelude                                          (Semigroup (..))
import Prelude                   qualified as           Haskell
import Text.Printf                                      (printf)

--Plutus Playground related
import Playground.Contract
--Emulator Trace simulation
import qualified Plutus.Trace                           as Trace
import           Plutus.Trace.Emulator     as Emulator
import           Wallet.Emulator.Wallet

--ON-CHAIN

newtype ProjectId = PID BuiltinByteString
    deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''ProjectId

data HourControl = HC {
                        hours     :: Integer
                      , worker    :: PaymentPubKeyHash
                      , projectId :: ProjectId
                      }

PlutusTx.makeIsDataIndexed ''HourControl [('HC,0)]

--makeIsDataIndexed ''ProjectId [('PID,0)]
--unsafeMakeIsData ''ProjectId

data Hours
instance Scripts.ValidatorTypes Hours where
    type instance RedeemerType Game = HourControl
    type instance DatumType Game = HourControl

{-# INLINABLE hourControl #-}
hourControl :: HourControl -> HourControl -> ScriptContext -> Bool
hourControl datum redeemer sContext = traceIfFalse "Not the right project!" verifyProjecId  &&
                                      traceIfFalse "Not the right worker!"  verifyWorker
    where
        info :: TxInfo
        info = scriptContextTxInfo sContext

        verifyProjectId :: Bool
        verifyProjectId = -- Read the datum project comparte to redeemer projectId and the output on sContext datum projectId

        verifyWorker :: Bool
        veriftWorker = -- Read the datum worker comparte to redeemer worker and the output on sContext datum projectId

-- Pending condition: Verify the caller's wallet for the right NFT


-- gameInstance :: Scripts.TypedValidator Game
-- gameInstance = Scripts.mkTypedValidator @Game
--     $$(PlutusTx.compile [|| validateGuess ||])
--     $$(PlutusTx.compile [|| wrap ||]) where
--         wrap = Scripts.wrapValidator @HashedString @ClearString

-- gameValidator :: Validator
-- gameValidator = Scripts.validatorScript gameInstance

-- gameAddress :: Address
-- gameAddress = Ledger.scriptAddress gameValidator

-- hashString :: Haskell.String -> HashedString
-- hashString = HS . sha2_256 . toBuiltin . C.pack

-- clearString :: Haskell.String -> ClearString
-- clearString = CS . toBuiltin . C.pack

-- -- OFF-CHAIN
-- data BlindParams = BP 
--                    { theSecret :: Haskell.String
--                    , value     :: Value
--                    } 
--                    deriving stock (Haskell.Eq, Haskell.Show, Generic)
--                    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

-- data GuessParams = GP 
--                    { guessSecret :: Haskell.String }
--                    deriving stock (Haskell.Eq, Haskell.Show, Generic)
--                    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

-- type GameSchema = 
--          Endpoint "theBlind" BlindParams
--      .\/ Endpoint "theGuess" GuessParams

-- theBlind :: BlindParams -> Contract w GameSchema Text ()
-- theBlind (BP secret blind) =  do
--                         let tx         = Constraints.mustPayToTheScript (hashString secret) blind
--                         ledgerTx <- submitTxConstraints gameInstance tx
--                         void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
--                         logInfo @Haskell.String $ "Pay " <> Haskell.show blind <> " to the script"    
--                         logInfo @Haskell.String $ printf "Put secret %s" secret 

-- theGuess :: GuessParams -> Contract w GameSchema Text ()
-- theGuess (GP theguess) = do
--           utxos <- utxosAt gameAddress 
--           case Map.toList utxos of
--                   []             -> logInfo @Haskell.String $ printf "No UTxOs on the Contract!"
--                   (oref,a):utxos -> do
--                                     let redeemer = clearString theguess
--                                     let lookups = Constraints.unspentOutputs (Map.fromList [(oref,a)]) <>
--                                                   Constraints.otherScript gameValidator 
--                                     let tx = Constraints.mustSpendScriptOutput oref (Ledger.Redeemer $ PlutusTx.toBuiltinData redeemer) 
--                                     ledgerTx <- submitTxConstraintsWith @Void lookups tx
--                                     void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
--                                     logInfo @Haskell.String $ printf "Guesses correctly: %s" (Haskell.show theguess)  

-- endpoints :: Contract () GameSchema Text ()
-- endpoints = awaitPromise (theBlind' `select` theGuess') >> endpoints
--    where
--      theBlind' = endpoint @"theBlind" theBlind
--      theGuess' = endpoint @"theGuess" theGuess

-- mkSchemaDefinitions ''GameSchema
-- mkKnownCurrencies []

-- --SIMULATION

-- test :: IO ()
-- test = runEmulatorTraceIO $ do
--     h1 <- activateContractWallet (knownWallet 1) endpoints
--     h2 <- activateContractWallet (knownWallet 2) endpoints
--     callEndpoint @"theBlind" h1 $ BP 
--                    { theSecret = "TheSecret"
--                    , value     = Ada.lovelaceValueOf 10000000
--                    }
--     void $ Emulator.waitNSlots 10
--     callEndpoint @"theGuess" h2 $ GP 
--                    { guessSecret = "TheSecret" }
--     void $ Emulator.waitNSlots 10