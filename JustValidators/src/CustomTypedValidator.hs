{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}

module CustomTypedValidator where

--Plutus On-Chain related
import           PlutusTx                   (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins          as Builtins
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
--Plutus and Cardano ledger types, functions and typeclasses
import           Ledger                     hiding (singleton)
import           Ledger.Constraints         as Constraints
import qualified Ledger.Typed.Scripts       as Scripts  -- Plutus.Script.Utils.V1.Typed.Scripts  // Plutus.V1.Ledger.Api
import           Ledger.Ada                 as Ada
--Plutus Contract Monad required for off-chain code tx construction
import           Plutus.Contract
--Plutus Playground related 
import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH       (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types    (KnownCurrency (..))
--"Normal" Haskell related
import           Prelude             (IO, Semigroup (..), String)
import           Text.Printf         (printf)
import           Control.Monad       hiding (fmap)
import           Data.Map            as Map
import           Data.Text           (Text)
import           Data.Void           (Void)
import           Data.Aeson          (ToJSON, FromJSON)
import           GHC.Generics        (Generic)  


{-# OPTIONS_GHC -fno-warn-unused-imports #-}

newtype MyWonderfullRedeemer = MWR Integer
newtype  MyWonderfullDatum = MWD Integer

--PlutusTx.unstableMakeIsData ''MyWonderfullRedeemer
--PlutusTx.unstableMakeIsData ''MyWonderfullDatum

PlutusTx.makeIsDataIndexed ''MyWonderfullRedeemer [('MWR,0)]
PlutusTx.makeIsDataIndexed ''MyWonderfullDatum [('MWD,0)]

{-# INLINABLE customTypedValidator #-}
customTypedValidator :: MyWonderfullDatum -> MyWonderfullRedeemer -> ScriptContext -> Bool
customTypedValidator (MWD datum) (MWR redeemer) sContext = traceIfFalse "Redeemer not equal the datum" (redeemer == datum)

data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed = MyWonderfullDatum
    type instance RedeemerType Typed = MyWonderfullRedeemer

ctvalidator :: Scripts.TypedValidator Typed
ctvalidator = Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [|| customTypedValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
 where
    wrap = Scripts.wrapValidator @MyWonderfullDatum @MyWonderfullRedeemer

validator :: Validator
validator = Scripts.validatorScript ctvalidator   -- Get the untyped validator script of the typeValidator PlutusCore

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator


-- OFF-CHAIN RELATED CODE

data GiveParams = GP {gpAmount :: Integer
                    , gpSecret :: Integer
                    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type GiftSchema =
            Endpoint "give" GiveParams 
        .\/ Endpoint "grab" Integer

give :: AsContractError e => GiveParams -> Contract w s e ()
give (GP amount secret) = do
    let tx = mustPayToTheScript (MWD secret) $ Ada.lovelaceValueOf amount   
    ledgerTx <- submitTxConstraints ctvalidator tx                                                   --This line submit the Tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                                --This line waits for confirmation
    logInfo @String $ printf "We put some value of %d lovelaces in the scriptAddress" amount                                     --This line log info,usable on the PP(Plutus Playground)
 
grab :: forall w s e. AsContractError e => Integer -> Contract w s e ()                                     
grab n = do
    utxos <- utxosAt scrAddress                                                                      -- This will find all UTXOs that sit at the script address
    let orefs   = fst <$> Map.toList utxos                                                           -- This get all the references of the UTXOs
        lookups = Constraints.unspentOutputs utxos      <>                                           -- Tell where to find all the UTXOS
                  Constraints.otherScript validator                                                  -- and inform about the actual validator (the spending tx needs to provide the actual validator)
        tx :: TxConstraints Void Void                                                            
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData (MWR n) | oref <- orefs]  -- Define the TX giving constrains, one for each UTXO sitting on this addrs,
                                                                                                    -- must provide a redeemer (ignored in this case)
    ledgerTx <- submitTxConstraintsWith @Void lookups tx                                             -- Allow the wallet to construct the tx with the necesary information
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx                                                -- Wait for confirmation
    logInfo @String $ "Collected Value"                                                              -- Log information 

endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints                                         -- Asynchronously wait for the endpoints interactions from the wallet
  where                                                                                              -- and recursively wait for the endpoints all over again
    give' = endpoint @"give" give                                                                    -- block until give
    grab' = endpoint @"grab" grab
                                                        

mkSchemaDefinitions ''GiftSchema                                                                     -- Generate the Schema for that
mkKnownCurrencies [] 
