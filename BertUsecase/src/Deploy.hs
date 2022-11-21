{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Deploy
    ( writeJSON
    , writeValidator
    , writeUnit
    , writeDatum
    , writeRedeemer
    , writeBadRedeemer
    , writeGGValidator
    ) where

import           Cardano.Api
import           Cardano.Api.Shelley   (PlutusScript (..))
import           Codec.Serialise       (serialise)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           PlutusTx              (Data (..))
import qualified PlutusTx
import qualified Ledger

import          GuessingGameO

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

writeUnit :: IO ()
writeUnit = writeJSON "testnet/unit.json" ()

writeDatum :: IO ()
writeDatum = writeJSON "testnet/datum.json" (hashString "TheSecret")

writeRedeemer :: IO ()
writeRedeemer = writeJSON "testnet/goodRedeemer.json" (clearString "TheSecret" )

writeBadRedeemer :: IO ()
writeBadRedeemer = writeJSON "testnet/badRedeemer.json" (clearString "TheWrongSecret" )

writeGGValidator :: IO (Either (FileError ()) ())
writeGGValidator = writeValidator "testnet/guessingGame.plutus" $ gameValidator
