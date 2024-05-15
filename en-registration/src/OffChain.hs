{-
  Author   : Torben Poguntke
  Company  : World Mobile Group
  Copyright: 2023
  Version  : v2.0
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}

module OffChain
where

-- import           Cardano.Api.Shelley            (PlutusScript (..),
--                                                  PlutusScriptV2)
-- import           Codec.Serialise
-- import qualified Data.ByteString.Lazy           as LB
-- import qualified Data.ByteString.Short          as SBS
-- import qualified Ledger
-- import           OnChain                        (vUt)
-- -- import qualified Plutus.Script.Utils.V2.Scripts as Utils
-- -- import qualified Plutus.V1.Ledger.Address       as Address
-- import qualified PlutusLedgerApi.V2             as PlutusV2
-- import qualified PlutusTx
-- import           PlutusTx.Prelude
-- import           Types                          (ScriptParams (..))

-- validator :: BuiltinData -> PlutusV2.Validator
-- validator sp = PlutusV2.mkValidatorScript
--         ($$(PlutusTx.compile [|| vUt ||])
--         `PlutusTx.applyCode` PlutusTx.liftCode sp)

-- script :: BuiltinData -> PlutusV2.Script
-- script = PlutusV2.unValidatorScript . validator

-- scriptHash :: ScriptParams -> PlutusV2.ValidatorHash
-- scriptHash sp = Utils.validatorHash $ validator $ PlutusTx.toBuiltinData sp

-- scriptAddress :: ScriptParams -> Ledger.Address
-- scriptAddress = Address.scriptHashAddress . scriptHash

-- scriptAsCbor :: BuiltinData -> LB.ByteString
-- scriptAsCbor = serialise . script

-- apiScript :: ScriptParams -> PlutusScript PlutusScriptV2
-- apiScript sp = PlutusScriptSerialised $ SBS.toShort $ LB.toStrict $ scriptAsCbor (PlutusTx.toBuiltinData sp)
