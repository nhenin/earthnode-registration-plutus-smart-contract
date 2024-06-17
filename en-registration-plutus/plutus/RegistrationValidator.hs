{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module RegistrationValidator (
    typedRegistrationValidator,
    associatedENOPNFTMonetaryPolicySettings,
    associatedENOPNFTMonetaryPolicy,
    associatedENOPNFTCurrencySymbol,
    mkRegistrationScriptSerialisedScript,
    mkRegistrationScriptScriptCBOREncoded,
) where

import Data.Either (Either (Left, Right))
import Data.Function (flip, ($), (.))
import Data.List ((++))
import ENOPNFT.MonetaryPolicy qualified as ENNOPNFT (currencySymbol, mkMonetaryPolicy)
import ENOPNFT.OnChainMonetaryPolicy (MonetaryPolicySettings (..))

import OnChainRegistrationValidator (
    ENNFTCurrencySymbol (..),
    RegistrationAction,
    RegistrationDatum,
    mkUntypedValidatorFunction,
    mkValidatorFunction,
 )
import Prelude (Show (..), error)

import Adapter.Plutus (validatorToTypedValidator)
import Data.ByteString (ByteString)
import Data.ByteString.Short (fromShort)
import Plutus.Script.Utils.Scripts qualified as Script (mkValidatorScript)
import Plutus.Script.Utils.Typed qualified as Script hiding (validatorHash)
import Plutus.Script.Utils.V3.Scripts qualified as Script
import PlutusLedgerApi.V3 (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V3 qualified as Script
import PlutusTx

data RegistrationScript

instance Script.ValidatorTypes RegistrationScript where
    type RedeemerType RegistrationScript = RegistrationAction
    type DatumType RegistrationScript = RegistrationDatum

mkVersionedValidator :: ENNFTCurrencySymbol -> Script.Versioned Script.Validator
mkVersionedValidator = flip Script.Versioned Script.PlutusV3 . mkValidator

mkValidator :: ENNFTCurrencySymbol -> Script.Validator
mkValidator settings =
    case $$(PlutusTx.compile [||\s -> Script.mkUntypedValidator (mkValidatorFunction s)||]) `PlutusTx.applyCode` PlutusTx.liftCodeDef settings of
        Left s -> error $ "Can't apply parameters in validator: " ++ show s
        Right code -> Script.mkValidatorScript code

typedRegistrationValidator :: ENNFTCurrencySymbol -> Script.TypedValidator RegistrationScript
typedRegistrationValidator = validatorToTypedValidator . mkValidator

mkRegistrationValidatorHash :: ENNFTCurrencySymbol -> Script.ValidatorHash
mkRegistrationValidatorHash settings = Script.validatorHash (mkValidator settings)

associatedENOPNFTMonetaryPolicySettings :: ENNFTCurrencySymbol -> MonetaryPolicySettings
associatedENOPNFTMonetaryPolicySettings settings@ENNFTCurrencySymbol{..} =
    MonetaryPolicySettings
        { ennftCurrencySymbol = ennftCurrencySymbol
        , registrationValidatorHash = mkRegistrationValidatorHash settings
        }

associatedENOPNFTMonetaryPolicy :: ENNFTCurrencySymbol -> Script.Versioned Script.MintingPolicy
associatedENOPNFTMonetaryPolicy = ENNOPNFT.mkMonetaryPolicy . associatedENOPNFTMonetaryPolicySettings

associatedENOPNFTCurrencySymbol :: ENNFTCurrencySymbol -> Script.CurrencySymbol
associatedENOPNFTCurrencySymbol settings = ENNOPNFT.currencySymbol (associatedENOPNFTMonetaryPolicySettings settings)

appliedRegistrationScript ::
    MonetaryPolicySettings ->
    CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
appliedRegistrationScript MonetaryPolicySettings{..} =
    $$(compile [||mkUntypedValidatorFunction||])
        `unsafeApplyCode` liftCodeDef (ENNFTCurrencySymbol ennftCurrencySymbol)

mkRegistrationScriptSerialisedScript :: MonetaryPolicySettings -> SerialisedScript
mkRegistrationScriptSerialisedScript = serialiseCompiledCode . appliedRegistrationScript

mkRegistrationScriptScriptCBOREncoded :: MonetaryPolicySettings -> ByteString
mkRegistrationScriptScriptCBOREncoded = fromShort . mkRegistrationScriptSerialisedScript
