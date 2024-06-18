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
    registrationValidatorAddress,
    mkRegistrationScriptSerialisedScript,
    mkRegistrationScriptScriptCBOREncoded,
) where

import Data.Either (Either (Left, Right))
import Data.Function (flip, ($), (.))
import Data.List ((++))
import ENOPNFT.MonetaryPolicy qualified as ENNOPNFT (currencySymbol, mkMonetaryPolicy)
import ENOPNFT.OnChainMonetaryPolicy (MonetaryPolicySettings (..))

import OnChainRegistrationValidator (
    RegistrationValidatorSettings (..),
    RegistrationAction,
    RegistrationDatum,
    mkUntypedValidatorFunction,
    mkValidatorFunction,
 )
import Prelude (Show (..), error)

import Adapter.Plutus.OffChain (validatorToTypedValidator)
import Data.ByteString (ByteString)
import Data.ByteString.Short (fromShort)
import Plutus.Script.Utils.Scripts qualified as Script (mkValidatorScript)
import Plutus.Script.Utils.Typed qualified as Script hiding (validatorHash)
import Plutus.Script.Utils.V3.Scripts qualified as Script
import PlutusLedgerApi.V3 (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V3 qualified as Script
import PlutusTx
    ( BuiltinData,
      CompiledCode,
      unsafeApplyCode,
      applyCode,
      liftCodeDef,
      compile )

data RegistrationScript

instance Script.ValidatorTypes RegistrationScript where
    type RedeemerType RegistrationScript = RegistrationAction
    type DatumType RegistrationScript = RegistrationDatum

mkVersionedValidator :: RegistrationValidatorSettings -> Script.Versioned Script.Validator
mkVersionedValidator = flip Script.Versioned Script.PlutusV3 . mkValidator

mkValidator :: RegistrationValidatorSettings -> Script.Validator
mkValidator settings =
    case $$(PlutusTx.compile [||\s -> Script.mkUntypedValidator (mkValidatorFunction s)||]) `PlutusTx.applyCode` PlutusTx.liftCodeDef settings of
        Left s -> error $ "Can't apply parameters in validator: " ++ show s
        Right code -> Script.mkValidatorScript code

typedRegistrationValidator :: RegistrationValidatorSettings -> Script.TypedValidator RegistrationScript
typedRegistrationValidator = validatorToTypedValidator . mkValidator

getRegistrationValidatorHash :: RegistrationValidatorSettings -> Script.ValidatorHash
getRegistrationValidatorHash settings = Script.validatorHash (mkValidator settings)

registrationValidatorAddress :: RegistrationValidatorSettings -> Script.Address
registrationValidatorAddress settings = Script.validatorAddress (typedRegistrationValidator settings)

associatedENOPNFTMonetaryPolicySettings :: RegistrationValidatorSettings -> MonetaryPolicySettings
associatedENOPNFTMonetaryPolicySettings settings@RegistrationValidatorSettings{..} =
    MonetaryPolicySettings
        { ennftCurrencySymbol = ennftCurrencySymbol
        , registrationValidatorHash = getRegistrationValidatorHash settings
        }

associatedENOPNFTMonetaryPolicy :: RegistrationValidatorSettings -> Script.Versioned Script.MintingPolicy
associatedENOPNFTMonetaryPolicy = ENNOPNFT.mkMonetaryPolicy . associatedENOPNFTMonetaryPolicySettings

associatedENOPNFTCurrencySymbol :: RegistrationValidatorSettings -> Script.CurrencySymbol
associatedENOPNFTCurrencySymbol settings = ENNOPNFT.currencySymbol (associatedENOPNFTMonetaryPolicySettings settings)

appliedRegistrationScript ::
    MonetaryPolicySettings ->
    CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
appliedRegistrationScript MonetaryPolicySettings{..} =
    $$(compile [||mkUntypedValidatorFunction||])
        `unsafeApplyCode` liftCodeDef (RegistrationValidatorSettings ennftCurrencySymbol)

mkRegistrationScriptSerialisedScript :: MonetaryPolicySettings -> SerialisedScript
mkRegistrationScriptSerialisedScript = serialiseCompiledCode . appliedRegistrationScript

mkRegistrationScriptScriptCBOREncoded :: MonetaryPolicySettings -> ByteString
mkRegistrationScriptScriptCBOREncoded = fromShort . mkRegistrationScriptSerialisedScript
