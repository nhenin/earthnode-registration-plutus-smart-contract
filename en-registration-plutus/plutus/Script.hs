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

module Script (
    script,
    versionedScript,
    typedScript,
    scriptHash,
    associatedENOPNFTMonetaryPolicySettings,
    associatedENOPNFTMonetaryPolicy,
    associatedENOPNFTCurrencySymbol,
) where

import Data.Either (Either (Left, Right))
import Data.Function (flip, ($), (.))
import Data.List ((++))
import ENOPNFT.MonetaryPolicy qualified as ENNOPNFT (currencySymbol, monetaryPolicy)
import ENOPNFT.Validator (MonetaryPolicySettings (..))

import PlutusTx qualified
import Validator (ENNFTCurrencySymbol (..), RegistrationAction, RegistrationDatum, validator)
import Prelude (Show (..), error)

import Adapter.Plutus
import Plutus.Script.Utils.Scripts qualified as Script (mkValidatorScript)
import Plutus.Script.Utils.Typed qualified as Script hiding (validatorHash)
import Plutus.Script.Utils.V3.Scripts qualified as Script
import PlutusLedgerApi.V3 qualified as Script

data RegistrationScript

instance Script.ValidatorTypes RegistrationScript where
    type RedeemerType RegistrationScript = RegistrationAction
    type DatumType RegistrationScript = RegistrationDatum

versionedScript :: ENNFTCurrencySymbol -> Script.Versioned Script.Validator
versionedScript = flip Script.Versioned Script.PlutusV3 . script

script :: ENNFTCurrencySymbol -> Script.Validator
script settings =
    case $$(PlutusTx.compile [||\s -> Script.mkUntypedValidator (validator s)||]) `PlutusTx.applyCode` PlutusTx.liftCodeDef settings of
        Left s -> error $ "Can't apply parameters in validator: " ++ show s
        Right code -> Script.mkValidatorScript code

typedScript :: ENNFTCurrencySymbol -> Script.TypedValidator RegistrationScript
typedScript = validatorToTypedValidator . script

scriptHash :: ENNFTCurrencySymbol -> Script.ValidatorHash
scriptHash settings = Script.validatorHash (script settings)

associatedENOPNFTMonetaryPolicySettings :: ENNFTCurrencySymbol -> MonetaryPolicySettings
associatedENOPNFTMonetaryPolicySettings settings@ENNFTCurrencySymbol{..} =
    MonetaryPolicySettings
        { ennftCurrencySymbol = ennftCurrencySymbol
        , registrationValidatorHash = scriptHash settings
        }

associatedENOPNFTMonetaryPolicy :: ENNFTCurrencySymbol -> Script.Versioned Script.MintingPolicy
associatedENOPNFTMonetaryPolicy = ENNOPNFT.monetaryPolicy . associatedENOPNFTMonetaryPolicySettings

associatedENOPNFTCurrencySymbol :: ENNFTCurrencySymbol -> Script.CurrencySymbol
associatedENOPNFTCurrencySymbol settings = ENNOPNFT.currencySymbol (associatedENOPNFTMonetaryPolicySettings settings)
