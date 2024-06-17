{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module ENOPNFT.MonetaryPolicy (
    mkMonetaryPolicy,
    mkMonetaryPolicySerialisedScript,
    mkMonetaryPolicyScriptCBOREncoded,
    currencySymbol,
    MonetaryPolicySettings (..),
    Action (..),
) where

import qualified Plutus.Script.Utils.Scripts as Script
import qualified Plutus.Script.Utils.Typed as Script
import PlutusTx (applyCode, compile, liftCodeDef)

import Data.Either (Either (Left, Right))

import Data.ByteString (ByteString)
import Data.ByteString.Short (fromShort)
import ENOPNFT.OnChainMonetaryPolicy (Action (..), MonetaryPolicySettings (..), mkMoneterayPolicyFunction, mkUntypedMintingPolicyFunction)
import PlutusLedgerApi.V3
import qualified PlutusLedgerApi.V3 as Script
import PlutusTx.Code
import qualified Prelude as Haskell

mkMonetaryPolicy :: MonetaryPolicySettings -> Script.Versioned Script.MintingPolicy
mkMonetaryPolicy settings =
    case $$(PlutusTx.compile [||\s -> Script.mkUntypedMintingPolicy (mkMoneterayPolicyFunction s)||]) `PlutusTx.applyCode` PlutusTx.liftCodeDef settings of
        Left s -> Haskell.error Haskell.$ "Can't apply parameters in validator: " Haskell.++ Haskell.show s
        Right code -> Haskell.flip Script.Versioned Script.PlutusV3 Haskell.. Script.mkMintingPolicyScript Haskell.$ code

currencySymbol :: MonetaryPolicySettings -> Script.CurrencySymbol
currencySymbol settings = Script.scriptCurrencySymbol (mkMonetaryPolicy settings)

appliedMonetaryPolicy ::
    MonetaryPolicySettings ->
    CompiledCode (BuiltinData -> BuiltinData -> ())
appliedMonetaryPolicy params =
    $$(compile [||mkUntypedMintingPolicyFunction||])
        `unsafeApplyCode` liftCodeDef params

mkMonetaryPolicySerialisedScript :: MonetaryPolicySettings -> SerialisedScript
mkMonetaryPolicySerialisedScript = serialiseCompiledCode Haskell.. appliedMonetaryPolicy

mkMonetaryPolicyScriptCBOREncoded :: MonetaryPolicySettings -> ByteString
mkMonetaryPolicyScriptCBOREncoded = fromShort Haskell.. mkMonetaryPolicySerialisedScript
