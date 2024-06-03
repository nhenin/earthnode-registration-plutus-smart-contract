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
    monetaryPolicy,
    currencySymbol,
    MonetaryPolicySettings (..),
    Action (..),
) where

import qualified Plutus.Script.Utils.Scripts as Script
import qualified Plutus.Script.Utils.Typed as Script
import PlutusTx (applyCode, compile, liftCodeDef)

import Data.Either (Either (Left, Right))

import ENOPNFT.Validator (Action (..), MonetaryPolicySettings (..), mkValidator)
import qualified PlutusLedgerApi.V3 as Script
import qualified Prelude as Haskell

monetaryPolicy :: MonetaryPolicySettings -> Script.Versioned Script.MintingPolicy
monetaryPolicy settings =
    case $$(PlutusTx.compile [||\s -> Script.mkUntypedMintingPolicy (mkValidator s)||]) `PlutusTx.applyCode` PlutusTx.liftCodeDef settings of
        Left s -> Haskell.error Haskell.$ "Can't apply parameters in validator: " Haskell.++ Haskell.show s
        Right code -> Haskell.flip Script.Versioned Script.PlutusV3 Haskell.. Script.mkMintingPolicyScript Haskell.$ code

currencySymbol :: MonetaryPolicySettings -> Script.CurrencySymbol
currencySymbol settings = Script.scriptCurrencySymbol (monetaryPolicy settings)
