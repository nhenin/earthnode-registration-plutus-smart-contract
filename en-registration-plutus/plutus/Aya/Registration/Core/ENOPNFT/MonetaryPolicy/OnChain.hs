{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
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
-- Options
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

{-# HLINT ignore "Use second" #-}

module Aya.Registration.Core.ENOPNFT.MonetaryPolicy.OnChain (
    mkMoneterayPolicyFunction,
    mkUntypedMintingPolicyFunction,
    MonetaryPolicySettings (..),
    Action (..),
    validateUnregister,
) where

import PlutusTx
import PlutusTx.Prelude

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Prelude qualified

import Adapter.Plutus.OnChain
import Aya.Registration.Core.Property.Violation
import Aya.Registration.Core.Validator.OnChain (
    checkRegistrationSignature,
    getENOPNFTTokenName,
    getRegistrationDatumAndENNFTTokenNameOutput,
 )
import Plutus.Script.Utils.Scripts (ValidatorHash (..))
import Plutus.Script.Utils.Value (flattenValue, valueOf)
import PlutusLedgerApi.V3 (
    CurrencySymbol,
    ScriptContext (scriptContextTxInfo),
    ScriptHash,
    TokenName,
    TxInInfo (txInInfoResolved),
    TxInfo (txInfoInputs, txInfoMint),
    TxOut (txOutValue),
    Value,
 )
import PlutusLedgerApi.V3.Contexts (ownCurrencySymbol)

data MonetaryPolicySettings = MonetaryPolicySettings
    { ennftCurrencySymbol :: CurrencySymbol
    , registrationValidatorHash :: ScriptHash
    }
    deriving (Prelude.Show, Generic, Ord)

instance Eq MonetaryPolicySettings where
    {-# INLINEABLE (==) #-}
    MonetaryPolicySettings x y == MonetaryPolicySettings x' y' = x == x' && y == y'

instance Eq ValidatorHash where
    {-# INLINEABLE (==) #-}
    ValidatorHash x == ValidatorHash y = x == y

PlutusTx.unstableMakeIsData ''MonetaryPolicySettings
PlutusTx.makeLift ''MonetaryPolicySettings

data Action = Mint | Burn
    deriving (Prelude.Show, Generic, FromJSON, ToJSON, Ord)

instance Eq Action where
    {-# INLINEABLE (==) #-}
    Mint == Mint = True
    Burn == Burn = True
    _ == _ = False

PlutusTx.makeIsDataIndexed ''Action [('Mint, 0), ('Burn, 1)]
PlutusTx.makeLift ''Action

{-# INLINEABLE mkMoneterayPolicyFunction #-}
mkMoneterayPolicyFunction :: MonetaryPolicySettings -> Action -> ScriptContext -> Bool
mkMoneterayPolicyFunction sp Mint ctx = canMintENOPNFT sp (ownCurrencySymbol ctx) (scriptContextTxInfo ctx)
mkMoneterayPolicyFunction _ Burn ctx = validateUnregister (ownCurrencySymbol ctx) (scriptContextTxInfo ctx)

{-# INLINEABLE mkUntypedMintingPolicyFunction #-}
mkUntypedMintingPolicyFunction :: MonetaryPolicySettings -> BuiltinData -> BuiltinData -> ()
mkUntypedMintingPolicyFunction settings r c =
    check
        ( mkMoneterayPolicyFunction
            settings
            (unsafeFromBuiltinData r)
            (unsafeFromBuiltinData c)
        )

{-# INLINEABLE canMintENOPNFT #-}
canMintENOPNFT :: MonetaryPolicySettings -> CurrencySymbol -> TxInfo -> Bool
canMintENOPNFT MonetaryPolicySettings{..} enopNFTCurrencySymbol txInfo =
    ( \(registrationDatum, ennftTokenName) ->
        propertyViolationIfFalse prop_1_1_0_enopAndEnNFTWithDifferentName (ennftTokenName == getENOPNFTTokenName enopNFTCurrencySymbol txInfo)
            && propertyViolationIfFalse prop_3_0_InvalidSignature (checkRegistrationSignature registrationDatum)
            && propertyViolationIfFalse prop_1_1_2_MultipleENOPTokenNamesForSameCurrencySymbol (ennftTokenName == tokenNameGivenToUniqueAndOnlySigner enopNFTCurrencySymbol txInfo)
    )
        . getRegistrationDatumAndENNFTTokenNameOutput registrationValidatorHash ennftCurrencySymbol
        $ txInfo

---- Below needs to be refined

{-# INLINEABLE validateUnregister #-}
validateUnregister :: CurrencySymbol -> TxInfo -> Bool
validateUnregister ocs info
    | traceIfFalse "enOpNftBurnt" $ enOpBurnt ocs (txInfoMint info) info = True
    | otherwise = False

{-- enOpBurnt --}
-- Make sure the Token is burnt
{-# INLINEABLE enOpBurnt #-}
enOpBurnt :: CurrencySymbol -> Value -> TxInfo -> Bool
enOpBurnt cs v info =
    let
        tn = getTokenName' (txInfoInputs info) cs
        burn_amt = case tn of
            Just tn' -> valueOf v cs tn'
            Nothing -> 0
     in
        burn_amt == -1

{-- getTokenName --}
-- We determine the TokenName from the input of the registration smart contract,
-- we know it must be exactly one available which must have the same tokenname as the ENOOPNFT
{-# INLINEABLE getTokenName' #-}
getTokenName' :: [TxInInfo] -> CurrencySymbol -> Maybe TokenName
getTokenName' is cs =
    let
        filter' :: TxInInfo -> [Maybe TokenName]
        filter' i = fn $ flattenValue (txOutValue $ txInInfoResolved i)
          where
            fn [] = []
            fn ((cs', tn', amt') : ls) = if cs == cs' && amt' == 1 then [Just tn'] else fn ls

        os [] = []
        os (x : xs) = filter' x ++ os xs
     in
        case os is of
            [h] -> h
            _ -> traceError "more than one registration input or none"
