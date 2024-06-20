{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- Options
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

{-# HLINT ignore "Use second" #-}

module Aya.Registration.Core.Property.Violation (
    NFTPropertyViolationMsg (..),
    mkENOPNFTPropertyViolationMsg,
    mkENNFTPropertyViolationMsg,
    prop_1_1_0_enopAndEnNFTWithDifferentName,
    prop_1_1_2_MultipleENOPTokenNamesForSameCurrencySymbol,
    prop_2_1_1_SignerIsNotUnique,
    prop_3_0_InvalidSignature,
    prop_3_1_RegistrationScriptNotFound,
    prop_3_2_NoRegistrationDatum,
    prop_3_3_OnlyHashRegistration,
    prop_3_4_MoreThanOneRegistrationOutput,
    prop_3_5_DatumDeserializationFailed,
) where

import PlutusTx.Prelude (BuiltinString)

{-# INLINEABLE prop_1_1_0_enopAndEnNFTWithDifferentName #-}
{-# INLINEABLE prop_1_1_2_MultipleENOPTokenNamesForSameCurrencySymbol #-}
{-# INLINEABLE prop_2_1_1_SignerIsNotUnique #-}
{-# INLINEABLE prop_3_0_InvalidSignature #-}
{-# INLINEABLE prop_3_1_RegistrationScriptNotFound #-}
{-# INLINEABLE prop_3_2_NoRegistrationDatum #-}
{-# INLINEABLE prop_3_3_OnlyHashRegistration #-}
{-# INLINEABLE prop_3_4_MoreThanOneRegistrationOutput #-}
{-# INLINEABLE prop_3_5_DatumDeserializationFailed #-}
prop_1_1_0_enopAndEnNFTWithDifferentName
    , prop_1_1_2_MultipleENOPTokenNamesForSameCurrencySymbol
    , prop_2_1_1_SignerIsNotUnique
    , prop_3_0_InvalidSignature
    , prop_3_1_RegistrationScriptNotFound
    , prop_3_2_NoRegistrationDatum
    , prop_3_3_OnlyHashRegistration
    , prop_3_4_MoreThanOneRegistrationOutput
    , prop_3_5_DatumDeserializationFailed ::
        BuiltinString
prop_1_1_2_MultipleENOPTokenNamesForSameCurrencySymbol = "1.1.2"
prop_1_1_0_enopAndEnNFTWithDifferentName = "1.1.0"
prop_2_1_1_SignerIsNotUnique = "2.1.1"
prop_3_0_InvalidSignature = "3.0"
prop_3_1_RegistrationScriptNotFound = "3.1"
prop_3_2_NoRegistrationDatum = "3.2"
prop_3_3_OnlyHashRegistration = "3.3"
prop_3_4_MoreThanOneRegistrationOutput = "3.4"
prop_3_5_DatumDeserializationFailed = "3.5"

data NFTPropertyViolationMsg = NFTPropertyViolationMsg
    { whenNoNFT :: BuiltinString
    , whenQuantityMoreThanOne :: BuiltinString
    , whenMultipleTokenNamesForSameCurrencySymbol :: BuiltinString
    }

{-# INLINEABLE mkENOPNFTPropertyViolationMsg #-}
mkENOPNFTPropertyViolationMsg :: NFTPropertyViolationMsg
mkENOPNFTPropertyViolationMsg =
    NFTPropertyViolationMsg
        { whenNoNFT = "1.0.1"
        , whenQuantityMoreThanOne = "1.0.2"
        , whenMultipleTokenNamesForSameCurrencySymbol = "1.1.1"
        }

{-# INLINEABLE mkENNFTPropertyViolationMsg #-}
mkENNFTPropertyViolationMsg :: NFTPropertyViolationMsg
mkENNFTPropertyViolationMsg =
    NFTPropertyViolationMsg
        { whenNoNFT = "1.0.3"
        , whenQuantityMoreThanOne = "1.0.4"
        , whenMultipleTokenNamesForSameCurrencySymbol = "1.1.2"
        }
