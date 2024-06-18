{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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

module Specifications (
    getHumanReadableSpecViolation,
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
    prop_3_5_DatumDeserializationFailed 
) where

import PlutusTx.Prelude

{- | Get the human readable representation on an Error Message OnChain
| N.B : This function should be used only for OffChain Logic, It has a double purpose :
| 1. To provide a human readable error message to the user for OffChain Logic
| 2. To provide direct documentation on the OnChain Logic
-}
{-# INLINEABLE getHumanReadableSpecViolation #-}
getHumanReadableSpecViolation :: BuiltinString -> BuiltinString
getHumanReadableSpecViolation =
    \case
        -- Property 1 : Non Fungible Property Transitivity : EN Token is an NFT => the ENOP Token should be an NFT
        -- Property 1.0 : When Minting ENOP Tokens, Tokens Quantities are verified
        -- ENOP NFT
        "1.0.1" -> "Property. 1.0.1 violation - No ENNOP Minted"
        "1.0.2" -> "Property. 1.0.2 violation - ENNOP's Minted Quantity > 1"
        -- EN NFT
        "1.0.3" -> "Property. 1.0.3 violation - No ENNFT on Registration validator output"
        "1.0.4" -> "Property. 1.0.4 violation - ENNFT's Quantity > 1"
        -- Property 1.1 : When Minting ENOP Tokens, NFTs Token Names & Cardinality equality : There is 1-1 relationship between the ENNFT and the ENOPNFT
        "1.1.0" -> "Property. 1.1.0 violation - ENOPNFT TokenName =/ ENNFT TokenName"
        "1.1.1" -> "Property. 1.1.1 violation - |ENOP NFT| > 1 (Cardinality Violation)"
        "1.1.2" -> "Property. 1.1.2 violation - |EN NFT|   > 1 (Cardinality Violation)"
        -- Property 1.2 : When Burning ENOP Tokens, ENNFT are released from the Script
        -- TODO
        -- Property 2 : Preserving NFTs ownership : ENOP and ENNFT can be swapped only between the operator and the registration smart contract
        -- Property 2.0 : ENOP NFT should be minted only to the operator
        "2.0.0" -> "Property. 2.0.0 violation - ENNOP Minted Not Output to Operator"
        -- Property 2.1 : Only the operator should sign the transaction
        "2.1.0" -> "Property. 2.1.0 violation - No signer found"
        "2.1.1" -> "Property. 2.1.1 violation - signer is not unique"
        -- Property 3 : The registration details (datum) should be verifiable
        "3.0" -> "Property. 3.0 violation - Registration datum has failed datum signature verification"
        "3.1" -> "Property. 3.1 violation - Registration validator output not found"
        "3.2" -> "Property. 3.2 violation - Registration validator output has no datum"
        "3.3" -> "Property. 3.3 violation - Registration validator output has only the hashed datum"
        "3.4" -> "Property. 3.4 violation - More than one Registration validator output is not allowed"
        "3.5" -> "Property. 3.5 violation - Registration Datum deserialization failed"
        _ -> "unknown error code"

{-# INLINEABLE prop_1_1_0_enopAndEnNFTWithDifferentName #-}
{-# INLINABLE prop_1_1_2_MultipleENOPTokenNamesForSameCurrencySymbol #-}
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
