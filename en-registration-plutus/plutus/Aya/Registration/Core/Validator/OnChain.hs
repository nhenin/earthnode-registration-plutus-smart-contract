{-
  Author   : Torben Poguntke
  Company  : World Mobile Group
  Copyright: 2023
  Version  : v2.0
-}
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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Aya.Registration.Core.Validator.OnChain (
    mkUntypedValidatorFunction,
    mkValidatorFunction,
    RegistrationValidatorSettings (..),
    RegistrationDatum (..),
    RegistrationAction (..),
    checkRegistrationSignature,
    mkHashedRegistrationMessage,
    getRegistrationDatumAndENNFTTokenNameOutput,
    getENOPNFTTokenName,
) where

import GHC.Generics (Generic)

import Plutus.Script.Utils.Value (
    valueOf,
 )
import PlutusLedgerApi.V3
import PlutusTx qualified
import PlutusTx.Prelude as Plutus.Prelude
import Prelude qualified

import PlutusTx.Builtins qualified as Cryptography

import Adapter.Plutus.OnChain
import Aya.Registration.Core.Property.Violation
import PlutusTx.Applicative ()

import Plutus.Script.Utils.V3.Contexts (valueSpent)

newtype RegistrationValidatorSettings = RegistrationValidatorSettings
    { ennftCurrencySymbol :: CurrencySymbol
    }
    deriving (Prelude.Show, Generic, Ord)

instance Eq RegistrationValidatorSettings where
    {-# INLINEABLE (==) #-}
    RegistrationValidatorSettings x == RegistrationValidatorSettings y = x == y

PlutusTx.unstableMakeIsData ''RegistrationValidatorSettings
PlutusTx.makeLift ''RegistrationValidatorSettings

data RegistrationDatum = RegistrationDatum
    { ayaValidatorPublicKey :: BuiltinByteString
    -- ^ Substrate Public Keys of the Aya Validator
    , signature :: BuiltinByteString
    -- ^ Signature of the datum. All datum fields below concatenated and signed by the substrateAVPublicKey
    , ennftTokenName :: TokenName
    -- ^ Unique ENNFT name, "used" for this registration
    , cardanoRewardPubKey :: PubKeyHash
    -- ^ Operator's wallet where rewards will be delivered after participating in a block production in Aya
    , commission :: Integer
    -- ^ Commission in percent shared with staking delegators.
    , enopNFTCurrencySymbol :: CurrencySymbol -- We cannot store the EnOpNft CurrencySymbol in the parameter because we get a cyclic dependency
    }
    deriving (Prelude.Show, Generic, Ord)

instance Eq RegistrationDatum where
    {-# INLINEABLE (==) #-}
    x == y =
        ayaValidatorPublicKey x
            == ayaValidatorPublicKey y
            && ennftTokenName x
            == ennftTokenName y
            && cardanoRewardPubKey x
            == cardanoRewardPubKey y
            && commission x
            == commission y
            && enopNFTCurrencySymbol x
            == enopNFTCurrencySymbol y
            && signature x
            == signature y

PlutusTx.unstableMakeIsData ''RegistrationDatum
PlutusTx.makeLift ''RegistrationDatum

{- | The actions that can be performed by the operator
| N.B : The action Register is enforced by the ENOPNFT NFT minting policy
-}
data RegistrationAction
    = -- | Unregister the operator
      Unregister
    | -- | Update the registration information
      UpdateRegistrationDetails
    deriving (Prelude.Show, Generic)

instance Eq RegistrationAction where
    {-# INLINEABLE (==) #-}
    Unregister == Unregister = True
    UpdateRegistrationDetails == UpdateRegistrationDetails = True
    _ == _ = False

PlutusTx.makeIsDataIndexed ''RegistrationAction [('UpdateRegistrationDetails, 0), ('Unregister, 1)]
PlutusTx.makeLift ''RegistrationAction

{-# INLINEABLE mkValidatorFunction #-}
mkValidatorFunction :: RegistrationValidatorSettings -> RegistrationDatum -> RegistrationAction -> ScriptContext -> Bool
mkValidatorFunction registrationValidatorSettings _ UpdateRegistrationDetails ctx = canUpdateRegistration registrationValidatorSettings ctx
mkValidatorFunction registrationValidatorSettings datum Unregister ctx = canUnregister registrationValidatorSettings datum ctx

{-# INLINEABLE mkUntypedValidatorFunction #-}
mkUntypedValidatorFunction :: RegistrationValidatorSettings -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkUntypedValidatorFunction s d r c =
    check
        ( mkValidatorFunction
            s
            (unsafeFromBuiltinData d)
            (unsafeFromBuiltinData r)
            (unsafeFromBuiltinData c)
        )

{-# INLINEABLE canUpdateRegistration #-}
canUpdateRegistration :: RegistrationValidatorSettings -> ScriptContext -> Bool
canUpdateRegistration RegistrationValidatorSettings{..} ctx =
    let
        (datumInput, ennftTokenNameInput) = getRegistrationDatumAndENNFTTokenNameInput ennftCurrencySymbol ctx
        (datumOutput, ennftTokenNameOutput) = getRegistrationDatumAndENNFTTokenNameOutput (ownHash ctx) ennftCurrencySymbol (scriptContextTxInfo ctx)
        enopNFTNameOutput = tokenNameGivenToUniqueAndOnlySigner (enopNFTCurrencySymbol datumInput) (scriptContextTxInfo ctx)
        enopNFTNameInput = tokenNameSpentByUniqueSigner (enopNFTCurrencySymbol datumInput) (scriptContextTxInfo ctx)
     in
        checkRegistrationSignature datumInput
            && propertyViolationIfFalse "Datum to spent is invalid" (checkRegistrationSignature datumOutput)
            && propertyViolationIfFalse "Changing the ennft token name in the datum is not allowed" (ennftTokenNameInput == ennftTokenNameOutput)
            && propertyViolationIfFalse "Changing the ennft token name in the datum is not allowed" (enopNFTNameInput == enopNFTNameOutput)
            && ennftTokenName datumInput
            == ennftTokenName datumOutput
            && enopNFTNameOutput
            == ennftTokenNameOutput
            && enopNFTNameOutput
            == ennftTokenName datumOutput
            && enopNFTCurrencySymbol datumInput
            == enopNFTCurrencySymbol datumOutput

-- {-# INLINABLE checkDatumSig #-}
-- Check the signature of the registration datum
checkRegistrationSignature :: RegistrationDatum -> Bool
checkRegistrationSignature RegistrationDatum{..} =
    Cryptography.verifyEd25519Signature
        ayaValidatorPublicKey
        (mkHashedRegistrationMessage ennftTokenName cardanoRewardPubKey commission enopNFTCurrencySymbol)
        signature

{-# INLINEABLE mkHashedRegistrationMessage #-}
mkHashedRegistrationMessage :: TokenName -> PubKeyHash -> Integer -> CurrencySymbol -> BuiltinByteString
mkHashedRegistrationMessage ennftTokenName cardanoRewardPubKey commission enopNFTCurrencySymbol =
    Cryptography.blake2b_256
        . appendByteString (unTokenName ennftTokenName)
        . appendByteString (getPubKeyHash cardanoRewardPubKey)
        . consByteString commission
        $ unCurrencySymbol enopNFTCurrencySymbol

{-# INLINEABLE getRegistrationDatumAndENNFTTokenNameOutput #-}
getRegistrationDatumAndENNFTTokenNameOutput :: ScriptHash -> CurrencySymbol -> TxInfo -> (RegistrationDatum, TokenName)
getRegistrationDatumAndENNFTTokenNameOutput registrationValidatorHash ennftCurrencySymbol =
    \case
        [] -> propertyViolation prop_3_1_RegistrationScriptNotFound
        [(OutputDatum (Datum scriptDatum), scriptValue)] -> (deserializeDatum scriptDatum, getENNFTTokenName ennftCurrencySymbol scriptValue)
        [(NoOutputDatum, _)] -> propertyViolation prop_3_2_NoRegistrationDatum
        [(OutputDatumHash _, _)] -> propertyViolation prop_3_3_OnlyHashRegistration
        _ -> propertyViolation prop_3_4_MoreThanOneRegistrationOutput
        . scriptOutputsAt registrationValidatorHash

{-# INLINEABLE getRegistrationDatumAndENNFTTokenNameInput #-}
getRegistrationDatumAndENNFTTokenNameInput :: CurrencySymbol -> ScriptContext -> (RegistrationDatum, TokenName)
getRegistrationDatumAndENNFTTokenNameInput ennftCurrencySymbol =
    \case
        (OutputDatum (Datum scriptDatum), scriptValue) -> (deserializeDatum scriptDatum, getENNFTTokenName ennftCurrencySymbol scriptValue)
        (NoOutputDatum, _) -> propertyViolation prop_3_2_NoRegistrationDatum
        (OutputDatumHash _, _) -> propertyViolation prop_3_3_OnlyHashRegistration
        . ( \case
                TxOut
                    { txOutAddress = Address (ScriptCredential _) _
                    , txOutValue = v
                    , txOutDatum = d
                    , txOutReferenceScript = Nothing
                    } -> (d, v)
                _ -> propertyViolation "Invalid Registration Input"
          )
        . txInInfoResolved
        . fromMaybe' (propertyViolation "No Registration Input Found")
        . findOwnInput

{-# INLINEABLE deserializeDatum #-}
deserializeDatum :: BuiltinData -> RegistrationDatum
deserializeDatum =
    fromMaybe' (propertyViolation prop_3_5_DatumDeserializationFailed)
        . fromBuiltinData

{-# INLINEABLE getENOPNFTTokenName #-}
getENOPNFTTokenName :: CurrencySymbol -> TxInfo -> TokenName
getENOPNFTTokenName enopNFTCurrencySymbol =
    getNFTTokenName
        mkENOPNFTPropertyViolationMsg
        enopNFTCurrencySymbol
        . txInfoMint

{-# INLINEABLE getENNFTTokenName #-}
getENNFTTokenName :: CurrencySymbol -> Value -> TokenName
getENNFTTokenName = getNFTTokenName mkENNFTPropertyViolationMsg

---- Code to be refined below

{-# INLINEABLE canUnregister #-}
canUnregister :: RegistrationValidatorSettings -> RegistrationDatum -> ScriptContext -> Bool
canUnregister RegistrationValidatorSettings{..} RegistrationDatum{..} ctx
    -- No UTxO's to the script are allowed
    | noScriptOutputs $ txInfoOutputs info
    , -- the ENOPNFT must be burnt in this transaction
      isEnOPNftBurnt
    , -- The ENNFT is spent in this transaction
      valueOf (valueSpent info) ennftCurrencySymbol ennftTokenName == 1 =
        True
    | otherwise = False
  where
    info = scriptContextTxInfo ctx
    -- make sure the EnOpNFT is burnt
    isEnOPNftBurnt :: Bool
    isEnOPNftBurnt = valueOf (txInfoMint info) enopNFTCurrencySymbol ennftTokenName == -1

    noScriptOutputs :: [TxOut] -> Bool
    noScriptOutputs [] = True
    noScriptOutputs (h : t) =
        let
            checkInput :: TxOut -> Bool
            checkInput TxOut{txOutAddress = Address (ScriptCredential _) _} = False
            checkInput _ = True
         in
            checkInput h && noScriptOutputs t
