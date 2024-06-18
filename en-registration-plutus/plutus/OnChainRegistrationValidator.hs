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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module OnChainRegistrationValidator (
    mkUntypedValidatorFunction,
    mkValidatorFunction,
    checkRegistrationSignature,
    mkHashedRegistrationMessage,
    getRegistrationDatumAndENNFTTokenNameOutput,
    getRegistrationDatumAndENNFTTokenNameInput,
    RegistrationValidatorSettings (..),
    RegistrationDatum (..),
    RegistrationAction (..),
    Quantity (..),
    getENOPNFTTokenName,
    getENNFTTokenName,
    enopNFTNameGivenToUniqueAndOnlyOperator,

) where

import GHC.Generics (Generic)

import Plutus.Script.Utils.Value
import PlutusLedgerApi.V3
import PlutusTx qualified
import PlutusTx.Prelude as Plutus.Prelude
    ( otherwise,
      Bool(..),
      Integer,
      Maybe(..),
      Eq(..),
      Ord((<=)),
      Functor(fmap),
      (&&),
      (.),
      appendByteString,
      consByteString,
      ($),
      maybe,
      check )
import Prelude qualified

import PlutusTx.Builtins qualified as Cryptography

import Plutus.Script.Utils.V3.Contexts (
    findOwnInput,
    ownHash,
    scriptOutputsAt,
    valueSpent,
 )
import Plutus.Script.Utils.V3.Scripts (ValidatorHash)
import Specifications
import Adapter.Plutus.OnChain ( fromMaybe', propertyViolation )
import PlutusTx.AssocMap qualified as Map
import PlutusLedgerApi.V3.Contexts (valuePaidTo)
import PlutusTx.Applicative ()

data Quantity = One | MoreThanOne
    deriving (Prelude.Show)

instance Eq Quantity where
    {-# INLINEABLE (==) #-}
    One == One = True
    MoreThanOne == MoreThanOne = True
    _ == _ = False

PlutusTx.unstableMakeIsData ''Quantity
PlutusTx.makeLift ''Quantity

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

PlutusTx.makeIsDataIndexed ''RegistrationDatum [('RegistrationDatum, 0)]
PlutusTx.makeLift ''RegistrationDatum

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

{- | The actions that can be performed by the operator
| N.B : The action Register is enforced by the ENOPNFT NFT minting policy
-}
data RegistrationAction
    = -- | Unregister the operator
      Unregister
    | -- | Update the registration information
      UpdateRegistrationDetails 
    deriving (Prelude.Show, Generic, Ord)

instance Eq RegistrationAction where
    {-# INLINEABLE (==) #-}
    Unregister == Unregister = True
    UpdateRegistrationDetails  == UpdateRegistrationDetails  = True
    _ == _ = False
    
PlutusTx.makeIsDataIndexed ''RegistrationAction [('UpdateRegistrationDetails, 0), ('Unregister, 1)]
PlutusTx.makeLift ''RegistrationAction


{-# INLINEABLE canUpdateRegistration #-}
canUpdateRegistration :: RegistrationValidatorSettings -> ScriptContext -> Bool
canUpdateRegistration RegistrationValidatorSettings{..} ctx = 
  let 
      (datumInput, ennftTokenNameInput) = getRegistrationDatumAndENNFTTokenNameInput ennftCurrencySymbol ctx
      (datumOutput, ennftTokenNameOutput) = getRegistrationDatumAndENNFTTokenNameOutput (ownHash ctx) ennftCurrencySymbol (scriptContextTxInfo ctx)
    --   enopNFTNameOutput = enopNFTNameGivenToUniqueAndOnlyOperator (enopNFTCurrencySymbol datumInput) (scriptContextTxInfo ctx)
    --   enopNFTNameInput = enopNFTNameSpentByUniqueOperator (enopNFTCurrencySymbol datumInput) (scriptContextTxInfo ctx)
   in
      checkRegistrationSignature datumInput
      && checkRegistrationSignature datumOutput
      && ennftTokenNameInput == ennftTokenNameOutput 
    --   && enopNFTNameInput == enopNFTNameOutput  
      && ennftTokenName datumInput == ennftTokenName datumOutput
    --   && enopNFTNameOutput == ennftTokenNameOutput 
    --   && enopNFTNameOutput == ennftTokenName datumOutput 
      && enopNFTCurrencySymbol datumInput == enopNFTCurrencySymbol datumOutput
      

{-# INLINEABLE getRegistrationDatumAndENNFTTokenNameOutput #-}
getRegistrationDatumAndENNFTTokenNameOutput :: ValidatorHash -> CurrencySymbol -> TxInfo -> (RegistrationDatum, TokenName)
getRegistrationDatumAndENNFTTokenNameOutput registrationValidatorHash ennftCurrencySymbol =
    \case
        [] -> propertyViolation prop_3_1_RegistrationScriptNotFound
        [(OutputDatum (Datum scriptDatum), scriptValue)] ->
            (,getENNFTTokenName ennftCurrencySymbol scriptValue)
            . (\case 
                Nothing -> propertyViolation prop_3_5_DatumDeserializationFailed
                Just x ->  x )
            . fromBuiltinData @RegistrationDatum 
            $ scriptDatum
        [(NoOutputDatum, _)] -> propertyViolation prop_3_2_NoRegistrationDatum
        [(OutputDatumHash _, _)] -> propertyViolation prop_3_3_OnlyHashRegistration
        _ : _ -> propertyViolation prop_3_4_MoreThanOneRegistrationOutput
    . scriptOutputsAt registrationValidatorHash



{-# INLINEABLE getRegistrationDatumAndENNFTTokenNameInput #-}
getRegistrationDatumAndENNFTTokenNameInput ::  CurrencySymbol -> ScriptContext -> (RegistrationDatum, TokenName)
getRegistrationDatumAndENNFTTokenNameInput ennftCurrencySymbol  =
    \case
        (OutputDatum (Datum scriptDatum), scriptValue) ->
            (,getENNFTTokenName ennftCurrencySymbol scriptValue)
            . fromMaybe' (propertyViolation prop_3_5_DatumDeserializationFailed)
            . fromBuiltinData @RegistrationDatum
            $ scriptDatum
        (NoOutputDatum, _)     -> propertyViolation prop_3_2_NoRegistrationDatum
        (OutputDatumHash _, _) -> propertyViolation prop_3_3_OnlyHashRegistration
    . \case
        TxOut
            { txOutAddress = Address (ScriptCredential _) _
            , txOutValue
            , txOutDatum = datum
            , txOutReferenceScript = Nothing
            } -> (datum, txOutValue)
        _ -> propertyViolation "Invalid Registration Input"
    . txInInfoResolved
    . fromMaybe' (propertyViolation "No Registration Input Found")
    . findOwnInput


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

{-# INLINEABLE getNFTTokenName #-}
getNFTTokenName :: NFTPropertyViolationMsg -> CurrencySymbol -> Value -> TokenName
getNFTTokenName nftPropertyViolationMsg@NFTPropertyViolationMsg{..} enopNFTCurrencySymbol =
    \case
        (tn, One) -> tn
        (_, MoreThanOne) -> propertyViolation whenQuantityMoreThanOne
        . getUniqueTokenNameAndQuantity nftPropertyViolationMsg enopNFTCurrencySymbol


{-# INLINEABLE getUniqueTokenNameAndQuantity #-}
getUniqueTokenNameAndQuantity :: NFTPropertyViolationMsg -> CurrencySymbol -> Value -> (TokenName, Quantity)
getUniqueTokenNameAndQuantity NFTPropertyViolationMsg{..} c =
    \case
        [] -> propertyViolation whenNoNFT
        [(tn, quantity)] -> (tn, quantity)
        _ -> propertyViolation whenMultipleTokenNamesForSameCurrencySymbol
        . getTokenNamesAndQuantities c

{-# INLINEABLE getTokenNamesAndQuantities #-}
getTokenNamesAndQuantities :: CurrencySymbol -> Value -> [(TokenName, Quantity)]
getTokenNamesAndQuantities givenCurrencySymbol (Value value) =
    maybe
        []
        ((fmap . fmap) convertToQuantity . Map.toList)
        (Map.lookup givenCurrencySymbol value)

{-# INLINEABLE convertToQuantity #-}
convertToQuantity :: Integer -> Quantity
convertToQuantity x
    | x <= 0 = propertyViolation "Ledger Property - Quantity can't be negative or zero"
    | 1 == x = One
    | otherwise = MoreThanOne

{-# INLINEABLE enopNFTNameGivenToUniqueAndOnlyOperator #-}
enopNFTNameGivenToUniqueAndOnlyOperator :: CurrencySymbol -> TxInfo -> TokenName
enopNFTNameGivenToUniqueAndOnlyOperator enopNFTCurrencySymbol =
    let nftPropertyViolationMsgs@NFTPropertyViolationMsg{..} =
            NFTPropertyViolationMsg
                { whenNoNFT = "2.0.0"
                , whenQuantityMoreThanOne = "1.0.2"
                , whenMultipleTokenNamesForSameCurrencySymbol = "1.1.1"
                }
     in \case
            (enopTokenName, One) -> enopTokenName
            (_, MoreThanOne) -> propertyViolation whenQuantityMoreThanOne
            . getUniqueTokenNameAndQuantity nftPropertyViolationMsgs enopNFTCurrencySymbol
            . getValuePaidByUniqueSigner

{-# INLINEABLE getValuePaidByUniqueSigner #-}
getValuePaidByUniqueSigner :: TxInfo -> Value
getValuePaidByUniqueSigner txInfo =
    valuePaidTo txInfo
        . getUniqueSigner
        $ txInfo 

{-# INLINEABLE getUniqueSigner #-}
getUniqueSigner :: TxInfo -> PubKeyHash
getUniqueSigner =
    \case
        [] -> propertyViolation "Ledger Property - At least one signer is required"
        [pkh] -> pkh
        _ -> propertyViolation prop_2_1_1_SignerIsNotUnique
        . txInfoSignatories

{-# INLINEABLE enopNFTNameSpentByUniqueOperator #-}
enopNFTNameSpentByUniqueOperator :: CurrencySymbol -> TxInfo -> TokenName
enopNFTNameSpentByUniqueOperator enopNFTCurrencySymbol =
    let nftPropertyViolationMsgs@NFTPropertyViolationMsg{..} =
            NFTPropertyViolationMsg
                { whenNoNFT = "2.0.0"
                , whenQuantityMoreThanOne = "1.0.2"
                , whenMultipleTokenNamesForSameCurrencySymbol = "1.1.1"
                }
     in \case
            (enopTokenName, One) -> enopTokenName
            (_, MoreThanOne) -> propertyViolation whenQuantityMoreThanOne
            . getUniqueTokenNameAndQuantity nftPropertyViolationMsgs enopNFTCurrencySymbol
            . valueSpent


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
