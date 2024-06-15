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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- Options
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

{-# HLINT ignore "Use second" #-}

module ENOPNFT.Validator (
    untypedValidator,
    mkValidator,
    MonetaryPolicySettings (..),
    Action (..),
    wVal,
    validateUnregister,
    getHumanReadable,
) where

import PlutusTx
import PlutusTx.Prelude

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Prelude qualified

import Plutus.Script.Utils.Scripts (ValidatorHash (..))
import Plutus.Script.Utils.V3.Contexts (scriptOutputsAt)
import Plutus.Script.Utils.Value (flattenValue, valueOf)
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.Contexts (ownCurrencySymbol, valuePaidTo)
import PlutusTx.AssocMap qualified as Map
import Validator (RegistrationDatum, checkRegistrationSignature)

data Quantity = One | MoreThanOne
    deriving (Prelude.Show)

instance Eq Quantity where
    {-# INLINEABLE (==) #-}
    One == One = True
    MoreThanOne == MoreThanOne = True
    _ == _ = False

PlutusTx.unstableMakeIsData ''Quantity
PlutusTx.makeLift ''Quantity

data MonetaryPolicySettings = MonetaryPolicySettings
    { ennftCurrencySymbol :: CurrencySymbol
    , registrationValidatorHash :: ValidatorHash
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

{-# INLINEABLE getHumanReadable #-}

{- | Get the human readable representation on an Error Message Onchain
| N.B : This function should be used only for OffChain Logic, It has a double purpose :
| 1. To provide a human readable error message to the user for OffChain Logic
| 2. To provide direct documentation on the OnChain Logic
-}
getHumanReadable :: BuiltinString -> BuiltinString
getHumanReadable =
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
        _ -> "unknown error code"

{-# INLINEABLE prop_1_1_0_enopAndEnNFTWithDifferentName #-}
{-# INLINEABLE prop_2_1_1_SignerIsNotUnique #-}
{-# INLINEABLE prop_3_0_InvalidSignature #-}
{-# INLINEABLE prop_3_1_RegistrationScriptNotFound #-}
{-# INLINEABLE prop_3_2_NoRegistrationDatum #-}
{-# INLINEABLE prop_3_3_OnlyHashRegistration #-}
{-# INLINEABLE prop_3_4_MoreThanOneRegistrationOutput #-}
prop_1_1_0_enopAndEnNFTWithDifferentName
    , prop_2_1_1_SignerIsNotUnique
    , prop_3_0_InvalidSignature
    , prop_3_1_RegistrationScriptNotFound
    , prop_3_2_NoRegistrationDatum
    , prop_3_3_OnlyHashRegistration
    , prop_3_4_MoreThanOneRegistrationOutput ::
        BuiltinString
prop_1_1_0_enopAndEnNFTWithDifferentName = "1.1.0"
prop_2_1_1_SignerIsNotUnique = "2.1.1"
prop_3_0_InvalidSignature = "3.0"
prop_3_1_RegistrationScriptNotFound = "3.1"
prop_3_2_NoRegistrationDatum = "3.2"
prop_3_3_OnlyHashRegistration = "3.3"
prop_3_4_MoreThanOneRegistrationOutput = "3.4"

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

{-# INLINEABLE propertyViolation #-}
propertyViolation :: BuiltinString -> a
propertyViolation = traceError

{-# INLINEABLE propertyViolationIfFalse #-}
propertyViolationIfFalse :: BuiltinString -> Bool -> Bool
propertyViolationIfFalse = traceIfFalse

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

{-# INLINEABLE canMintENOPNFT #-}
canMintENOPNFT :: MonetaryPolicySettings -> CurrencySymbol -> TxInfo -> Bool
canMintENOPNFT monetaryPolicySettings enopNFTCurrencySymbol txInfo =
    ( \(scriptDatum, scriptValue) ->
        let enopNFTPropertyViolationMsg = mkENOPNFTPropertyViolationMsg
            enopTokenName =
                getNFTTokenName
                    enopNFTPropertyViolationMsg
                    enopNFTCurrencySymbol
                    (txInfoMint txInfo)
            ennftPropertyViolationMsg = mkENNFTPropertyViolationMsg
            ennftTokenName =
                getNFTTokenName
                    ennftPropertyViolationMsg
                    (ennftCurrencySymbol monetaryPolicySettings)
                    scriptValue
         in propertyViolationIfFalse
                prop_1_1_0_enopAndEnNFTWithDifferentName
                (enopTokenName == ennftTokenName)
                && propertyViolationIfFalse
                    prop_3_0_InvalidSignature
                    (checkRegistrationSignature scriptDatum)
                && isENOPNFTGivenToOperator enopNFTCurrencySymbol txInfo
    )
        $ getScriptDatumAndValueSpent monetaryPolicySettings txInfo

{-# INLINEABLE isENOPNFTGivenToOperator #-}
isENOPNFTGivenToOperator :: CurrencySymbol -> TxInfo -> Bool
isENOPNFTGivenToOperator enopNFTCurrencySymbol =
    let nftPropertyViolationMsgs@NFTPropertyViolationMsg{..} =
            NFTPropertyViolationMsg
                { whenNoNFT = "2.0.0"
                , whenQuantityMoreThanOne = "1.0.2"
                , whenMultipleTokenNamesForSameCurrencySymbol = "1.1.1"
                }
     in \case
            (_, One) -> True
            (_, MoreThanOne) -> propertyViolation whenQuantityMoreThanOne
            . getUniqueTokenNameAndQuantity nftPropertyViolationMsgs enopNFTCurrencySymbol
            . getValuePaidByUniqueSigner

{-# INLINEABLE getValuePaidByUniqueSigner #-}
getValuePaidByUniqueSigner :: TxInfo -> Value
getValuePaidByUniqueSigner txInfo =
    valuePaidTo txInfo
        . getUniqueSigner
        $ txInfo -- equivalent could be `valuePaidTo <*> getUniqueSigner` but Applicative (->) doesn't exist in PlutusTx

{-# INLINEABLE getUniqueSigner #-}
getUniqueSigner :: TxInfo -> PubKeyHash
getUniqueSigner =
    \case
        [] -> propertyViolation "Ledger Property - At least one signer is required"
        [pkh] -> pkh
        _ -> propertyViolation prop_2_1_1_SignerIsNotUnique
        . txInfoSignatories

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
getTokenNamesAndQuantities currencySymbol (Value value) =
    maybe
        []
        (((\(tokenName, quantity) -> (tokenName, convertToQuantity quantity)) <$>) . Map.toList)
        (Map.lookup currencySymbol value)

{-# INLINEABLE convertToQuantity #-}
convertToQuantity :: Integer -> Quantity
convertToQuantity x
    | x <= 0 = propertyViolation "Ledger Property - Quantity can't be negative or zero"
    | 1 == x = One
    | otherwise = MoreThanOne

{-# INLINEABLE getScriptDatumAndValueSpent #-}
getScriptDatumAndValueSpent :: MonetaryPolicySettings -> TxInfo -> (RegistrationDatum, Value)
getScriptDatumAndValueSpent MonetaryPolicySettings{registrationValidatorHash} txInfo =
    \case
        [(OutputDatum (Datum scriptDatum), scriptValue)] ->
            case fromBuiltinData scriptDatum of
                Nothing -> propertyViolation "TODO"
                Just registrationDatum -> (registrationDatum, scriptValue)
        [] -> propertyViolation prop_3_1_RegistrationScriptNotFound
        [(NoOutputDatum, _)] -> propertyViolation prop_3_2_NoRegistrationDatum
        [(OutputDatumHash _, _)] -> propertyViolation prop_3_3_OnlyHashRegistration
        _ : _ -> propertyViolation prop_3_4_MoreThanOneRegistrationOutput
        $ scriptOutputsAt registrationValidatorHash txInfo

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

-- Main Mintin Policy
{-# INLINEABLE mkValidator #-}
mkValidator :: MonetaryPolicySettings -> Action -> ScriptContext -> Bool
mkValidator sp Mint ctx = canMintENOPNFT sp (ownCurrencySymbol ctx) (scriptContextTxInfo ctx)
mkValidator _ Burn ctx = validateUnregister (ownCurrencySymbol ctx) (scriptContextTxInfo ctx)

{-# INLINEABLE untypedValidator #-}
untypedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedValidator s r c =
    wVal mkValidator (unsafeFromBuiltinData s) (unsafeFromBuiltinData r) (unsafeFromBuiltinData c)

{-# INLINEABLE wVal #-}
wVal ::
    forall s r c.
    (UnsafeFromData s, UnsafeFromData r, UnsafeFromData c) =>
    (s -> r -> c -> Bool) ->
    BuiltinData ->
    BuiltinData ->
    BuiltinData ->
    ()
wVal f s r c = check (f (unsafeFromBuiltinData s) (unsafeFromBuiltinData r) (unsafeFromBuiltinData c))
