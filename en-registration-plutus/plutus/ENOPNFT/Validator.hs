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

data Quantity = Zero | One | MoreThanOne
    deriving (Prelude.Show)

instance Eq Quantity where
    {-# INLINEABLE (==) #-}
    Zero == Zero = True
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
        -- Property 1 : Earth Node being NFT implies ENOP Token Minted is also A Non Fungible Token
        -- Property 1.0 : Quantities are preserved when Minting ENOP Tokens
        "1.0.0" -> "Property. 1.0.0 violation - ENOPNFT TokenName =/ ENNFT TokenName"
        "1.0.1" -> "Property. 1.0.1 violation - ENNOP's Minted Quantity > 1"
        "1.0.2" -> "Property. 1.0.2 violation - ENNOP's Minted Quantity == 0"
        "1.0.3" -> "Property. 1.0.3 violation - ENNFT 's Minted Quantity > 1"
        "1.0.4" -> "Property. 1.0.4 violation - No ENNFT on Registration validator output"
        -- Property 1.1 : Quantities are preserved when Burning ENOP Tokens
        -- TODO
        -- Property 2 : ENOP NFT should be minted only to the operator
        "2.0" -> "Property. 2 violation - ENNOP Minted Not Output to Operator"
        -- Property 3 : Only the operator should sign the transaction
        "3.0" -> "Property. 3 violation - No signer found"
        "3.1" -> "Property. 3 violation - signer is not unique"
        -- Property 4 : The registration Datum should be verifiable
        "4.0" -> "Property. 4 violation - Registration datum is not valid"
        "4.1" -> "Property. 4 violation - Registration validator output not found"
        "4.2" -> "Property. 4 violation - Registration validator output has no datum"
        "4.3" -> "Property. 4 violation - Registration validator output has only the hashed datum"
        "4.4" -> "Property. 4 violation - More than one Registration validator output is not allowed"
        _ -> "unknown error code"

{-# INLINEABLE prop_1_0_0_enopAndEnNFTWithDifferentName #-}
{-# INLINEABLE prop_1_0_1_enopNFTQuantityMintedAboveOne #-}
{-# INLINEABLE prop_1_0_2_enopNFTQuantityMintedEqualNone #-}
{-# INLINEABLE prop_1_0_3_enNFTQuantityMintedAboveOne #-}
{-# INLINEABLE prop_1_0_4_NoENNFTRegisteredOnScript #-}
prop_1_0_0_enopAndEnNFTWithDifferentName
    , prop_1_0_1_enopNFTQuantityMintedAboveOne
    , prop_1_0_2_enopNFTQuantityMintedEqualNone
    , prop_1_0_3_enNFTQuantityMintedAboveOne
    , prop_1_0_4_NoENNFTRegisteredOnScript ::
        BuiltinString
prop_1_0_0_enopAndEnNFTWithDifferentName = "1.0.0"
prop_1_0_1_enopNFTQuantityMintedAboveOne = "1.0.1"
prop_1_0_2_enopNFTQuantityMintedEqualNone = "1.0.2"
prop_1_0_3_enNFTQuantityMintedAboveOne = "1.0.3"
prop_1_0_4_NoENNFTRegisteredOnScript = "1.0.4"

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

{-- canMintEnOpNft --}
-- Validates that the enopnft is minted only if the registration conditions are met :
--  - Makes sure the ennft is paid to the registration smart contract and the UTxO contains
--    a valid ENRegistration Datum with that ennft stored in the datum and
--  - the enopnft is named same as the ennft. Also the own currency symbol must be stored correctly in the datum.
--  - Additionally it checks the Aya Signature of this Datum.
{-# INLINEABLE canMintENOPNFT #-}
canMintENOPNFT :: MonetaryPolicySettings -> CurrencySymbol -> TxInfo -> Bool
canMintENOPNFT monetaryPolicySettings enopNFTCurrencySymbol txInfo =
    ( \(scriptDatum, scriptValue) ->
        let enopTokenName =
                getNFTTokenName
                    ( prop_1_0_2_enopNFTQuantityMintedEqualNone
                    , prop_1_0_1_enopNFTQuantityMintedAboveOne
                    )
                    enopNFTCurrencySymbol
                    (txInfoMint txInfo)
            ennftTokenName =
                getNFTTokenName
                    ( prop_1_0_4_NoENNFTRegisteredOnScript
                    , prop_1_0_3_enNFTQuantityMintedAboveOne
                    )
                    (ennftCurrencySymbol monetaryPolicySettings)
                    scriptValue
         in propertyViolationIfFalse
                prop_1_0_0_enopAndEnNFTWithDifferentName
                (enopTokenName == ennftTokenName)
                && isENOPNFTGivenToOperator enopNFTCurrencySymbol txInfo
                && checkRegistrationSignature scriptDatum
    )
        $ getScriptDatumAndValueSpent monetaryPolicySettings txInfo

{-# INLINEABLE isENOPNFTGivenToOperator #-}
isENOPNFTGivenToOperator :: CurrencySymbol -> TxInfo -> Bool
isENOPNFTGivenToOperator enopNFTCurrencySymbol =
    \case
        (_, Zero) -> propertyViolation prop_1_0_2_enopNFTQuantityMintedEqualNone
        (_, One) -> True
        (_, MoreThanOne) -> propertyViolation prop_1_0_3_enNFTQuantityMintedAboveOne
        . getUniqueTokenNameAndQuantity enopNFTCurrencySymbol
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
        [pkh] -> pkh
        [] -> traceError "3"
        _ -> traceError "4"
        . txInfoSignatories

{-# INLINEABLE getNFTTokenName #-}
getNFTTokenName :: (BuiltinString, BuiltinString) -> CurrencySymbol -> Value -> TokenName
getNFTTokenName (quantityZero, quantityMoreThanOne) enopNFTCurrencySymbol =
    \case
        (_, Zero) -> propertyViolation quantityZero
        (tn, One) -> tn
        (_, MoreThanOne) -> propertyViolation quantityMoreThanOne
        . getUniqueTokenNameAndQuantity enopNFTCurrencySymbol

{-# INLINEABLE getUniqueTokenNameAndQuantity #-}
getUniqueTokenNameAndQuantity :: CurrencySymbol -> Value -> (TokenName, Quantity)
getUniqueTokenNameAndQuantity c =
    \case
        [] -> propertyViolation prop_1_0_2_enopNFTQuantityMintedEqualNone
        [(tn, quantity)] -> (tn, quantity)
        _ -> propertyViolation prop_1_0_1_enopNFTQuantityMintedAboveOne
        . getTokenNamesAndQuantities c

{-# INLINEABLE getTokenNamesAndQuantities #-}
getTokenNamesAndQuantities :: CurrencySymbol -> Value -> [(TokenName, Quantity)]
getTokenNamesAndQuantities c (Value v) =
    maybe
        []
        (((\(tn, i) -> (tn, convertToQuantity i)) <$>) . Map.toList)
        (Map.lookup c v)

{-# INLINEABLE convertToQuantity #-}
convertToQuantity :: Integer -> Quantity
convertToQuantity x
    | 0 == x = Zero
    | 1 == x = One
    | 1 > x = MoreThanOne
    | otherwise = propertyViolation "Negative quantity"

{-# INLINEABLE getScriptDatumAndValueSpent #-}
getScriptDatumAndValueSpent :: MonetaryPolicySettings -> TxInfo -> (RegistrationDatum, Value)
getScriptDatumAndValueSpent MonetaryPolicySettings{registrationValidatorHash} txInfo =
    \case
        [(OutputDatum (Datum scriptDatum), scriptValue)] ->
            case fromBuiltinData scriptDatum of
                Nothing -> propertyViolation prop_1_0_4_NoENNFTRegisteredOnScript
                Just registrationDatum -> (registrationDatum, scriptValue)
        [] -> traceError "11"
        [(NoOutputDatum, _)] -> traceError "12"
        [(OutputDatumHash _, _)] -> traceError "13"
        _ : _ -> traceError "14"
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
