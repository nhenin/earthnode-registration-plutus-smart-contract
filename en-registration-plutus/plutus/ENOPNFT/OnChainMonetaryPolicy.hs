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

module ENOPNFT.OnChainMonetaryPolicy (
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

import Adapter.Plutus
import OnChainRegistrationValidator (RegistrationDatum, checkRegistrationSignature)
import Plutus.Script.Utils.Scripts (ValidatorHash (..))
import Plutus.Script.Utils.V3.Contexts (scriptOutputsAt)
import Plutus.Script.Utils.Value (flattenValue, valueOf)
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.Contexts (ownCurrencySymbol, valuePaidTo)
import PlutusTx.AssocMap qualified as Map
import Specifications

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
        propertyViolationIfFalse prop_1_1_0_enopAndEnNFTWithDifferentName (getENOPNFTTokenName enopNFTCurrencySymbol txInfo == getENNFTTokenName monetaryPolicySettings scriptValue)
            && propertyViolationIfFalse prop_3_0_InvalidSignature (checkRegistrationSignature scriptDatum)
            && isENOPNFTGivenToOperator enopNFTCurrencySymbol txInfo
    )
        . getScriptDatumAndValueSpent monetaryPolicySettings
        $ txInfo

{-# INLINEABLE getENOPNFTTokenName #-}
getENOPNFTTokenName :: CurrencySymbol -> TxInfo -> TokenName
getENOPNFTTokenName enopNFTCurrencySymbol =
    getNFTTokenName
        mkENOPNFTPropertyViolationMsg
        enopNFTCurrencySymbol
        . txInfoMint

{-# INLINEABLE getENNFTTokenName #-}
getENNFTTokenName :: MonetaryPolicySettings -> Value -> TokenName
getENNFTTokenName MonetaryPolicySettings{ennftCurrencySymbol} =
    getNFTTokenName mkENNFTPropertyViolationMsg ennftCurrencySymbol

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
