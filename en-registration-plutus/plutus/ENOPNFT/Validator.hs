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
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module ENOPNFT.Validator (
    untypedValidator,
    mkValidator,
    MonetaryPolicySettings (..),
    Action (..),
    wVal,
    validateUnregister,
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

{-- validateUnregister --}
-- Checks if the ENOP-NFT is burnt.
-- We check that the CurrencySymbol of the token is the one of this minting policy
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
        tn = getTokenName (txInfoInputs info) cs
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
        let enopTokenName = getENOPNFTTokenName enopNFTCurrencySymbol txInfo
            ennftTokenName = getENNFTTokenName monetaryPolicySettings scriptValue
         in traceIfFalse "Can't Mint ENNOP - ENOPNFT and ENNFT TokenNames are not equal" (enopTokenName == ennftTokenName)
                && traceIfFalse "Can't Mint ENNOP - ENNOP Minted Not Output to Operator" (isENOPNFTGivenToOperator txInfo enopNFTCurrencySymbol)
                && traceIfFalse "Can't Mint ENNOP - Registration Signature Verification Failed" (checkRegistrationSignature scriptDatum)
    )
        $ getScriptDatumAndValueSpent monetaryPolicySettings txInfo

{-# INLINEABLE isENOPNFTGivenToOperator #-}
isENOPNFTGivenToOperator :: TxInfo -> CurrencySymbol -> Bool
isENOPNFTGivenToOperator txInfo enopNFTCurrencySymbol =
    \case
        [_] -> True
        [] -> traceError "Can't Mint ENNOP - No ENNOP minted in transaction"
        _ -> traceError "Can't Mint ENNOP - More than one ENNOP minted"
        $ getNFTTokenNames enopNFTCurrencySymbol (valuePaidTo txInfo . getUniqueSigner $ txInfo)

{-# INLINEABLE getUniqueSigner #-}
getUniqueSigner :: TxInfo -> PubKeyHash
getUniqueSigner txInfo =
    \case
        [pkh] -> pkh
        [] -> traceError "Can't Mint ENNOP - No unique signer found"
        _ -> traceError "Can't Mint ENNOP - More than one unique signer found"
        $ txInfoSignatories txInfo

{-# INLINEABLE getENOPNFTTokenName #-}
getENOPNFTTokenName :: CurrencySymbol -> TxInfo -> TokenName
getENOPNFTTokenName enopNFTCurrencySymbol txInfo =
    \case
        [tn] -> tn
        [] -> traceError "Can't Mint ENNOP - No ENNOP minted in transaction"
        _ -> traceError "Can't Mint ENNOP - More than one ENNOP minted"
        $ getNFTTokenNames enopNFTCurrencySymbol (txInfoMint txInfo)

{-# INLINEABLE getNFTTokenNames #-}
getNFTTokenNames :: CurrencySymbol -> Value -> [TokenName]
getNFTTokenNames c (Value v) = maybe [] Map.keys (Map.lookup c v)

{-# INLINEABLE getENNFTTokenName #-}
getENNFTTokenName :: MonetaryPolicySettings -> Value -> TokenName
getENNFTTokenName MonetaryPolicySettings{ennftCurrencySymbol} value =
    \case
        [tn] -> tn
        [] -> traceError "Can't Mint ENNOP - No ENNFT on Registration validator output"
        _ -> traceError "Can't Mint ENNOP - More than one ENNFT present"
        $ getNFTTokenNames ennftCurrencySymbol value

{-# INLINEABLE getScriptDatumAndValueSpent #-}
getScriptDatumAndValueSpent :: MonetaryPolicySettings -> TxInfo -> (RegistrationDatum, Value)
getScriptDatumAndValueSpent MonetaryPolicySettings{registrationValidatorHash} txInfo =
    \case
        [(OutputDatum (Datum scriptDatum), scriptValue)] ->
            case fromBuiltinData scriptDatum of
                Nothing -> traceError "Can't Mint ENNOP - Registration datum is not valid"
                Just registrationDatum -> (registrationDatum, scriptValue)
        [] -> traceError "Can't Mint ENNOP - Registration validator output not found"
        [(NoOutputDatum, _)] -> traceError "Can't Mint ENNOP - Registration validator output has no datum"
        [(OutputDatumHash _, _)] -> traceError "Can't Mint ENNOP - Registration validator output has only the hashed datum"
        _ : _ -> traceError "Can't Mint ENNOP - More than one Registration validator output is not allowed"
        $ scriptOutputsAt registrationValidatorHash txInfo

{-- getTokenName --}
-- We determine the TokenName from the input of the registration smart contract,
-- we know it must be exactly one available which must have the same tokenname as the ENOOPNFT
{-# INLINEABLE getTokenName #-}
getTokenName :: [TxInInfo] -> CurrencySymbol -> Maybe TokenName
getTokenName is cs =
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
