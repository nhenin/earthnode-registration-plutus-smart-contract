{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Adapter.Plutus.OnChain (
    propertyViolation,
    propertyViolationIfFalse,
    fromMaybe',
    findOwnInput,
    ownHash,
    scriptOutputsAt,
    getUniqueSigner,
    getValuePaidByUniqueSigner,
) where

import PlutusTx.Prelude

import Plutus.Script.Utils.Value
import PlutusLedgerApi.V3

import Aya.Registration.Core.Property.Violation
import PlutusLedgerApi.V3.Contexts (valuePaidTo)
import PlutusTx.Applicative ()

{- | This function is normally available in the PlutusTx.Maybe module
| But its behaviour is unexpected and so it has been redefined here
-}
{-# INLINEABLE fromMaybe' #-}
fromMaybe' :: a -> Maybe a -> a
fromMaybe' d =
    \case
        Nothing -> d
        Just v -> v

{-# INLINEABLE propertyViolation #-}
propertyViolation :: BuiltinString -> a
propertyViolation = traceError

{-# INLINEABLE propertyViolationIfFalse #-}
propertyViolationIfFalse :: BuiltinString -> Bool -> Bool
propertyViolationIfFalse = traceIfFalse

{-# INLINEABLE findOwnInput #-}
findOwnInput :: ScriptContext -> Maybe TxInInfo
findOwnInput
    ScriptContext
        { scriptContextTxInfo = TxInfo{txInfoInputs}
        , scriptContextPurpose = Spending txOutRef
        } =
        find
            (\TxInInfo{txInInfoOutRef} -> txInInfoOutRef == txOutRef)
            txInfoInputs
findOwnInput _ = Nothing

-- | Get the validator and datum hashes of the output that is curently being validated
{-# INLINEABLE ownHash #-}
ownHash :: ScriptContext -> ScriptHash
ownHash
    ( findOwnInput ->
            Just
                TxInInfo
                    { txInInfoResolved =
                        TxOut
                            { txOutAddress = Address (ScriptCredential s) _
                            }
                    }
        ) = s
ownHash _ = traceError "Lg" -- "Can't get validator and datum hashes"

{-# INLINEABLE outputsAt #-}

-- | Get the datums and values paid to an address by a pending transaction.
outputsAt :: Address -> TxInfo -> [(OutputDatum, Value)]
outputsAt addr p =
    let flt TxOut{txOutAddress, txOutValue, txOutDatum} | txOutAddress == addr = Just (txOutDatum, txOutValue)
        flt _ = Nothing
     in mapMaybe flt (txInfoOutputs p)

{-# INLINEABLE scriptOutputsAt #-}

-- | Get the datums and values paid to an address of a validator by a pending transaction.
scriptOutputsAt :: ScriptHash -> TxInfo -> [(OutputDatum, Value)]
scriptOutputsAt s = outputsAt (Address (ScriptCredential s) Nothing)

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
