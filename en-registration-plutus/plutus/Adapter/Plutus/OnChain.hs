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
  Quantity (..),
  getNFTTokenName,
  tokenNameGivenToUniqueAndOnlySigner,
  tokenNameSpent,
) where

import PlutusTx.Prelude

import Plutus.Script.Utils.Value
import PlutusLedgerApi.V3

import Aya.Registration.Core.Property.Violation (
  NFTPropertyViolationMsg (..),
 )

import Plutus.Script.Utils.V3.Contexts (valueSpent)
import PlutusLedgerApi.V3.Contexts (valuePaidTo)
import PlutusTx qualified
import PlutusTx.Applicative ()
import PlutusTx.AssocMap qualified as Map
import Prelude qualified as Haskell

data Quantity = One | MoreThanOne
  deriving (Haskell.Show)

instance Eq Quantity where
  {-# INLINEABLE (==) #-}
  One == One = True
  MoreThanOne == MoreThanOne = True
  _ == _ = False

PlutusTx.unstableMakeIsData ''Quantity
PlutusTx.makeLift ''Quantity

{-# INLINEABLE convertToQuantity #-}
convertToQuantity :: Integer -> Quantity
convertToQuantity x
  | x <= 0 = propertyViolation "Ledger Property - Quantity can't be negative or zero"
  | 1 == x = One
  | otherwise = MoreThanOne

-- | This function is normally available in the PlutusTx.Maybe module
-- | But its behaviour is unexpected and so it has been redefined here
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
    _ -> propertyViolation "2.1.1"
    . txInfoSignatories

-- | Get the token name of the NFT given to the unique and only signer
{-# INLINEABLE tokenNameGivenToUniqueAndOnlySigner #-}
tokenNameGivenToUniqueAndOnlySigner :: NFTPropertyViolationMsg -> CurrencySymbol -> TxInfo -> TokenName
tokenNameGivenToUniqueAndOnlySigner nftPropertyViolationMsgs enopNFTCurrencySymbol =
  \case
    (enopTokenName, One) -> enopTokenName
    (_, MoreThanOne) -> propertyViolation . whenQuantityMoreThanOne $ nftPropertyViolationMsgs
    . getUniqueTokenNameAndQuantity nftPropertyViolationMsgs enopNFTCurrencySymbol
    . getValuePaidByUniqueSigner

-- | Get the token name of the currency symbol spent
{-# INLINEABLE tokenNameSpent #-}
tokenNameSpent :: NFTPropertyViolationMsg -> CurrencySymbol -> TxInfo -> TokenName
tokenNameSpent nftPropertyViolationMsgs enopNFTCurrencySymbol =
  \case
    (enopTokenName, One) -> enopTokenName
    (_, MoreThanOne) -> propertyViolation . whenQuantityMoreThanOne $ nftPropertyViolationMsgs
    . getUniqueTokenNameAndQuantity nftPropertyViolationMsgs enopNFTCurrencySymbol
    . valueSpent

-- | Get the token name of the NFT given to the unique and only signer
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
