{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Specs.Aya.Registration.Core.Model (
  ENNFT,
  ENOPNFT,
  NFT (..),
  Commission,
  Quantity,
  nfts,
  nftToValue,
  operator,
  anotherOperator,
) where

import Cooked

import PlutusTx.AssocMap qualified as PMap

import Aya.Registration.Core.ENOPNFT.MonetaryPolicy.OnChain (Action (..))

import Adapter.CardanoCryptoClass.Crypto
import Aya.Registration.Core.Validator.OnChain (
  RegistrationAction (..),
  RegistrationDatum (..),
 )
import Data.Text qualified as Text
import PlutusLedgerApi.V3 (CurrencySymbol, TokenName, Value, fromBuiltin)
import PlutusLedgerApi.V3 qualified as V3
import Prettyprinter.Extras (Pretty (..))

data NFT = NFT {currencySymbol :: CurrencySymbol, tokenName :: TokenName} deriving (Eq, Show)
type ENNFT = NFT
type ENOPNFT = NFT
type Commission = Integer
type Quantity = Integer

-- | Make a 'Value' containing only the given quantity of the given currency.
nfts :: CurrencySymbol -> [TokenName] -> V3.Value
nfts c tns = V3.Value $ PMap.singleton c (PMap.fromList $ (,1) <$> tns)

nftToValue :: NFT -> Value
nftToValue NFT{..} = V3.singleton currencySymbol tokenName 1

operator :: Wallet
operator = wallet 1

anotherOperator :: Wallet
anotherOperator = wallet 2

instance PrettyCooked Action where
  prettyCookedOpt _ Mint = "Mint ENNOPNFT"
  prettyCookedOpt _ Burn = "Burn ENNOPNFT"

instance PrettyCooked RegistrationAction where
  prettyCookedOpt _ UpdateRegistrationDetails = "UpdateRegistrationDetails"
  prettyCookedOpt _ Unregister = "Unregister"

instance PrettyCooked RegistrationDatum where
  prettyCookedOpt opts RegistrationDatum{..} =
    "RegistrationDatum: "
      <> "\n  ayaValidatorPublicKey: "
      <> pretty (Text.unpack . encode . fromByteString @Hexadecimal . fromBuiltin $ ayaValidatorPublicKey)
      <> "\n  ennftTokenName: "
      <> prettyCookedOpt opts ennftTokenName
      <> "\n  cardanoRewardPubKey: "
      <> prettyCookedOpt opts cardanoRewardPubKey
      <> "\n  commission: "
      <> prettyCookedOpt opts commission
      <> "\n  enopNFTCurrencySymbol: "
      <> prettyCookedOpt opts enopNFTCurrencySymbol
      <> "\n  signature: "
      <> pretty (Text.unpack . encode . fromByteString @Hexadecimal . fromBuiltin $ signature)
