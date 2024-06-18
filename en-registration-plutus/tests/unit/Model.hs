{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Model (
    ENNFT (..),
    ENOPNFT (..),
    NFT (..),
    Commission,
    Quantity,
    nfts,
) where

import Cardano.Api qualified as Cardano
import Cardano.Node.Emulator qualified as Emulator
import Control.Monad (void)
import Cooked

import Data.Default (Default (def))

import Data.List (foldl')
import Data.List.NonEmpty qualified as NEList
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEMap
import PlutusTx.AssocMap qualified as PMap

import Data.Set (Set)
import Data.Set qualified as Set
import ENOPNFT.OnChainMonetaryPolicy (Action (..))
import Plutus.Script.Utils.Scripts qualified as Script

import Adapter.CardanoCryptoClass.Crypto (
    Codec (encode),
    FromByteString (fromByteString),
    Hexadecimal,
 )
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.List.NonEmpty qualified as NL
import Data.Text qualified as Text
import OnChainRegistrationValidator
    ( RegistrationValidatorSettings(..),
      RegistrationDatum(..),
      mkHashedRegistrationMessage,
      RegistrationAction(Unregister),
      RegistrationAction(..) )
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V3 (BuiltinByteString, CurrencySymbol, PubKeyHash, TokenName, fromBuiltin, toBuiltin, toBuiltinData)
import PlutusLedgerApi.V3 qualified as Api
import PlutusLedgerApi.V3 qualified as V3
import Prettyprinter.Extras (Pretty (..))
import RegistrationValidator (
    associatedENOPNFTCurrencySymbol,
    associatedENOPNFTMonetaryPolicy,
    typedRegistrationValidator,
 )

data NFT = NFT {currencySymbol :: CurrencySymbol, tokenName :: TokenName} deriving (Eq, Show)
type ENNFT = NFT
type ENOPNFT = NFT
type Commission = Integer
type Quantity = Integer

-- | Make a 'Value' containing only the given quantity of the given currency.
nfts :: CurrencySymbol -> [TokenName] -> V3.Value
nfts c tns = V3.Value $ PMap.singleton c (PMap.fromList $ (,1) <$> tns)

instance PrettyCooked Action where
    prettyCookedOpt opts Mint = "Mint ENNOPNFT"
    prettyCookedOpt opts Burn = "Burn ENNOPNFT"

-- instance PrettyCooked RegistrationAction where
--     prettyCookedOpt opts UpdateRegistrationDetails = "UpdateRegistrationDetails"
--     prettyCookedOpt opts Unregister = "Unregister"

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
