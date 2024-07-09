{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Specs.Aya.Registration.Core.Register.Fixture (
  FixtureNominalCase (..),
  genFixtureNominalCase,
  FixtureENNFTWithWrongQuantityAbove1 (..),
  genFixtureENNFTWithWrongQuantityAbove1,
  FixtureFailureCaseDifferentTokenNames (..),
  genFixtureFailureCaseDifferentTokenNames,
  FixtureMultipleENNFTs (..),
  genFixtureMultipleENNFTs,
  FixtureWithInvalidSignature (..),
  genFixtureWithInvalidSignature,
) where

import Cooked (InitialDistribution (..), distributionFromList, walletPKHash)

import Test.Tasty.QuickCheck (
  Gen,
  chooseInteger,
  elements,
  suchThat,
 )

import Adapter.CardanoCryptoClass.Crypto (ContextDSIGN, DSIGNAlgorithm (Signable), KeyPair (..), sign)
import Cooked.ValueUtils (ada)
import Data.ByteString (ByteString)
import Data.Coerce
import Data.List.NonEmpty qualified as NL

import Adapter.Cooked ()
import Adapter.Plutus.Gen (anyAtleast2ENNFTWithSameCurrency, anyENNFT, anyENNFTTokenNames, genByteStringOf)
import Aya.Registration.Core.Validator.Builder
import Aya.Registration.Core.Validator.OnChain
import PlutusLedgerApi.V3 (
  CurrencySymbol (CurrencySymbol),
  TokenName,
  fromBuiltin,
  singleton,
 )
import Specs.Aya.Registration.Core.Model

data FixtureNominalCase a = FixtureNominalCase
  { genesis :: InitialDistribution
  , substrateKeyPair :: KeyPair a
  , commission :: Integer
  , ennft :: ENNFT
  }
instance Show (FixtureNominalCase a) where
  show FixtureNominalCase{..} = "FixtureNominalCase { commission = " <> show commission <> ", ennft = " <> show ennft <> " }"

data FixtureWithInvalidSignature a = FixtureWithInvalidSignature
  { genesis :: InitialDistribution
  , substrateKeyPair :: KeyPair a
  , commission :: Integer
  , ennft :: ENNFT
  , invalidSignature :: ByteString
  }
  deriving (Show)

data FixtureENNFTWithWrongQuantityAbove1 a = FixtureENNFTWithWrongQuantityAbove1
  { genesis :: InitialDistribution
  , substrateKeyPair :: KeyPair a
  , commission :: Integer
  , ennft :: ENNFT
  , wrongENNFTQuantity :: Integer
  }
  deriving (Show)

data FixtureMultipleENNFTs a = FixtureMultipleENNFTs
  { genesis :: InitialDistribution
  , substrateKeyPair :: KeyPair a
  , commission :: Integer
  , ennftCurrencySymbol :: CurrencySymbol
  , firstEnnftTn :: TokenName
  , ennftsTokenNames :: NL.NonEmpty TokenName
  }
  deriving (Show)

data FixtureFailureCaseDifferentTokenNames a = FixtureFailureCaseDifferentTokenNames
  { genesis :: InitialDistribution
  , substrateKeyPair :: KeyPair a
  , commission :: Integer
  , ennft :: ENNFT
  , enopNFTTokenNames :: [TokenName]
  }
  deriving (Show)

genFixtureNominalCase :: [KeyPair a] -> Gen (FixtureNominalCase a)
genFixtureNominalCase keypairs = do
  anEnnft <- anyENNFT
  FixtureNominalCase
    ( distributionFromList
        [
          ( operator
          ,
            [ ada 100
            , ada 5
            , ada 5
            , ada 5
            , ada 5
            , ada 2 <> nftToValue anEnnft
            ]
          )
        ]
    )
    <$> elements keypairs
    <*> chooseInteger (0, 100)
    <*> pure anEnnft

genFixtureFailureCaseDifferentTokenNames :: [KeyPair a] -> Gen (FixtureFailureCaseDifferentTokenNames a)
genFixtureFailureCaseDifferentTokenNames keypairs = do
  aSubstrateKeyPair <- elements keypairs
  anEnnft <- anyENNFT
  anEnopNFTTokenNames <- anyENNFTTokenNames `suchThat` (notElem . tokenName $ anEnnft)
  aCommission <- chooseInteger (0, 100)
  pure
    FixtureFailureCaseDifferentTokenNames
      { genesis =
          distributionFromList
            [
              ( operator
              ,
                [ ada 100
                , ada 5
                , ada 5
                , ada 5
                , ada 5
                , ada 2 <> nftToValue anEnnft
                ]
              )
            ]
      , substrateKeyPair = aSubstrateKeyPair
      , commission = aCommission
      , ennft = anEnnft
      , enopNFTTokenNames = anEnopNFTTokenNames
      }

genFixtureENNFTWithWrongQuantityAbove1 :: [KeyPair a] -> Gen (FixtureENNFTWithWrongQuantityAbove1 a)
genFixtureENNFTWithWrongQuantityAbove1 keypairs = do
  aSubstrateKeyPair <- elements keypairs
  anEnnft <- anyENNFT
  aWrongENNFTQuantity <- chooseInteger (2, 100)
  aCommission <- chooseInteger (0, 100)
  pure
    FixtureENNFTWithWrongQuantityAbove1
      { genesis =
          distributionFromList
            [
              ( operator
              ,
                [ ada 100
                , ada 5
                , ada 5
                , ada 5
                , ada 5
                , ada 2 <> singleton (currencySymbol anEnnft) (tokenName anEnnft) aWrongENNFTQuantity
                ]
              )
            ]
      , substrateKeyPair = aSubstrateKeyPair
      , commission = aCommission
      , ennft = anEnnft
      , wrongENNFTQuantity = aWrongENNFTQuantity
      }

genFixtureMultipleENNFTs :: [KeyPair a] -> Gen (FixtureMultipleENNFTs a)
genFixtureMultipleENNFTs keypairs = do
  aSubstrateKeyPair <- elements keypairs
  (aCurrencySymbol, aFirstEnnftTn, anEnnftsTokenNames) <- anyAtleast2ENNFTWithSameCurrency
  aCommission <- chooseInteger (0, 100)
  pure
    FixtureMultipleENNFTs
      { genesis =
          distributionFromList
            [
              ( operator
              ,
                [ ada 100
                , ada 5
                , ada 5
                , ada 5
                , ada 5
                , ada 2 <> foldMap (\tn -> singleton aCurrencySymbol tn 1) (anEnnftsTokenNames <> NL.fromList [aFirstEnnftTn])
                ]
              )
            ]
      , substrateKeyPair = aSubstrateKeyPair
      , commission = aCommission
      , ennftCurrencySymbol = aCurrencySymbol
      , firstEnnftTn = aFirstEnnftTn
      , ennftsTokenNames = anEnnftsTokenNames
      }

genFixtureWithInvalidSignature
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) => [KeyPair a] -> Gen (FixtureWithInvalidSignature a)
genFixtureWithInvalidSignature keypairs = do
  aSubstrateKeyPair <- elements keypairs
  anEnnft <- anyENNFT
  aCommission <- chooseInteger (0, 100)
  let settings = coerce . currencySymbol $ anEnnft
      validSignedMessage =
        sign
          (signatureKey aSubstrateKeyPair)
          ( fromBuiltin $
              mkHashedRegistrationMessage
                (tokenName anEnnft)
                (walletPKHash operator)
                aCommission -- signing with a different commission
                (associatedENOPNFTCurrencySymbol settings)
          )
  anInvalidSignature <- genByteStringOf 64 `suchThat` (/= validSignedMessage)
  pure
    FixtureWithInvalidSignature
      { genesis =
          distributionFromList
            [
              ( operator
              ,
                [ ada 100
                , ada 5
                , ada 5
                , ada 5
                , ada 5
                , ada 2 <> nftToValue anEnnft
                ]
              )
            ]
      , substrateKeyPair = aSubstrateKeyPair
      , commission = aCommission
      , ennft = anEnnft
      , invalidSignature = anInvalidSignature
      }
