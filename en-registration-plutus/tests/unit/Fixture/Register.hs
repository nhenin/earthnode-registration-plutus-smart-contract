{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Fixture.Register (
    FixtureNominalCase (..),
    genFixtureNominalCase,
    FixtureFailureCaseDifferentTokenNames (..),
    genFixtureFailureCaseDifferentTokenNames,
    operator,
    anotherOperator,
) where

import Cooked (InitialDistribution (..), Wallet, distributionFromList, interpretAndRunWith, printCooked, runMockChain, testSucceeds, testSucceedsFrom, wallet)

import Test.Tasty.QuickCheck (
    Arbitrary (arbitrary),
    Gen,
    Property,
    QuickCheckTests (QuickCheckTests),
    chooseInteger,
    elements,
    forAll,
    listOf1,
    suchThat,
    testProperty,
    vector,
 )

import Adapter.CardanoCryptoClass.Crypto (ContextDSIGN, DSIGNAlgorithm (Signable), KeyPair)
import Control.Monad.IO.Class
import Cooked.ValueUtils (ada)
import Data.Aeson (decodeFileStrict)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Default (Default (def))
import Data.String (IsString (fromString))
import OffChain.Actions (ENNFT, NFT (..), register)
import PlutusLedgerApi.V3 (
    CurrencySymbol (CurrencySymbol),
    TokenName (TokenName),
    Value,
    singleton,
    toBuiltin,
 )

instance Show InitialDistribution where
    show (InitialDistribution xs) = "InitialDistribution : " -- ++ show xs

data FixtureNominalCase a = FixtureNominalCase
    { genesis :: InitialDistribution
    , substrateKeyPair :: KeyPair a
    , commission :: Integer
    , ennft :: ENNFT
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

nftToValue :: NFT -> Value
nftToValue NFT{..} = singleton currencySymbol tokenName 1

operator :: Wallet
operator = wallet 1

anotherOperator :: Wallet
anotherOperator = wallet 2

genFixtureNominalCase :: [KeyPair a] -> Gen (FixtureNominalCase a)
genFixtureNominalCase keypairs = do
    substrateKeyPair <- elements keypairs
    ennft <- anyENNFT

    commission <- chooseInteger (0, 100)
    pure
        FixtureNominalCase
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
                            , ada 2 <> nftToValue ennft
                            ]
                        )
                    ]
            , substrateKeyPair = substrateKeyPair
            , commission = commission
            , ennft = ennft
            }

genFixtureFailureCaseDifferentTokenNames :: [KeyPair a] -> Gen (FixtureFailureCaseDifferentTokenNames a)
genFixtureFailureCaseDifferentTokenNames keypairs = do
    substrateKeyPair <- elements keypairs
    ennft <- anyENNFT
    enopNFTTokenNames <- anyENNFTTokenNames `suchThat` (notElem . tokenName $ ennft)
    commission <- chooseInteger (0, 100)
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
                            , ada 2 <> nftToValue ennft
                            ]
                        )
                    ]
            , substrateKeyPair = substrateKeyPair
            , commission = commission
            , ennft = ennft
            , enopNFTTokenNames = enopNFTTokenNames
            }

anyCurrencySymbol :: Gen CurrencySymbol
anyCurrencySymbol =
    CurrencySymbol
        <$> fmap toBuiltin (genByteStringOf 28)

genByteStringOf :: Int -> Gen ByteString
genByteStringOf n =
    BS.pack <$> vector n

anyENNFTTokenName :: Gen TokenName
anyENNFTTokenName = TokenName . fromString . show <$> chooseInteger (1, 1_0000)

anyENNFTTokenNames :: Gen [TokenName]
anyENNFTTokenNames = listOf1 (TokenName . fromString . show <$> chooseInteger (1, 1_0000))

anyENNFT :: Gen ENNFT
anyENNFT = NFT <$> anyCurrencySymbol <*> anyENNFTTokenName
