{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Register.Fixture (
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
    operator,
    anotherOperator,
) where

import Cooked (InitialDistribution (..), Wallet, distributionFromList, interpretAndRunWith, printCooked, runMockChain, testSucceeds, testSucceedsFrom, wallet, walletPKHash)

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

import Adapter.CardanoCryptoClass.Crypto (ContextDSIGN, DSIGNAlgorithm (Signable), KeyPair (..), sign)
import Control.Monad.IO.Class
import Cooked.ValueUtils (ada)
import Data.Aeson (decodeFileStrict)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Coerce
import Data.Default (Default (def))
import Data.Foldable (Foldable (..))
import Data.List.NonEmpty qualified as NL
import Data.String (IsString (fromString))

import Model
import OnChainRegistrationValidator
import PlutusLedgerApi.V3 (
    CurrencySymbol (CurrencySymbol),
    TokenName (TokenName),
    Value,
    fromBuiltin,
    singleton,
    toBuiltin,
 )
import RegistrationValidator

instance Show InitialDistribution where
    show (InitialDistribution xs) = "InitialDistribution : " ++ show xs

data FixtureNominalCase a = FixtureNominalCase
    { genesis :: InitialDistribution
    , substrateKeyPair :: KeyPair a
    , commission :: Integer
    , ennft :: ENNFT
    }
    deriving (Show)

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

genFixtureENNFTWithWrongQuantityAbove1 :: [KeyPair a] -> Gen (FixtureENNFTWithWrongQuantityAbove1 a)
genFixtureENNFTWithWrongQuantityAbove1 keypairs = do
    substrateKeyPair <- elements keypairs
    ennft <- anyENNFT
    wrongENNFTQuantity <- chooseInteger (2, 100)
    commission <- chooseInteger (0, 100)
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
                            , ada 2 <> singleton (currencySymbol ennft) (tokenName ennft) wrongENNFTQuantity
                            ]
                        )
                    ]
            , substrateKeyPair = substrateKeyPair
            , commission = commission
            , ennft = ennft
            , wrongENNFTQuantity = wrongENNFTQuantity
            }

genFixtureMultipleENNFTs :: [KeyPair a] -> Gen (FixtureMultipleENNFTs a)
genFixtureMultipleENNFTs keypairs = do
    substrateKeyPair <- elements keypairs
    (currencySymbol, firstEnnftTn, ennftsTokenNames) <- anyAtleast2ENNFTWithSameCurrency
    commission <- chooseInteger (0, 100)
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
                            , ada 2 <> foldMap (\tn -> singleton currencySymbol tn 1) (ennftsTokenNames <> NL.fromList [firstEnnftTn])
                            ]
                        )
                    ]
            , substrateKeyPair = substrateKeyPair
            , commission = commission
            , ennftCurrencySymbol = currencySymbol
            , firstEnnftTn = firstEnnftTn
            , ennftsTokenNames = ennftsTokenNames
            }

genFixtureWithInvalidSignature :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) => [KeyPair a] -> Gen (FixtureWithInvalidSignature a)
genFixtureWithInvalidSignature keypairs = do
    substrateKeyPair <- elements keypairs
    ennft <- anyENNFT
    commission <- chooseInteger (0, 100)
    let settings = coerce . currencySymbol $ ennft
        validSignedMessage =
            sign
                (signatureKey substrateKeyPair)
                ( fromBuiltin $
                    mkHashedRegistrationMessage
                        (tokenName ennft)
                        (walletPKHash operator)
                        commission -- signing with a different commission
                        (associatedENOPNFTCurrencySymbol settings)
                )
    invalidSignature <- genByteStringOf 64 `suchThat` (/= validSignedMessage)
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
                            , ada 2 <> nftToValue ennft
                            ]
                        )
                    ]
            , substrateKeyPair = substrateKeyPair
            , commission = commission
            , ennft = ennft
            , invalidSignature = invalidSignature
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

anyAtleast2ENNFTWithSameCurrency :: Gen (CurrencySymbol, TokenName, NL.NonEmpty TokenName)
anyAtleast2ENNFTWithSameCurrency = do
    currencySymbol <- anyCurrencySymbol
    tn1s <- listOf1 (TokenName . fromString . show <$> chooseInteger (1, 500))
    tn2s <- listOf1 (TokenName . fromString . show <$> chooseInteger (500, 1_000))
    pure (currencySymbol, head tn1s, NL.fromList tn2s)
