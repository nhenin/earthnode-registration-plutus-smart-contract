{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Spec.NominalCaseSpec (
    specs,
) where

import Cooked (InitialDistribution (..), Wallet, distributionFromList, interpretAndRunWith, printCooked, runMockChain, testSucceeds, testSucceedsFrom, wallet)
import Test.Tasty

import Test.Tasty.QuickCheck

import Control.Monad.IO.Class
import Cooked.ValueUtils
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Default
import Data.String
import OffChain.Actions
import PlutusLedgerApi.V3

instance Show InitialDistribution where
    show (InitialDistribution xs) = "InitialDistribution : " -- ++ show xs

data FixtureNominalCase = FixtureNominalCase
    { genesis :: InitialDistribution
    , partnerChainSettings :: PartnerChainValidatorSettings
    , ennft :: ENNFT
    }
    deriving (Show)

nftToValue :: NFT -> Value
nftToValue NFT{..} = singleton currencySymbol tokenName 1

operator :: Wallet
operator = wallet 1

genFixtureNominalCase :: Gen FixtureNominalCase
genFixtureNominalCase = do
    ennft <- anyENNFT
    anyPartnerChainSettings <- anyPartnerChainValidatorSettings
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
            , partnerChainSettings = anyPartnerChainSettings
            , ennft = ennft
            }

anyPartnerChainValidatorSettings :: Gen PartnerChainValidatorSettings
anyPartnerChainValidatorSettings = do
    ayaValidatorPublicKey <- fmap toBuiltin $ genByteStringOf 28
    commission <- arbitrary
    pure PartnerChainValidatorSettings{..}

anyCurrencySymbol :: Gen CurrencySymbol
anyCurrencySymbol =
    CurrencySymbol
        <$> fmap toBuiltin (genByteStringOf 28)

genByteStringOf :: Int -> Gen ByteString
genByteStringOf n =
    BS.pack <$> vector n

anyENNFTTokenName :: Gen TokenName
anyENNFTTokenName = TokenName . fromString . show <$> chooseInteger (1, 1_0000)

anyENNFT :: Gen ENNFT
anyENNFT = NFT <$> anyCurrencySymbol <*> anyENNFTTokenName

specs :: TestTree
specs =
    localOption (QuickCheckTests 30) $
        testGroup
            "Nominal Cases"
            [ testProperty "Operators Can Register by Providing an ENNFT and Retrieving an ENOPNFT" $
                forAll genFixtureNominalCase $
                    \FixtureNominalCase{..} ->
                        testSucceedsFrom @Property
                            def
                            genesis
                            $ register partnerChainSettings ennft operator
            ]
