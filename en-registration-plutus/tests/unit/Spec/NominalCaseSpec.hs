{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Spec.NominalCaseSpec (
    specs,
) where

import Cooked (InitialDistribution (..), Wallet, distributionFromList, interpretAndRunWith, printCooked, runMockChain, testSucceeds, testSucceedsFrom, wallet)
import Test.Tasty

import Test.Tasty.QuickCheck (
    Gen,
    Property,
    QuickCheckTests (QuickCheckTests),
    chooseInteger,
    elements,
    forAll,
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
import Fixture.Register
import OffChain.Actions (ENNFT, NFT (..), register)
import PlutusLedgerApi.V3 (
    CurrencySymbol (CurrencySymbol),
    TokenName (TokenName),
    Value,
    singleton,
    toBuiltin,
 )

specs :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) => [KeyPair a] -> TestTree
specs keys =
    testGroup
        "Nominal Cases"
        [ testProperty "Operators Can Register by Providing an ENNFT and Retrieving an ENOPNFT" $
            forAll (genFixtureNominalCase keys) $
                \FixtureNominalCase{..} ->
                    testSucceedsFrom @Property
                        def
                        genesis
                        $ register substrateKeyPair ennft commission operator
        ]
