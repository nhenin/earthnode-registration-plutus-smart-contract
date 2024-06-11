{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Spec.RegisterSpec (
    specs,
) where

import Cooked (InitialDistribution (..), Wallet, distributionFromList, interpretAndRunWith, isCekEvaluationFailureWithMsg, printCooked, runMockChain, testFailsFrom, testSucceeds, testSucceedsFrom, wallet)
import Test.Tasty

import Adapter.CardanoCryptoClass.Crypto (ContextDSIGN, DSIGNAlgorithm (Signable), KeyPair)
import Control.Monad.IO.Class
import Cooked.MockChain qualified as C
import Cooked.Pretty qualified as C
import Cooked.ValueUtils (ada)
import Data.Aeson (decodeFileStrict)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Default (Default (def))
import Data.String (IsString (fromString))
import Data.Text (Text)
import Fixture.Register
import Ledger qualified
import OffChain.Actions (ENNFT, NFT (..), register, registerAndMintToAnotherOperator, registerByGeneratingMoreThan1ENOPNFT, registerWithDifferentENandENOPNFTTokenNames)
import PlutusLedgerApi.V3 (
    CurrencySymbol (CurrencySymbol),
    TokenName (TokenName),
    Value,
    singleton,
    toBuiltin,
 )
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

specs :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) => [KeyPair a] -> TestTree
specs keys =
    testGroup
        "Registration Specifications"
        [ testGroup
            "Nominal Cases"
            [ testProperty "Nominal Case - Operators can register" $
                forAll (genFixtureNominalCase keys) $
                    \FixtureNominalCase{..} ->
                        testSucceedsFrom @Property
                            def
                            genesis
                            $ register substrateKeyPair ennft commission operator
            ]
        , testGroup
            "Property 1 : Earth Node being NFT implies ENOP Token Minted is also A Non Fungible Token"
            [ testGroup
                "Property 1.0 : Quantities are preserved when Minting ENOP Tokens"
                [ testProperty "1.0.0 - Property. 1.0.0 violation - ENOPNFT TokenName =/ ENNFT TokenName" $
                    forAll (genFixtureFailureCaseDifferentTokenNames keys) $
                        \FixtureFailureCaseDifferentTokenNames{..} ->
                            testFailsFrom @Property
                                def
                                ( isCekEvaluationFailureWithMsg
                                    def
                                    (== "1.0.0")
                                )
                                genesis
                                $ registerWithDifferentENandENOPNFTTokenNames
                                    substrateKeyPair
                                    ennft
                                    (head enopNFTTokenNames)
                                    commission
                                    operator
                , testProperty "Property. 1.0.1 violation - ENNOP's Minted Quantity > 1" $
                    forAll (genFixtureFailureCaseDifferentTokenNames keys) $
                        \FixtureFailureCaseDifferentTokenNames{..} ->
                            testFailsFrom @Property
                                def
                                ( isCekEvaluationFailureWithMsg
                                    def
                                    (== "1.0.1")
                                )
                                genesis
                                $ registerByGeneratingMoreThan1ENOPNFT
                                    substrateKeyPair
                                    ennft
                                    enopNFTTokenNames
                                    commission
                                    operator
                ]
            , testGroup
                "Property 2 : ENOP NFT should be minted only to the operator"
                [ testProperty "2.0 -Property. 2 violation - ENNOP Minted Not Output to Operator" $
                    forAll (genFixtureNominalCase keys) $
                        \FixtureNominalCase{..} ->
                            testFailsFrom @Property
                                def
                                ( isCekEvaluationFailureWithMsg
                                    def
                                    (== "2.0")
                                )
                                genesis
                                $ registerAndMintToAnotherOperator substrateKeyPair ennft commission operator anotherOperator
                ]
            ]
        ]
