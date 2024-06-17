{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Register.Spec (
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
import Data.List.NonEmpty qualified as NL
import Data.String (IsString (fromString))
import Data.Text (Text)
import Ledger qualified
import PlutusLedgerApi.V3 (
    CurrencySymbol (CurrencySymbol),
    TokenName (TokenName),
    Value,
    singleton,
    toBuiltin,
 )
import Register.Fixture (
    FixtureENNFTWithWrongQuantityAbove1 (
        FixtureENNFTWithWrongQuantityAbove1,
        commission,
        ennft,
        genesis,
        substrateKeyPair,
        wrongENNFTQuantity
    ),
    FixtureFailureCaseDifferentTokenNames (
        FixtureFailureCaseDifferentTokenNames,
        commission,
        ennft,
        enopNFTTokenNames,
        genesis,
        substrateKeyPair
    ),
    FixtureMultipleENNFTs (
        FixtureMultipleENNFTs,
        commission,
        ennftCurrencySymbol,
        ennftsTokenNames,
        firstEnnftTn,
        genesis,
        substrateKeyPair
    ),
    FixtureNominalCase (
        FixtureNominalCase,
        commission,
        ennft,
        genesis,
        substrateKeyPair
    ),
    FixtureWithInvalidSignature (
        FixtureWithInvalidSignature,
        commission,
        ennft,
        genesis,
        invalidSignature,
        substrateKeyPair
    ),
    anotherOperator,
    genFixtureENNFTWithWrongQuantityAbove1,
    genFixtureFailureCaseDifferentTokenNames,
    genFixtureMultipleENNFTs,
    genFixtureNominalCase,
    genFixtureWithInvalidSignature,
    operator,
 )
import Register.TxBuilding (mintWithoutRegistrationScript, register, registerAndMintToAnotherOperator, registerByGeneratingMoreThan1ENNFT, registerByGeneratingMoreThan1ENOPNFT, registerMintingAnENOPWithQuantityAbove1, registerMoreThanOneOperator, registerWith2WalletsSigning, registerWithDifferentENandENOPNFTTokenNames, registerWithHashedRegistrationDatum, registerWithInvalidDatumVerification, registerWithNoRegistrationDatum, registerWithanENNFTWithAQuantityAbove1, registerWithoutAnENNFT, registerWithoutSigning)
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
            [ testProperty "Register" $
                forAll (genFixtureNominalCase keys) $
                    \FixtureNominalCase{..} ->
                        testSucceedsFrom @Property
                            def
                            genesis
                            $ register substrateKeyPair ennft commission operator
            , testProperty "Update" $
                forAll (genFixtureNominalCase keys) $
                    \FixtureNominalCase{..} ->
                        testSucceedsFrom @Property
                            def
                            genesis
                            $ register substrateKeyPair ennft commission operator
            ]
        , testGroup
            "Property 1 : Non Fungible Property Transitivity : EN Token is an NFT => the ENOP Token should be an NFT "
            [ testGroup
                "Property 1.0 : When Minting ENOP Tokens, Tokens Quantities are verified"
                [ testProperty
                    "Property. 1.0.1 violation - No ENNOP Minted (can't be enforced it is an issue...)"
                    True
                , testProperty "Property. 1.0.2 violation - ENNOP's Minted Quantity > 1" $
                    forAll (genFixtureFailureCaseDifferentTokenNames keys) $
                        \FixtureFailureCaseDifferentTokenNames{..} ->
                            testFailsFrom @Property
                                def
                                ( isCekEvaluationFailureWithMsg
                                    def
                                    (== "1.0.2")
                                )
                                genesis
                                $ registerMintingAnENOPWithQuantityAbove1
                                    substrateKeyPair
                                    ennft
                                    commission
                                    operator
                , testProperty "Property. 1.0.3 violation - No ENNFT on Registration validator output" $
                    forAll (genFixtureNominalCase keys) $
                        \FixtureNominalCase{..} ->
                            testFailsFrom @Property
                                def
                                ( isCekEvaluationFailureWithMsg
                                    def
                                    (== "1.0.3")
                                )
                                genesis
                                $ registerWithoutAnENNFT
                                    substrateKeyPair
                                    ennft
                                    commission
                                    operator
                , testProperty "Property. 1.0.4 violation - ENNFT 's Minted Quantity > 1" $
                    forAll (genFixtureENNFTWithWrongQuantityAbove1 keys) $
                        \FixtureENNFTWithWrongQuantityAbove1{..} ->
                            testFailsFrom @Property
                                def
                                ( isCekEvaluationFailureWithMsg
                                    def
                                    (== "1.0.4")
                                )
                                genesis
                                $ registerWithanENNFTWithAQuantityAbove1
                                    substrateKeyPair
                                    ennft
                                    wrongENNFTQuantity
                                    commission
                                    operator
                ]
            , testGroup
                "Property 1.1 : When Minting ENOP Tokens, NFTs Token Names & Cardinality equality : There is 1-1 relationship between the ENNFT and the ENOPNFT"
                [ testProperty "Property. 1.1.0 violation - ENOPNFT TokenName =/ ENNFT TokenName" $
                    forAll (genFixtureFailureCaseDifferentTokenNames keys) $
                        \FixtureFailureCaseDifferentTokenNames{..} ->
                            testFailsFrom @Property
                                def
                                ( isCekEvaluationFailureWithMsg
                                    def
                                    (== "1.1.0")
                                )
                                genesis
                                $ registerWithDifferentENandENOPNFTTokenNames
                                    substrateKeyPair
                                    ennft
                                    (head enopNFTTokenNames)
                                    commission
                                    operator
                , testProperty "Property. 1.1.1 violation - |ENOP NFT| > 1 (Cardinality Violation)" $
                    forAll (genFixtureFailureCaseDifferentTokenNames keys) $
                        \FixtureFailureCaseDifferentTokenNames{..} ->
                            testFailsFrom @Property
                                def
                                ( isCekEvaluationFailureWithMsg
                                    def
                                    (== "1.1.1")
                                )
                                genesis
                                $ registerByGeneratingMoreThan1ENOPNFT
                                    substrateKeyPair
                                    ennft
                                    enopNFTTokenNames
                                    commission
                                    operator
                , testProperty "Property. 1.1.2 violation - |EN NFT|   > 1 (Cardinality Violation)" $
                    forAll (genFixtureMultipleENNFTs keys) $
                        \FixtureMultipleENNFTs{..} ->
                            testFailsFrom @Property
                                def
                                ( isCekEvaluationFailureWithMsg
                                    def
                                    (== "1.1.2")
                                )
                                genesis
                                $ registerByGeneratingMoreThan1ENNFT
                                    substrateKeyPair
                                    (ennftCurrencySymbol, firstEnnftTn, ennftsTokenNames)
                                    commission
                                    operator
                ]
            , testGroup
                "Property 1.2 : When Burning ENOP Tokens, ENNFT are released from the Script"
                []
            ]
        , testGroup
            "Property 2 : Preserving NFTs ownership : ENOP and ENNFT can be swapped only between the operator and the registration smart contract"
            [ testGroup
                "Property 2.0 : ENOP NFT should be minted only to the operator"
                [ testProperty "Property. 2.0.0 violation - ENNOP Minted Not Output to Operator" $
                    forAll (genFixtureNominalCase keys) $
                        \FixtureNominalCase{..} ->
                            testFailsFrom @Property
                                def
                                ( isCekEvaluationFailureWithMsg
                                    def
                                    (== "2.0.0")
                                )
                                genesis
                                $ registerAndMintToAnotherOperator substrateKeyPair ennft commission operator anotherOperator
                ]
            , testGroup
                "Property 2.1 : Only the operator should sign the transaction"
                [ testProperty
                    "Property. 2.1.0 violation - No signer found (Enforced by Ledger Properties)"
                    True
                , testProperty "Property. 2.1.1 violation - signer is not unique" $
                    forAll (genFixtureNominalCase keys) $
                        \FixtureNominalCase{..} ->
                            testFailsFrom @Property
                                def
                                ( isCekEvaluationFailureWithMsg
                                    def
                                    (== "2.1.1")
                                )
                                genesis
                                $ registerWith2WalletsSigning substrateKeyPair ennft commission operator anotherOperator
                ]
            ]
        , testGroup
            "Property 3 : The registration details (datum) should be verifiable"
            [ testProperty "Property. 3.0 violation - Registration datum is not valid" $
                forAll (genFixtureWithInvalidSignature keys) $
                    \FixtureWithInvalidSignature{..} ->
                        testFailsFrom @Property
                            def
                            ( isCekEvaluationFailureWithMsg
                                def
                                (== "3.0")
                            )
                            genesis
                            $ registerWithInvalidDatumVerification substrateKeyPair ennft commission operator invalidSignature
            , testProperty "Property. 3.1 violation - Registration validator output not found" $
                forAll (genFixtureNominalCase keys) $
                    \FixtureNominalCase{..} ->
                        testFailsFrom @Property
                            def
                            ( isCekEvaluationFailureWithMsg
                                def
                                (== "3.1")
                            )
                            genesis
                            $ mintWithoutRegistrationScript substrateKeyPair ennft operator
            , testProperty "Property. 3.2 violation - Registration validator output has no datum" $
                forAll (genFixtureNominalCase keys) $
                    \FixtureNominalCase{..} ->
                        testFailsFrom @Property
                            def
                            ( isCekEvaluationFailureWithMsg
                                def
                                (== "3.2")
                            )
                            genesis
                            $ registerWithNoRegistrationDatum substrateKeyPair ennft commission operator
            , testProperty "Property. 3.3 violation - Registration validator output has only the hashed datum" $
                forAll (genFixtureNominalCase keys) $
                    \FixtureNominalCase{..} ->
                        testFailsFrom @Property
                            def
                            ( isCekEvaluationFailureWithMsg
                                def
                                (== "3.3")
                            )
                            genesis
                            $ registerWithHashedRegistrationDatum substrateKeyPair ennft commission operator
            , testProperty "Property. 3.4 violation - More than one Registration validator output is not allowed" $
                forAll (genFixtureMultipleENNFTs keys) $
                    \FixtureMultipleENNFTs{..} ->
                        testFailsFrom @Property
                            def
                            ( isCekEvaluationFailureWithMsg
                                def
                                (== "3.4")
                            )
                            genesis
                            $ registerMoreThanOneOperator substrateKeyPair (ennftCurrencySymbol, firstEnnftTn, NL.head ennftsTokenNames) commission operator
            ]
        ]
