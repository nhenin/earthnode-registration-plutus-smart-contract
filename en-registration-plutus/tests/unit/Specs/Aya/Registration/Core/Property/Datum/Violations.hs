{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Specs.Aya.Registration.Core.Property.Datum.Violations (
  specs,
) where

import Cooked (isCekEvaluationFailureWithMsg, testFailsFrom)
import Test.Tasty

import Adapter.CardanoCryptoClass.Crypto (ContextDSIGN, DSIGNAlgorithm (Signable), KeyPair)
import Data.ByteString (ByteString)
import Data.Default (Default (def))
import Data.List.NonEmpty qualified as NL

import Specs.Aya.Registration.Core.Model
import Specs.Aya.Registration.Core.Register.Fixture
import Specs.Aya.Registration.Core.Register.TxBuilding (
  mintWithoutRegistrationScript,
  registerMoreThanOneOperator,
  registerWithHashedRegistrationDatum,
  registerWithInvalidDatumVerification,
  registerWithNoRegistrationDatum,
 )
import Test.Tasty.QuickCheck (
  Property,
  forAll,
  testProperty,
 )

specs :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) => [KeyPair a] -> TestTree
specs keys =
  testGroup
    "Property 3 - Datum Authenticity & Validity : The registration details (datum) should be verifiable and Valid (Signed and Provided by the owner of the ENNFT)"
    [ testProperty "3.0 violation - Registration datum is not valid" $
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
              $ registerMoreThanOneOperator
                substrateKeyPair
                (ennftCurrencySymbol, firstEnnftTn, NL.head ennftsTokenNames)
                commission
                operator
    ]
