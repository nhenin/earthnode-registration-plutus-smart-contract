{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Specs.Aya.Registration.Core.Property.NFT.Ownership.Violations (
  specs,
) where

import Test.Tasty (TestTree, testGroup)

import Adapter.CardanoCryptoClass.Crypto (ContextDSIGN, DSIGNAlgorithm (Signable), KeyPair)
import Data.ByteString (ByteString)

import Aya.Registration.Core.Property.NFT.Ownership.Register (
  v_2_0_0_ENOP_NFT_Not_Output_To_Operator,
  v_2_1_0_More_Than_One_Signer,
 )
import Specs.Aya.Registration.Core.Register.Fixture
import Specs.Aya.Registration.Core.Register.TxBuilding

import Adapter.Cooked
import Specs.Aya.Registration.Core.Model
import Test.Tasty.QuickCheck (
  forAll,
  testProperty,
 )

specs :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) => [KeyPair a] -> TestTree
specs keys =
  testGroup
    "Property 2 : Preserving NFTs ownership : ENOP and ENNFT can be swapped only between the operator and the registration smart contract"
    [ testGroup
        "Property 2.0 : ENOP NFT should be given to the operator"
        [ testProperty "2.0.0 violation - ENNOP Minted Not Output to Operator" $
            forAll (genFixtureNominalCase keys) $
              \FixtureNominalCase{..} ->
                shouldViolateAProperty
                  v_2_0_0_ENOP_NFT_Not_Output_To_Operator
                  genesis
                  $ registerAndMintToAnotherOperator substrateKeyPair ennft commission operator anotherOperator
        ]
    , testGroup
        "Property 2.1 : Only the operator should sign the transaction"
        [ testProperty
            "2.1.0 violation - No signer found (Enforced by Ledger Properties)"
            True
        , testProperty "2.1.1 violation - signer is not unique" $
            forAll (genFixtureNominalCase keys) $
              \FixtureNominalCase{..} ->
                shouldViolateAProperty
                  v_2_1_0_More_Than_One_Signer
                  genesis
                  $ registerWith2WalletsSigning substrateKeyPair ennft commission operator anotherOperator
        ]
    ]
