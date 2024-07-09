{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Specs.Aya.Registration.Core.Property.NFT.Transitivity.Register.Violations (
  specs,
) where

import Test.Tasty

import Adapter.CardanoCryptoClass.Crypto (ContextDSIGN, DSIGNAlgorithm (Signable), KeyPair)
import Data.ByteString (ByteString)

import Adapter.Cooked
import Aya.Registration.Core.Property.NFT.Transitivity.Register (
  v_1_0_1_ENNOP_Minted_Quantity_Above_One,
  v_1_0_2_No_ENNFT_On_Validator_Output,
  v_1_0_3_ENNFT_Quantity_Above_One,
  v_1_1_0_ENOP_NFT_TokenName_Not_Equal_To_ENNFT_TokenName,
  v_1_1_1_ENOP_NFT_Cardinality_Above_1,
  v_1_1_2_ENNFT_Cardinality_Above_1,
 )
import Specs.Aya.Registration.Core.Model
import Specs.Aya.Registration.Core.Register.Fixture
import Specs.Aya.Registration.Core.Register.TxBuilding (
  registerByGeneratingMoreThan1ENNFT,
  registerByGeneratingMoreThan1ENOPNFT,
  registerMintingAnENOPWithQuantityAbove1,
  registerWithDifferentENandENOPNFTTokenNames,
  registerWithanENNFTWithAQuantityAbove1,
  registerWithoutAnENNFT,
 )
import Test.Tasty.QuickCheck (
  forAll,
  testProperty,
 )

specs :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) => [KeyPair a] -> TestTree
specs keys =
  testGroup
    "Register"
    [ testGroup
        "Property 1.0 : Tokens Quantities are verified"
        [ testProperty
            "1.0.0 violation - No ENNOP Minted (can't be enforced)"
            True
        , testProperty "1.0.1 violation - ENNOP Minted Quantity > 1" $
            forAll (genFixtureFailureCaseDifferentTokenNames keys) $
              \FixtureFailureCaseDifferentTokenNames{..} ->
                shouldViolateAProperty
                  v_1_0_1_ENNOP_Minted_Quantity_Above_One
                  genesis
                  $ registerMintingAnENOPWithQuantityAbove1
                    substrateKeyPair
                    ennft
                    commission
                    operator
        , testProperty "1.0.2 violation - No ENNFT on Registration validator output" $
            forAll (genFixtureNominalCase keys) $
              \FixtureNominalCase{..} ->
                shouldViolateAProperty
                  v_1_0_2_No_ENNFT_On_Validator_Output
                  genesis
                  $ registerWithoutAnENNFT
                    substrateKeyPair
                    ennft
                    commission
                    operator
        , testProperty "1.0.3 violation - ENNFT Quantity > 1" $
            forAll (genFixtureENNFTWithWrongQuantityAbove1 keys) $
              \FixtureENNFTWithWrongQuantityAbove1{..} ->
                shouldViolateAProperty
                  v_1_0_3_ENNFT_Quantity_Above_One
                  genesis
                  $ registerWithanENNFTWithAQuantityAbove1
                    substrateKeyPair
                    ennft
                    wrongENNFTQuantity
                    commission
                    operator
        ]
    , testGroup
        "Property 1.1 : NFTs Token Names & Cardinality equality : There is 1-1 relationship between the ENNFT and the ENOPNFT"
        [ testProperty "1.1.0 violation - ENOPNFT TokenName =/ ENNFT TokenName" $
            forAll (genFixtureFailureCaseDifferentTokenNames keys) $
              \FixtureFailureCaseDifferentTokenNames{..} ->
                shouldViolateAProperty
                  v_1_1_0_ENOP_NFT_TokenName_Not_Equal_To_ENNFT_TokenName
                  genesis
                  $ registerWithDifferentENandENOPNFTTokenNames
                    substrateKeyPair
                    ennft
                    (head enopNFTTokenNames)
                    commission
                    operator
        , testProperty "1.1.1 violation - |ENOP NFT| > 1 (Cardinality Violation)" $
            forAll (genFixtureFailureCaseDifferentTokenNames keys) $
              \FixtureFailureCaseDifferentTokenNames{..} ->
                shouldViolateAProperty
                  v_1_1_1_ENOP_NFT_Cardinality_Above_1
                  genesis
                  $ registerByGeneratingMoreThan1ENOPNFT
                    substrateKeyPair
                    ennft
                    enopNFTTokenNames
                    commission
                    operator
        , testProperty "1.1.2 violation - |EN NFT|   > 1 (Cardinality Violation)" $
            forAll (genFixtureMultipleENNFTs keys) $
              \FixtureMultipleENNFTs{..} ->
                shouldViolateAProperty
                  v_1_1_2_ENNFT_Cardinality_Above_1
                  genesis
                  $ registerByGeneratingMoreThan1ENNFT
                    substrateKeyPair
                    (ennftCurrencySymbol, firstEnnftTn, ennftsTokenNames)
                    commission
                    operator
        ]
    ]
