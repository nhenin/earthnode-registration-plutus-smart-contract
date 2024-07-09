{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (
  main,
) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import qualified Specs.Aya.Registration.Core.Property.Datum.Violations as Property.Datum
import qualified Specs.Aya.Registration.Core.Property.NFT.Ownership.Violations as Property.NFT.Ownership
import qualified Specs.Aya.Registration.Core.Property.NFT.Transitivity.Register.Violations as Property.NFT.Transitivity.Register
import qualified Specs.Aya.Registration.Core.Property.NFT.Transitivity.Update.Violations as Property.NFT.Transitivity.Update
import qualified Specs.Aya.Registration.Core.Register.NominalCase as Register.Nominal.Case
import qualified Specs.Aya.Registration.Core.Update.NominalCase as Update.Nominal.Case
import Test.Tasty (defaultMain, testGroup)

import Cardano.Crypto.DSIGN

import Adapter.CardanoCryptoClass.Crypto
import Data.Aeson hiding (decode, decode', encode)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  keys <- fromJust <$> decodeFileStrict @[KeyPair Ed25519DSIGN] "tests/unit/data/generatedSubstrateEd25519Keys.json"
  setLocaleEncoding utf8
  defaultMain $
    testGroup
      "Validator Registration Plutus Contract - Unit Tests"
      [ testGroup
          "On Chain Specifications"
          [ testGroup
              "Nominal Cases"
              [ Register.Nominal.Case.specs keys
              , Update.Nominal.Case.specs keys
              ]
          , testGroup
              "Property 1 : Non Fungible Property Transitivity : EN Token is an NFT => the ENOP Token should be an NFT"
              [ Property.NFT.Transitivity.Register.specs keys
              , Property.NFT.Transitivity.Update.specs keys
              ]
              -- , Property.NFT.Ownership.specs keys
              -- ,  Property.Datum.specs keys
          ]
      , testGroup
          "Off Chain"
          []
      ]
