{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (
    main,
) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import qualified Specifications 
import Test.Tasty (defaultMain, testGroup)

import Cardano.Crypto.DSIGN
import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.DSIGN.SchnorrSecp256k1
import Cardano.Crypto.Hash (ByteString)
import qualified Data.List as T
import Data.Text as Text hiding (drop)

import Data.Either
import Data.String

import Adapter.CardanoCryptoClass.Crypto
import Data.Aeson hiding (decode, decode', encode)
import Data.Coerce (coerce)
import Data.Maybe (fromJust)
import GHC.Stack (HasCallStack)
import Text.Hex hiding (ByteString)

main :: IO ()
main = do
    keys <- fromJust <$> decodeFileStrict @[KeyPair Ed25519DSIGN] "tests/unit/data/generatedSubstrateEd25519Keys.json"
    setLocaleEncoding utf8
    defaultMain $
        testGroup
            "Validator Registration Plutus Contract - Unit Tests"
            [ testGroup
                "On Chain Specifications"
                [ Specifications.specs keys
                ]
            , testGroup
                "Off Chain"
                []
            ]
