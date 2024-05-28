module Main (
    main,
) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import qualified Spec.NominalCaseSpec as Validator.NominalCases
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
    setLocaleEncoding utf8
    defaultMain $
        testGroup
            "Validator Registration Plutus Contract - Unit Tests"
            [ testGroup
                "On Chain"
                [ Validator.NominalCases.specs
                ]
            , testGroup
                "Off Chain"
                []
            ]
