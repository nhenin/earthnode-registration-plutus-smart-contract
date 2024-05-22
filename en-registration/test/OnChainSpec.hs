module OnChainSpec (
    spec,
) where

import Cooked ()
import qualified Plutus.Script.Utils.Ada as Pl
import qualified Test.Hspec as Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as Gen

import Test.Hspec

spec :: Spec
spec = do
    describe "strip" $ do
        it "removes leading and trailing whitespace" $ do
            "\t  foo bar\n" `shouldBe` "foo bar"
