{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OffChain.Actions (
    ENNFT (..),
    ENOPNFT (..),
    NFT (..),
    PartnerChainValidatorSettings (..),
    register,
) where

import Cardano.Api qualified as Cardano
import Cardano.Node.Emulator qualified as Emulator
import Control.Monad (void)
import Cooked
import Cooked.Output
import Cooked.Pretty.Class
import Cooked.ValueUtils
import Cooked.Wallet
import Data.Default
import Data.Function
import Data.List (foldl')
import Data.List.NonEmpty qualified as NEList
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import ENOPNFT.Validator
import Plutus.Script.Utils.Scripts qualified as Script

import Control.Monad.IO.Class (MonadIO (..))
import Data.Coerce (coerce)
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V3 (BuiltinByteString, CurrencySymbol, PubKeyHash, TokenName, toBuiltinData)
import PlutusLedgerApi.V3 qualified as Api
import PlutusLedgerApi.V3 qualified as V3
import Script
import Validator (ENNFTCurrencySymbol (..), RegistrationDatum (..))

data NFT = NFT {currencySymbol :: CurrencySymbol, tokenName :: TokenName} deriving (Eq, Show)
type ENNFT = NFT
type ENOPNFT = NFT

data PartnerChainValidatorSettings = PartnerChainValidatorSettings
    { operatorAddress :: BuiltinByteString
    -- ^ Owner Account which is operating the Aya Validator on the Aya chain
    , enCommission :: Integer
    -- ^ Commission in percent shared with staking delegators.
    }
    deriving (Show)

register ::
    (MonadBlockChain m) =>
    PartnerChainValidatorSettings ->
    ENNFT ->
    Wallet ->
    m ENOPNFT
register PartnerChainValidatorSettings{operatorAddress = givenOperatorAddress} ennft wallet = do
    let settings = coerce . currencySymbol $ ennft
    _ <-
        validateTxSkel $
            txSkelTemplate
                { txSkelMints = txSkelMintsFromList [(associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Mint, tokenName ennft, 1)]
                , txSkelLabel = Set.empty
                , txSkelOpts = def{txOptEnsureMinAda = True}
                , txSkelValidityRange = Api.always
                , txSkelSigners = [wallet]
                , txSkelIns = Map.empty
                , txSkelInsReference = Set.empty
                , txSkelOuts =
                    [ paysPK
                        wallet
                        (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName ennft) 1)
                    , paysScriptInlineDatum
                        (typedScript settings)
                        ( RegistrationDatum
                            { rewardWallet = walletPKHash wallet
                            , operatorAddress = givenOperatorAddress
                            , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                            , ennftTokenName = tokenName ennft
                            , enCommission = 1
                            , datumSigned = givenOperatorAddress
                            }
                        )
                        (V3.singleton (currencySymbol ennft) (tokenName ennft) 1)
                    ]
                }
    return $
        NFT
            (associatedENOPNFTCurrencySymbol settings)
            (tokenName ennft)

instance PrettyCooked Action where
    prettyCookedOpt opts Mint = "Mint ENNOPNFT"
    prettyCookedOpt opts Burn = "Burn ENNOPNFT"

instance PrettyCooked RegistrationDatum where
    prettyCookedOpt opts RegistrationDatum{..} = "RegistrationDatum TBD"
