{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Update.TxBuilding (
    ENNFT (..),
    ENOPNFT (..),
    NFT (..),
    Commission,
    register,
) where

import Cardano.Api qualified as Cardano
import Cardano.Node.Emulator qualified as Emulator
import Control.Monad (void)
import Cooked

import Data.Default (Default (def))

import Data.List (foldl')
import Data.List.NonEmpty qualified as NEList
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEMap
import PlutusTx.AssocMap qualified as PMap

import Data.Set (Set)
import Data.Set qualified as Set
import ENOPNFT.OnChainMonetaryPolicy (Action (..))
import Plutus.Script.Utils.Scripts qualified as Script

import Adapter.CardanoCryptoClass.Crypto (Codec (decode, encode), DSIGNAlgorithm (..), Ed25519DSIGN, FromByteString (fromByteString), Hexadecimal, KeyPair (signatureKey, verificationKey), ToByteString (toByteString), sign)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.List.NonEmpty qualified as NL
import Data.Text qualified as Text
import Model (Commission, ENNFT, ENOPNFT, NFT (..))
import OnChainRegistrationValidator (ENNFTCurrencySymbol (..), RegistrationDatum (..), mkHashedRegistrationMessage)
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V3 (BuiltinByteString, CurrencySymbol, PubKeyHash, TokenName, fromBuiltin, toBuiltin, toBuiltinData)
import PlutusLedgerApi.V3 qualified as Api
import PlutusLedgerApi.V3 qualified as V3
import Prettyprinter.Extras (Pretty (..))
import RegistrationValidator (
    associatedENOPNFTCurrencySymbol,
    associatedENOPNFTMonetaryPolicy,
    typedRegistrationValidator,
 )

register ::
    (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) =>
    (MonadBlockChain m) =>
    KeyPair a ->
    ENNFT ->
    Commission ->
    Wallet ->
    m ENOPNFT
register keyPair ennft commission operator = do
    let settings = coerce . currencySymbol $ ennft
        signedMessage =
            sign
                (signatureKey keyPair)
                ( fromBuiltin $
                    mkHashedRegistrationMessage
                        (tokenName ennft)
                        (walletPKHash operator)
                        commission
                        (associatedENOPNFTCurrencySymbol settings)
                )
    _ <-
        validateTxSkel $
            txSkelTemplate
                { txSkelMints = txSkelMintsFromList [(associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Mint, tokenName ennft, 1)]
                , txSkelLabel = Set.empty
                , txSkelOpts = def{txOptEnsureMinAda = True}
                , txSkelValidityRange = Api.always
                , txSkelSigners = [operator]
                , txSkelIns = Map.empty
                , txSkelInsReference = Set.empty
                , txSkelOuts =
                    [ paysPK
                        operator
                        (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName ennft) 1)
                    , paysScriptInlineDatum
                        (typedRegistrationValidator settings)
                        ( RegistrationDatum
                            { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                            , signature = toBuiltin signedMessage
                            , ennftTokenName = tokenName ennft
                            , cardanoRewardPubKey = walletPKHash operator
                            , commission = commission
                            , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                            }
                        )
                        (V3.singleton (currencySymbol ennft) (tokenName ennft) 1)
                    ]
                }
    return $
        NFT
            (associatedENOPNFTCurrencySymbol settings)
            (tokenName ennft)
