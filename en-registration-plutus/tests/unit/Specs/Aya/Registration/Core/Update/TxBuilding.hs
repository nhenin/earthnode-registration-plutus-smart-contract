{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Specs.Aya.Registration.Core.Update.TxBuilding (
    update,
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

import Aya.Registration.Core.ENOPNFT.MonetaryPolicy.OnChain (Action (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Plutus.Script.Utils.Scripts qualified as Script

import Adapter.CardanoCryptoClass.Crypto (Codec (decode, encode), DSIGNAlgorithm (..), Ed25519DSIGN, FromByteString (fromByteString), Hexadecimal, KeyPair (signatureKey, verificationKey), ToByteString (toByteString), sign)
import Aya.Registration.Core.Validator.Builder (
    associatedENOPNFTCurrencySymbol,
    associatedENOPNFTMonetaryPolicy,
    registrationValidatorAddress,
    typedRegistrationValidator,
 )
import Aya.Registration.Core.Validator.OnChain (
    RegistrationAction (..),
    RegistrationDatum (..),
    RegistrationValidatorSettings (..),
    mkHashedRegistrationMessage,
 )
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.List.NonEmpty qualified as NL
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V3 (
    BuiltinByteString,
    CurrencySymbol,
    PubKeyHash,
    TokenName,
    Value (..),
    fromBuiltin,
    toBuiltin,
    toBuiltinData,
    unionWith,
 )
import PlutusLedgerApi.V3 qualified as Api
import PlutusLedgerApi.V3 qualified as V3
import PlutusTx.Maybe (isJust)
import Prettyprinter.Extras (Pretty (..))
import Safe (headMay)
import Specs.Aya.Registration.Core.Model (Commission, ENNFT, ENOPNFT, NFT (..))

update ::
    (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) =>
    (MonadBlockChain m) =>
    KeyPair a ->
    (RegistrationValidatorSettings, ENOPNFT) ->
    Commission ->
    Wallet ->
    Wallet ->
    m (RegistrationValidatorSettings, ENOPNFT)
update keyPair registratedItemId@(settings, enopNFT) newCommission operator newOperator = do
    registeredItemRef <-
        ( fst
                . fromMaybe (error "No ENNFT found on registration validator utxos.")
                . headMay
                <$>
            )
            . runUtxoSearch
            . flip filterWithPred (containsENNFT registratedItemId . outputValue)
            . utxosAtSearch
            $ registrationValidatorAddress settings

    let signedMessage =
            sign
                (signatureKey keyPair)
                ( fromBuiltin $
                    mkHashedRegistrationMessage
                        (tokenName enopNFT)
                        (walletPKHash newOperator)
                        newCommission
                        (associatedENOPNFTCurrencySymbol settings)
                )
    _ <-
        validateTxSkel $
            txSkelTemplate
                { txSkelMints = Map.empty
                , txSkelLabel = Set.empty
                , txSkelOpts = def{txOptEnsureMinAda = True}
                , txSkelValidityRange = Api.always
                , txSkelSigners = [operator]
                , txSkelIns = Map.singleton registeredItemRef (TxSkelRedeemerForScript UpdateRegistrationDetails)
                , txSkelInsReference = Set.empty
                , txSkelOuts =
                    [ paysPK
                        operator
                        (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName enopNFT) 1)
                    , paysScriptInlineDatum
                        (typedRegistrationValidator settings)
                        ( RegistrationDatum
                            { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                            , signature = toBuiltin signedMessage
                            , ennftTokenName = tokenName enopNFT
                            , cardanoRewardPubKey = walletPKHash newOperator
                            , commission = newCommission
                            , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                            }
                        )
                        (V3.singleton (ennftCurrencySymbol settings) (tokenName enopNFT) 1)
                    ]
                }
    return registratedItemId

getENNFT :: (RegistrationValidatorSettings, ENOPNFT) -> Value
getENNFT (settings, enopNFT) = V3.singleton (ennftCurrencySymbol settings) (tokenName enopNFT) 1

containsENNFT :: (RegistrationValidatorSettings, ENOPNFT) -> Value -> Bool
containsENNFT (settings, enopNFT) (Value v) =
    let currency = ennftCurrencySymbol settings
        tn = tokenName enopNFT
     in isJust (PMap.lookup currency v >>= PMap.lookup tn)
