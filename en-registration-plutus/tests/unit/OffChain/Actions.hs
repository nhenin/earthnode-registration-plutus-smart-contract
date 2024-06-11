{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module OffChain.Actions (
    ENNFT (..),
    ENOPNFT (..),
    NFT (..),
    Commission,
    register,
    registerWithDifferentENandENOPNFTTokenNames,
    registerByGeneratingMoreThan1ENOPNFT,
    registerAndMintToAnotherOperator,
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
import ENOPNFT.Validator (Action (..))
import Plutus.Script.Utils.Scripts qualified as Script

import Adapter.CardanoCryptoClass.Crypto (DSIGNAlgorithm (..), Ed25519DSIGN, KeyPair (signatureKey, verificationKey), ToByteString (toByteString), sign)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V3 (BuiltinByteString, CurrencySymbol, PubKeyHash, TokenName, fromBuiltin, toBuiltin, toBuiltinData)
import PlutusLedgerApi.V3 qualified as Api
import PlutusLedgerApi.V3 qualified as V3
import Script (
    associatedENOPNFTCurrencySymbol,
    associatedENOPNFTMonetaryPolicy,
    typedScript,
 )
import Validator (ENNFTCurrencySymbol (..), RegistrationDatum (..), mkHashedRegistrationMessage)

data NFT = NFT {currencySymbol :: CurrencySymbol, tokenName :: TokenName} deriving (Eq, Show)
type ENNFT = NFT
type ENOPNFT = NFT
type Commission = Integer

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
                        (typedScript settings)
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

{- | Register by generating more than 1 NFT
| This function is used to test the failure case when more than 1 ENOP NFT is minted
| 1 ENOP NFT have the same token name as the ENNFT but the others don't
-}
registerByGeneratingMoreThan1ENOPNFT ::
    (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) =>
    (MonadBlockChain m) =>
    KeyPair a ->
    ENNFT ->
    [TokenName] ->
    Commission ->
    Wallet ->
    m [ENOPNFT]
registerByGeneratingMoreThan1ENOPNFT keyPair ennft differentTokenNames commission operator = do
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
                { txSkelMints =
                    txSkelMintsFromList $
                        (associatedENOPNFTMonetaryPolicy settings,SomeMintsRedeemer Mint,,1)
                            <$> (tokenName ennft : differentTokenNames)
                , txSkelLabel = Set.empty
                , txSkelOpts = def{txOptEnsureMinAda = True}
                , txSkelValidityRange = Api.always
                , txSkelSigners = [operator]
                , txSkelIns = Map.empty
                , txSkelInsReference = Set.empty
                , txSkelOuts =
                    [ paysPK
                        operator
                        ( nfts
                            (associatedENOPNFTCurrencySymbol settings)
                            (tokenName ennft : differentTokenNames)
                        )
                    , paysScriptInlineDatum
                        (typedScript settings)
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
        NFT (associatedENOPNFTCurrencySymbol settings)
            <$> (tokenName ennft : differentTokenNames)

-- | Make a 'Value' containing only the given quantity of the given currency.
nfts :: CurrencySymbol -> [TokenName] -> V3.Value
nfts c tns = V3.Value $ PMap.singleton c (PMap.fromList $ (,1) <$> tns)

registerAndMintToAnotherOperator ::
    (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) =>
    (MonadBlockChain m) =>
    KeyPair a ->
    ENNFT ->
    Commission ->
    Wallet ->
    Wallet ->
    m ENOPNFT
registerAndMintToAnotherOperator keyPair ennft commission operator anotherOperator = do
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
                        anotherOperator
                        (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName ennft) 1)
                    , paysScriptInlineDatum
                        (typedScript settings)
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

registerWithDifferentENandENOPNFTTokenNames ::
    (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) =>
    (MonadBlockChain m) =>
    KeyPair a ->
    ENNFT ->
    TokenName ->
    Commission ->
    Wallet ->
    m ENOPNFT
registerWithDifferentENandENOPNFTTokenNames keyPair ennft enoptTokenName commission wallet = do
    let settings = coerce . currencySymbol $ ennft
        signedMessage =
            sign
                (signatureKey keyPair)
                ( fromBuiltin $
                    mkHashedRegistrationMessage
                        (tokenName ennft)
                        (walletPKHash wallet)
                        commission
                        (associatedENOPNFTCurrencySymbol settings)
                )
    _ <-
        validateTxSkel $
            txSkelTemplate
                { txSkelMints = txSkelMintsFromList [(associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Mint, enoptTokenName, 1)]
                , txSkelLabel = Set.empty
                , txSkelOpts = def{txOptEnsureMinAda = True}
                , txSkelValidityRange = Api.always
                , txSkelSigners = [wallet]
                , txSkelIns = Map.empty
                , txSkelInsReference = Set.empty
                , txSkelOuts =
                    [ paysPK
                        wallet
                        (V3.singleton (associatedENOPNFTCurrencySymbol settings) enoptTokenName 1)
                    , paysScriptInlineDatum
                        (typedScript settings)
                        ( RegistrationDatum
                            { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                            , signature = toBuiltin signedMessage
                            , ennftTokenName = tokenName ennft
                            , cardanoRewardPubKey = walletPKHash wallet
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

instance PrettyCooked Action where
    prettyCookedOpt opts Mint = "Mint ENNOPNFT"
    prettyCookedOpt opts Burn = "Burn ENNOPNFT"

instance PrettyCooked RegistrationDatum where
    prettyCookedOpt opts RegistrationDatum{..} = "RegistrationDatum TBD"
