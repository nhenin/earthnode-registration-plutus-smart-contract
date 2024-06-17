{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Register.TxBuilding (
    register,
    -- Pushing Fungible Tokens instead of NFTs
    registerMintingAnENOPWithQuantityAbove1,
    registerWithanENNFTWithAQuantityAbove1,
    -- Varying the Quantity of NFTs
    registerWithoutAnENNFT,
    registerWithDifferentENandENOPNFTTokenNames,
    registerByGeneratingMoreThan1ENOPNFT,
    registerByGeneratingMoreThan1ENNFT,
    -- Varying the Signers
    registerAndMintToAnotherOperator,
    registerWith2WalletsSigning,
    registerWithoutSigning,
    -- Varying the datum
    registerWithInvalidDatumVerification,
    mintWithoutRegistrationScript,
    registerWithNoRegistrationDatum,
    registerWithHashedRegistrationDatum,
    registerMoreThanOneOperator,
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

import Adapter.CardanoCryptoClass.Crypto
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.List.NonEmpty qualified as NL
import Data.Text qualified as Text
import Model
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

registerMintingAnENOPWithQuantityAbove1 ::
    (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) =>
    (MonadBlockChain m) =>
    KeyPair a ->
    ENNFT ->
    Commission ->
    Wallet ->
    m ()
registerMintingAnENOPWithQuantityAbove1 keyPair ennft commission operator = do
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
                { txSkelMints = txSkelMintsFromList [(associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Mint, tokenName ennft, 2)]
                , txSkelLabel = Set.empty
                , txSkelOpts = def{txOptEnsureMinAda = True}
                , txSkelValidityRange = Api.always
                , txSkelSigners = [operator]
                , txSkelIns = Map.empty
                , txSkelInsReference = Set.empty
                , txSkelOuts =
                    [ paysPK
                        operator
                        (V3.singleton (associatedENOPNFTCurrencySymbol settings) (tokenName ennft) 2)
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
    return ()

registerWithoutAnENNFT ::
    (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) =>
    (MonadBlockChain m) =>
    KeyPair a ->
    ENNFT ->
    Commission ->
    Wallet ->
    m ENOPNFT
registerWithoutAnENNFT keyPair ennft commission operator = do
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
                        (ada 3) -- withtout the ENNFT
                    ]
                }
    return $
        NFT
            (associatedENOPNFTCurrencySymbol settings)
            (tokenName ennft)

registerWithanENNFTWithAQuantityAbove1 ::
    (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) =>
    (MonadBlockChain m) =>
    KeyPair a ->
    ENNFT ->
    Quantity ->
    Commission ->
    Wallet ->
    m ENOPNFT
registerWithanENNFTWithAQuantityAbove1 keyPair ennft ennftQuantityAbove1 commission operator = do
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
                        (V3.singleton (currencySymbol ennft) (tokenName ennft) ennftQuantityAbove1)
                    ]
                }
    return $
        NFT
            (associatedENOPNFTCurrencySymbol settings)
            (tokenName ennft)

registerByGeneratingMoreThan1ENNFT ::
    (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) =>
    (MonadBlockChain m) =>
    KeyPair a ->
    (CurrencySymbol, TokenName, NL.NonEmpty TokenName) ->
    Commission ->
    Wallet ->
    m ENOPNFT
registerByGeneratingMoreThan1ENNFT keyPair (ennftCurrencySymbol, firstEnnftTn, ennftTns) commission operator = do
    let settings = coerce ennftCurrencySymbol
        signedMessage =
            sign
                (signatureKey keyPair)
                ( fromBuiltin $
                    mkHashedRegistrationMessage
                        firstEnnftTn
                        (walletPKHash operator)
                        commission
                        (associatedENOPNFTCurrencySymbol settings)
                )
    _ <-
        validateTxSkel $
            txSkelTemplate
                { txSkelMints = txSkelMintsFromList [(associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Mint, firstEnnftTn, 1)]
                , txSkelLabel = Set.empty
                , txSkelOpts = def{txOptEnsureMinAda = True}
                , txSkelValidityRange = Api.always
                , txSkelSigners = [operator]
                , txSkelIns = Map.empty
                , txSkelInsReference = Set.empty
                , txSkelOuts =
                    [ paysPK
                        operator
                        (V3.singleton (associatedENOPNFTCurrencySymbol settings) firstEnnftTn 1)
                    , paysScriptInlineDatum
                        (typedRegistrationValidator settings)
                        ( RegistrationDatum
                            { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                            , signature = toBuiltin signedMessage
                            , ennftTokenName = firstEnnftTn
                            , cardanoRewardPubKey = walletPKHash operator
                            , commission = commission
                            , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                            }
                        )
                        (foldMap (\tn -> V3.singleton ennftCurrencySymbol tn 1) (ennftTns <> NL.fromList [firstEnnftTn]))
                    ]
                }
    return $
        NFT
            (associatedENOPNFTCurrencySymbol settings)
            firstEnnftTn

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
        NFT (associatedENOPNFTCurrencySymbol settings)
            <$> (tokenName ennft : differentTokenNames)

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

registerWith2WalletsSigning ::
    (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) =>
    (MonadBlockChain m) =>
    KeyPair a ->
    ENNFT ->
    Commission ->
    Wallet ->
    Wallet ->
    m ENOPNFT
registerWith2WalletsSigning keyPair ennft commission operator anotherWallet = do
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
                , txSkelSigners = [operator, anotherWallet]
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

registerWithoutSigning ::
    (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) =>
    (MonadBlockChain m) =>
    KeyPair a ->
    ENNFT ->
    Commission ->
    Wallet ->
    m ENOPNFT
registerWithoutSigning keyPair ennft commission operator = do
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
                , txSkelSigners = []
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
                        (typedRegistrationValidator settings)
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

registerWithInvalidDatumVerification ::
    (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) =>
    (MonadBlockChain m) =>
    KeyPair a ->
    ENNFT ->
    Commission ->
    Wallet ->
    ByteString ->
    m ENOPNFT
registerWithInvalidDatumVerification keyPair ennft commission operator invalidSignature = do
    let settings = coerce . currencySymbol $ ennft
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
                            , signature = toBuiltin invalidSignature
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

registerWithNoRegistrationDatum ::
    (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) =>
    (MonadBlockChain m) =>
    KeyPair a ->
    ENNFT ->
    Commission ->
    Wallet ->
    m ENOPNFT
registerWithNoRegistrationDatum keyPair ennft commission operator = do
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
                    , paysScriptNoDatum
                        (typedRegistrationValidator settings)
                        (V3.singleton (currencySymbol ennft) (tokenName ennft) 1)
                    ]
                }
    return $
        NFT
            (associatedENOPNFTCurrencySymbol settings)
            (tokenName ennft)

registerWithHashedRegistrationDatum ::
    (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) =>
    (MonadBlockChain m) =>
    KeyPair a ->
    ENNFT ->
    Commission ->
    Wallet ->
    m ENOPNFT
registerWithHashedRegistrationDatum keyPair ennft commission operator = do
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
                    , paysScriptDatumHash
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

mintWithoutRegistrationScript ::
    (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) =>
    (MonadBlockChain m) =>
    KeyPair a ->
    ENNFT ->
    Wallet ->
    m ENOPNFT
mintWithoutRegistrationScript keyPair ennft operator = do
    let settings = coerce . currencySymbol $ ennft
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
                    , paysPK
                        operator
                        (V3.singleton (currencySymbol ennft) (tokenName ennft) 1)
                    ]
                }
    return $
        NFT
            (associatedENOPNFTCurrencySymbol settings)
            (tokenName ennft)

registerMoreThanOneOperator ::
    (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) =>
    (MonadBlockChain m) =>
    KeyPair a ->
    (CurrencySymbol, TokenName, TokenName) ->
    Commission ->
    Wallet ->
    m (ENOPNFT, ENOPNFT)
registerMoreThanOneOperator keyPair (currencySymbolEnnfts, ennft1Tn, ennft2Tn) commission operator = do
    let settings = coerce currencySymbolEnnfts
        signedMessage1 =
            sign
                (signatureKey keyPair)
                ( fromBuiltin $
                    mkHashedRegistrationMessage
                        ennft1Tn
                        (walletPKHash operator)
                        commission
                        (associatedENOPNFTCurrencySymbol settings)
                )
        signedMessage2 =
            sign
                (signatureKey keyPair)
                ( fromBuiltin $
                    mkHashedRegistrationMessage
                        ennft2Tn
                        (walletPKHash operator)
                        commission
                        (associatedENOPNFTCurrencySymbol settings)
                )
    _ <-
        validateTxSkel $
            txSkelTemplate
                { txSkelMints =
                    txSkelMintsFromList
                        [ (associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Mint, ennft1Tn, 1)
                        , (associatedENOPNFTMonetaryPolicy settings, SomeMintsRedeemer Mint, ennft2Tn, 1)
                        ]
                , txSkelLabel = Set.empty
                , txSkelOpts = def{txOptEnsureMinAda = True}
                , txSkelValidityRange = Api.always
                , txSkelSigners = [operator]
                , txSkelIns = Map.empty
                , txSkelInsReference = Set.empty
                , txSkelOuts =
                    [ paysPK
                        operator
                        (V3.singleton (associatedENOPNFTCurrencySymbol settings) ennft1Tn 1)
                    , paysPK
                        operator
                        (V3.singleton (associatedENOPNFTCurrencySymbol settings) ennft2Tn 1)
                    , paysScriptInlineDatum
                        (typedRegistrationValidator settings)
                        ( RegistrationDatum
                            { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                            , signature = toBuiltin signedMessage1
                            , ennftTokenName = ennft1Tn
                            , cardanoRewardPubKey = walletPKHash operator
                            , commission = commission
                            , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                            }
                        )
                        (V3.singleton currencySymbolEnnfts ennft1Tn 1)
                    , paysScriptInlineDatum
                        (typedRegistrationValidator settings)
                        ( RegistrationDatum
                            { ayaValidatorPublicKey = toBuiltin . toByteString . verificationKey $ keyPair
                            , signature = toBuiltin signedMessage2
                            , ennftTokenName = ennft2Tn
                            , cardanoRewardPubKey = walletPKHash operator
                            , commission = commission
                            , enopNFTCurrencySymbol = associatedENOPNFTCurrencySymbol settings
                            }
                        )
                        (V3.singleton currencySymbolEnnfts ennft2Tn 1)
                    ]
                }
    return
        ( NFT
            (associatedENOPNFTCurrencySymbol settings)
            ennft1Tn
        , NFT
            (associatedENOPNFTCurrencySymbol settings)
            ennft2Tn
        )
