{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Update.TxBuilding (
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
import Model ( Commission, ENNFT, ENOPNFT, NFT(..) )
import OnChainRegistrationValidator
    ( RegistrationValidatorSettings(..),
      RegistrationDatum(..),
      mkHashedRegistrationMessage,
      RegistrationAction(..) )
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V3 (BuiltinByteString, CurrencySymbol, PubKeyHash, TokenName, fromBuiltin, toBuiltin, toBuiltinData, Value)
import PlutusLedgerApi.V3 qualified as Api
import PlutusLedgerApi.V3 qualified as V3
import Prettyprinter.Extras (Pretty (..))
import RegistrationValidator
    ( associatedENOPNFTCurrencySymbol,
      associatedENOPNFTMonetaryPolicy,
      typedRegistrationValidator,
      registrationValidatorAddress )
import Data.Maybe (fromMaybe)
import Safe (headMay)

update ::
    (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString) =>
    (MonadBlockChain m) =>
    KeyPair a ->
    (RegistrationValidatorSettings,ENOPNFT) ->
    Commission ->
    Wallet ->
    Wallet ->
    m (RegistrationValidatorSettings,ENOPNFT)
update keyPair registratedItemId@(settings,enopNFT) newCommission operator newOperator= do
    registeredItemRef
        <- (fst
            . fromMaybe (error "No ENNFT found on registration validator utxos.")
            . headMay <$>)
            . runUtxoSearch
            . flip filterWithPred ((== getENNFT registratedItemId) . outputValue)
            . utxosAtSearch $ registrationValidatorAddress settings
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
                -- , txSkelIns = Map.singleton registeredItemRef (TxSkelRedeemerForScript UpdateRegistrationDetails)
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


getENNFT ::  (RegistrationValidatorSettings,ENOPNFT) -> Value
getENNFT (settings,enopNFT) = V3.singleton (ennftCurrencySymbol settings) (tokenName enopNFT) 1