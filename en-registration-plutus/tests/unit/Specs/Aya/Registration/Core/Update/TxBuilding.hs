{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Specs.Aya.Registration.Core.Update.TxBuilding (
  update,
) where

import Cooked

import Data.Default (Default (def))

import Data.Map qualified as Map
import PlutusTx.AssocMap qualified as PMap

import Data.Set qualified as Set

import Adapter.CardanoCryptoClass.Crypto (
  DSIGNAlgorithm (..),
  KeyPair (signatureKey, verificationKey),
  ToByteString (toByteString),
  sign,
 )
import Aya.Registration.Core.Validator.Builder (
  associatedENOPNFTCurrencySymbol,
  registrationValidatorAddress,
  typedRegistrationValidator,
 )
import Aya.Registration.Core.Validator.OnChain (
  RegistrationAction (..),
  RegistrationDatum (..),
  RegistrationValidatorSettings (..),
  mkHashedRegistrationMessage,
 )
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import PlutusLedgerApi.V3 (
  Value (..),
  fromBuiltin,
  toBuiltin,
 )
import PlutusLedgerApi.V3 qualified as Api
import PlutusLedgerApi.V3 qualified as V3
import PlutusTx.Maybe (isJust)
import Safe (headMay)
import Specs.Aya.Registration.Core.Model (Commission, ENOPNFT, NFT (..))

update
  :: (ContextDSIGN a ~ (), DSIGNAlgorithm a, Signable a ByteString)
  => (MonadBlockChain m)
  => KeyPair a
  -> (RegistrationValidatorSettings, ENOPNFT)
  -> Commission
  -> Wallet
  -> Wallet
  -> m (RegistrationValidatorSettings, ENOPNFT)
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

containsENNFT :: (RegistrationValidatorSettings, ENOPNFT) -> Value -> Bool
containsENNFT (settings, enopNFT) (Value v) =
  let currency = ennftCurrencySymbol settings
      tn = tokenName enopNFT
   in isJust (PMap.lookup currency v >>= PMap.lookup tn)
