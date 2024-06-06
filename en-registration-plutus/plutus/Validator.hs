{-
  Author   : Torben Poguntke
  Company  : World Mobile Group
  Copyright: 2023
  Version  : v2.0
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Validator (
    untypedValidator,
    validator,
    checkRegistrationSignature,
    mkHashedRegistrationMessage,
    ENNFTCurrencySymbol (..),
    RegistrationDatum (..),
    RegistrationAction (..),
) where

import GHC.Generics (Generic)

import Plutus.Script.Utils.Value
import PlutusLedgerApi.V3
import PlutusTx qualified
import PlutusTx.Prelude as Plutus.Prelude
import Prelude qualified

import PlutusTx.Builtins qualified as Cryptography

import Plutus.Script.Utils.V3.Contexts

newtype ENNFTCurrencySymbol = ENNFTCurrencySymbol
    { ennftCurrencySymbol :: CurrencySymbol
    }
    deriving (Prelude.Show, Generic, Ord)

instance Eq ENNFTCurrencySymbol where
    {-# INLINEABLE (==) #-}
    ENNFTCurrencySymbol x == ENNFTCurrencySymbol y = x == y

PlutusTx.unstableMakeIsData ''ENNFTCurrencySymbol
PlutusTx.makeLift ''ENNFTCurrencySymbol

data RegistrationDatum = RegistrationDatum
    { ayaValidatorPublicKey :: BuiltinByteString
    -- ^ Substrate Public Keys of the Aya Validator
    , signature :: BuiltinByteString
    -- ^ Signature of the datum. All datum fields below concatenated and signed by the substrateAVPublicKey
    , ennftTokenName :: TokenName
    -- ^ Unique ENNFT name, "used" for this registration
    , cardanoRewardPubKey :: PubKeyHash
    -- ^ Operator's wallet where rewards will be delivered after participating in a block production in Aya
    , commission :: Integer
    -- ^ Commission in percent shared with staking delegators.
    , enopNFTCurrencySymbol :: CurrencySymbol -- We cannot store the EnOpNft CurrencySymbol in the parameter because we get a cyclic dependency
    }
    deriving (Prelude.Show, Generic, Ord)

instance Eq RegistrationDatum where
    {-# INLINEABLE (==) #-}
    x == y =
        ayaValidatorPublicKey x
            == ayaValidatorPublicKey y
            && ennftTokenName x
            == ennftTokenName y
            && cardanoRewardPubKey x
            == cardanoRewardPubKey y
            && commission x
            == commission y
            && enopNFTCurrencySymbol x
            == enopNFTCurrencySymbol y
            && signature x
            == signature y

PlutusTx.makeIsDataIndexed ''RegistrationDatum [('RegistrationDatum, 0)]
PlutusTx.makeLift ''RegistrationDatum

-- {-# INLINABLE checkDatumSig #-}
-- Check the signature of the registration datum
checkRegistrationSignature :: RegistrationDatum -> Bool
checkRegistrationSignature RegistrationDatum{..} =
    Cryptography.verifyEd25519Signature
        ayaValidatorPublicKey
        (mkHashedRegistrationMessage ennftTokenName cardanoRewardPubKey commission enopNFTCurrencySymbol)
        signature

{-# INLINEABLE mkHashedRegistrationMessage #-}
mkHashedRegistrationMessage :: TokenName -> PubKeyHash -> Integer -> CurrencySymbol -> BuiltinByteString
mkHashedRegistrationMessage ennftTokenName cardanoRewardPubKey commission enopNFTCurrencySymbol =
    Cryptography.blake2b_256
        . appendByteString (unTokenName ennftTokenName)
        . appendByteString (getPubKeyHash cardanoRewardPubKey)
        . consByteString commission
        $ unCurrencySymbol enopNFTCurrencySymbol

{- | The actions that can be performed by the operator
| N.B : The action Register is enforced by the ENOPNFT NFT minting policy
-}
data RegistrationAction
    = -- | Unregister the operator
      Unregister
    | -- | Update the registration information
      Update BuiltinByteString
    deriving (Prelude.Show, Generic, Prelude.Eq, Prelude.Ord)

PlutusTx.makeIsDataIndexed ''RegistrationAction [('Unregister, 0), ('Update, 1)]
PlutusTx.makeLift ''RegistrationAction

{-- updateRegistration --}
-- ENOs can update the registration information, for that the signature of the current and the new datum is checked
-- it is also checked if the correct ENOP-NFT is spent in this transaction.
-- is must be ensured that the enUsedNftTn and pEnOpCs never change, this can only be done by performing a new registration
{-# INLINEABLE updateRegistration #-}
updateRegistration :: ENNFTCurrencySymbol -> BuiltinByteString -> ScriptContext -> Bool
updateRegistration ENNFTCurrencySymbol{..} _ ctx
    -- \| checkDatumSig nDat, -- new datum signature is verifued
    -- checkDatumSig cDat, -- old datum signature is verified
    -- newDatumSigned, -- signature check of new datum by old key
    | hasEnOpNft ov cDat -- EnOpNFT is in the new output UTxO
    , hasEnOpNft iv cDat -- EnOpNFT is spent and present in input UTxO
    , hasEnNft cv (ennftTokenName cDat) -- ENNFT is in inputs
    , hasEnNft nv (ennftTokenName cDat) -- ENNFT is in smart contract output
    , opOk nDat cDat =
        True -- The Tokenname and Policy of the ENOPNFT did not change
    | otherwise = False
  where
    -- Make sure the ENOP-NFT is spent
    hasEnOpNft :: Value -> RegistrationDatum -> Bool
    hasEnOpNft v d = valueOf v (enopNFTCurrencySymbol d) (ennftTokenName d) == 1

    -- Make sure ENNFT is in input and output
    hasEnNft :: Value -> TokenName -> Bool
    hasEnNft v t = valueOf v ennftCurrencySymbol t == 1

    -- Make sure the TokenName and CurrencySymbol don't change
    opOk :: RegistrationDatum -> RegistrationDatum -> Bool
    opOk d1 d2 = enopNFTCurrencySymbol d1 == enopNFTCurrencySymbol d2 && ennftTokenName d1 == ennftTokenName d2

    -- Check if the new datum was signed by the current consensus public key, the signature must be provided by the redeemer
    -- newDatumSigned = BI.verifySchnorrSecp256k1Signature (enOperatorAddress cDat) (makeMessage nDat) sig

    -- Input Value
    iv = valueSpent info

    -- Output Value
    ov = valueProduced info

    -- Construct the EnRegistration Datum
    nDat = makeEnRegDatum nd
    cDat = makeEnRegDatum cd

    -- get the OutputDatum (New Datum) and Value
    (nd, nv) = getRegOutputDatum (scriptOutputsAt (ownHash ctx) info)
    -- Extract Datum and Value from Output UTxO
    getRegOutputDatum :: [(OutputDatum, Value)] -> (Datum, Value)
    getRegOutputDatum [(OutputDatum nd', nv')] = (nd', nv')
    getRegOutputDatum _ = traceError "wrongOutputDatum"

    -- get the InputDatum (Current Datum) and Value
    (cd, cv) = case findOwnInput ctx of
        Just i -> extractTxO $ txInInfoResolved i
        Nothing -> traceError "CouldNotFindOwnInput"
    -- Extract Datum and Value from Input UTxO
    extractTxO :: TxOut -> (Datum, Value)
    extractTxO
        TxOut
            { txOutAddress = Address (ScriptCredential _) _
            , txOutValue
            , txOutDatum = OutputDatum cd'
            , txOutReferenceScript = Nothing
            } = (cd', txOutValue)
    extractTxO _ = traceError "wrongInputDatum"

    -- make ENRegistration from Datum
    makeEnRegDatum :: Datum -> RegistrationDatum
    makeEnRegDatum = unsafeFromBuiltinData . getDatum

    info = scriptContextTxInfo ctx

{-# INLINEABLE validateUnregister #-}
validateUnregister :: ENNFTCurrencySymbol -> RegistrationDatum -> ScriptContext -> Bool
validateUnregister ENNFTCurrencySymbol{..} RegistrationDatum{..} ctx
    -- No UTxO's to the script are allowed
    | noScriptOutputs $ txInfoOutputs info
    , -- the ENOPNFT must be burnt in this transaction
      isEnOPNftBurnt
    , -- The ENNFT is spent in this transaction
      valueOf (valueSpent info) ennftCurrencySymbol ennftTokenName == 1 =
        True
    | otherwise = False
  where
    info = scriptContextTxInfo ctx
    -- make sure the EnOpNFT is burnt
    isEnOPNftBurnt :: Bool
    isEnOPNftBurnt = valueOf (txInfoMint info) enopNFTCurrencySymbol ennftTokenName == -1

    noScriptOutputs :: [TxOut] -> Bool
    noScriptOutputs [] = True
    noScriptOutputs (h : t) =
        let
            checkInput :: TxOut -> Bool
            checkInput TxOut{txOutAddress = Address (ScriptCredential _) _} = False
            checkInput _ = True
         in
            checkInput h && noScriptOutputs t

{-# INLINEABLE validator #-}
validator :: ENNFTCurrencySymbol -> RegistrationDatum -> RegistrationAction -> ScriptContext -> Bool
validator sp d Unregister ctx = validateUnregister sp d ctx
validator sp _ (Update bs) ctx = updateRegistration sp bs ctx

{-# INLINEABLE untypedValidator #-}
untypedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedValidator s d r c =
    wVal validator (unsafeFromBuiltinData s) (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData c)

{-# INLINEABLE wVal #-}
wVal ::
    forall s d r c.
    (UnsafeFromData s, UnsafeFromData d, UnsafeFromData r, UnsafeFromData c) =>
    (s -> d -> r -> c -> Bool) ->
    BuiltinData ->
    BuiltinData ->
    BuiltinData ->
    BuiltinData ->
    ()
wVal f s d r c = check (f (unsafeFromBuiltinData s) (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData c))
