{-
  Author   : Torben Poguntke
  Company  : World Mobile Group
  Copyright: 2023
  Version  : v2.0
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
-- Options
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}

module OnChain (
    vUt,
    mkVal,
    scriptOutputsAt,
) where

import Ledger (ValidatorHash (..))
import PlutusLedgerApi.V1.Value (valueOf)
import PlutusLedgerApi.V2
import qualified PlutusLedgerApi.V2 as PV2
import PlutusLedgerApi.V2.Contexts
import qualified PlutusTx.Builtins as BI
import PlutusTx.Prelude
import Types

{-- updateRegistration --}
-- ENOs can update the registration information, for that the signature of the current and the new datum is checked
-- it is also checked if the correct ENOP-NFT is spent in this transaction.
-- is must be ensured that the enUsedNftTn and pEnOpCs never change, this can only be done by performing a new registration
{-# INLINEABLE updateRegistration #-}
updateRegistration :: ScriptParams -> BuiltinByteString -> ScriptContext -> Bool
updateRegistration ScriptParams{..} sig ctx
    | checkDatumSig nDat -- new datum signature is verifued
    , checkDatumSig cDat -- old datum signature is verified
    , newDatumSigned -- signatures of new datum by old key
    , hasEnOpNft nv cDat -- EnOpNFT is in the new output UTxO
    , hasEnOpNft cv cDat -- EnOpNFT is spent and present in input UTxO
    , hasEnNft cv (enUsedNftTn cDat)
    , hasEnNft nv (enUsedNftTn cDat)
    , opOk nDat cDat =
        True -- The EnOpNFT is not changed in this update
    | otherwise = False
  where
    -- Make sure the ENOP-NFT is spent and contained in the new output
    hasEnOpNft :: Value -> EnRegistration -> Bool
    hasEnOpNft v d = valueOf v (pEnOpCs d) (enUsedNftTn d) == 1

    -- Make sure ENNFT is in input and output
    hasEnNft :: Value -> TokenName -> Bool
    hasEnNft v t = valueOf v pNftCs t == 1

    -- Make sure the TokenName and CurrencySymbol don't change
    opOk :: EnRegistration -> EnRegistration -> Bool
    opOk d1 d2 = pEnOpCs d1 == pEnOpCs d2 && enUsedNftTn d1 == enUsedNftTn d2

    -- Check if the new datum was signed by the current CceAddress, the signature must be provided by the redeemer
    newDatumSigned = BI.verifySchnorrSecp256k1Signature (enConsensusPubKey cDat) (makeMessage nDat) sig

    -- Construct the EnRegistration
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
    makeEnRegDatum :: Datum -> EnRegistration
    makeEnRegDatum = unsafeFromBuiltinData . getDatum

    info = scriptContextTxInfo ctx

{-# INLINEABLE checkDatumSig #-}
-- Check the signature of the registration datum
checkDatumSig :: EnRegistration -> Bool
checkDatumSig dat@EnRegistration{..} = BI.verifySchnorrSecp256k1Signature enConsensusPubKey (makeMessage dat) enSignature

{-# INLINEABLE makeMessage #-}
-- Make a signature message EnRegistrationDatum
makeMessage :: EnRegistration -> BuiltinByteString
makeMessage EnRegistration{..} = BI.blake2b_256 $ appendByteString (unCurrencySymbol pEnOpCs) $ consByteString enCommission $ appendByteString (getPubKeyHash enRwdWallet) $ appendByteString (unTokenName enUsedNftTn) $ appendByteString enCceAddress $ appendByteString enMerkleTreeRoot $ appendByteString enOperatorAddress enConsensusPubKey

{-# INLINEABLE validateUnregister #-}
validateUnregister :: ScriptParams -> EnRegistration -> ScriptContext -> Bool
validateUnregister ScriptParams{..} EnRegistration{..} ctx
    -- No UTxO's to the script are allowed
    | noScriptOutputs $ txInfoOutputs info
    , -- the ENOPNFT must be burnt in this transaction
      isEnOPNftBurnt
    , -- The ENNFT is spent in this transaction
      valueOf (valueSpent info) pNftCs enUsedNftTn == 1 =
        True
    | otherwise = False
  where
    info = scriptContextTxInfo ctx
    -- make sure the EnOpNFT is burnt
    isEnOPNftBurnt :: Bool
    isEnOPNftBurnt = valueOf (txInfoMint info) pEnOpCs enUsedNftTn == -1

    noScriptOutputs :: [TxOut] -> Bool
    noScriptOutputs [] = True
    noScriptOutputs (h : t) =
        let
            checkInput :: TxOut -> Bool
            checkInput TxOut{txOutAddress = Address (ScriptCredential _) _} = False
            checkInput _ = True
         in
            checkInput h && noScriptOutputs t

{-# INLINEABLE mkVal #-}
mkVal :: ScriptParams -> EnRegistration -> Action -> ScriptContext -> Bool
mkVal sp d Unregister ctx = validateUnregister sp d ctx
mkVal sp _ (Update bs) ctx = updateRegistration sp bs ctx

{-# INLINEABLE vUt #-}
vUt :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
vUt s d r c =
    wVal mkVal (unsafeFromBuiltinData s) (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData c)

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

{-# INLINEABLE scriptOutputsAt #-}

-- | Get the datums and values paid to an address of a validator by a pending transaction.
scriptOutputsAt :: ValidatorHash -> TxInfo -> [(PV2.OutputDatum, Value)]
scriptOutputsAt (ValidatorHash s) = outputsAt (PV2.Address (PV2.ScriptCredential (PV2.ScriptHash s)) Nothing)

{-# INLINEABLE outputsAt #-}

-- | Get the datums and values paid to an address by a pending transaction.
outputsAt :: Address -> TxInfo -> [(PV2.OutputDatum, Value)]
outputsAt addr p =
    let flt TxOut{txOutAddress, txOutValue, txOutDatum} | txOutAddress == addr = Just (txOutDatum, txOutValue)
        flt _ = Nothing
     in mapMaybe flt (txInfoOutputs p)

{-# INLINEABLE ownHashes #-}

-- | Get the validator and datum hashes of the output that is curently being validated
ownHashes :: ScriptContext -> (ValidatorHash, PV2.OutputDatum)
ownHashes
    ( findOwnInput ->
            Just
                TxInInfo
                    { txInInfoResolved =
                        TxOut{txOutAddress = PV2.Address (PV2.ScriptCredential (PV2.ScriptHash s)) _, txOutDatum = d}
                    }
        ) = (ValidatorHash s, d)
ownHashes _ = traceError "Lg" -- "Can't get validator and datum hashes"

{-# INLINEABLE ownHash #-}

-- | Get the hash of the validator script that is currently being validated.
ownHash :: ScriptContext -> ValidatorHash
ownHash p = fst (ownHashes p)
