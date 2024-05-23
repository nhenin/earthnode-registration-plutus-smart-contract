{-# LANGUAGE BangPatterns #-}
{-
  Author   : Torben Poguntke
  Company  : World Mobile Group
  Copyright: 2023
  Version  : v2.0
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fobject-code #-}

module Types where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger
import PlutusLedgerApi.V2
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude

newtype ScriptParams = ScriptParams
    { pNftCs :: CurrencySymbol
    }
    deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.unstableMakeIsData ''ScriptParams
PlutusTx.makeLift ''ScriptParams

data EnRegistration = EnRegistration
    { enOperatorAddress :: BuiltinByteString
    -- ^ Owner Account which is operating the Aya Validator on the Aya chain
    , enConsensusPubKey :: BuiltinByteString
    -- ^ Validator Pubkey of the consensus Node
    , enMerkleTreeRoot :: BuiltinByteString
    -- ^ MerkleRoot for verifiable randomness (this will disappear)
    , enCceAddress :: BuiltinByteString
    -- ^ CrossChain Pubkey which can be verified on both chains
    , enUsedNftTn :: TokenName
    -- ^ Unique ENNFT name, "used" for this registration
    , enRwdWallet :: PubKeyHash
    -- ^ Operator's wallet where rewards will be delivered after participating in a block production in Aya
    , enCommission :: Integer
    -- ^ Commission in percent shared with staking delegators.
    , pEnOpCs :: CurrencySymbol -- We cannot store the EnOpNft CurrencySymbol in the parameter because we get a cyclic dependency
    , enSignature :: BuiltinByteString
    -- ^ Signature of the datum. All datum fields concatenated and signed by the enCceAddress
    }
    deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeIsDataIndexed ''EnRegistration [('EnRegistration, 0)]
PlutusTx.makeLift ''EnRegistration

data Action = Unregister | Update BuiltinByteString
    deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)
PlutusTx.makeIsDataIndexed ''Action [('Unregister, 0), ('Update, 1)]
PlutusTx.makeLift ''Action
