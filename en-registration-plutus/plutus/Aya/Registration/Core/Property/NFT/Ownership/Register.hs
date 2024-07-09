{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Aya.Registration.Core.Property.NFT.Ownership.Register (
  -- `Property 2.0` : ENOP NFT should be given to the operator
  v_2_0_0_ENOP_NFT_Not_Output_To_Operator,
  -- `Property 2.1` : ENNFT should be output on script (Already Validated by - `1.0.2 violation` - No ENNFT on Registration validator output  )
  -- `Property 2.2` : Only the operator should sign the transaction
  -- `2.1.0 violation` - No signer found (Enforced by Ledger Properties)
  v_2_1_0_More_Than_One_Signer,
) where

import PlutusTx.Prelude (BuiltinString)

{-# INLINEABLE v_2_0_0_ENOP_NFT_Not_Output_To_Operator #-}
v_2_0_0_ENOP_NFT_Not_Output_To_Operator :: BuiltinString
v_2_0_0_ENOP_NFT_Not_Output_To_Operator = "2.0.0"

-- `Property 2.2` : Only the operator should sign the transaction
{-# INLINEABLE v_2_1_0_More_Than_One_Signer #-}
v_2_1_0_More_Than_One_Signer :: BuiltinString
v_2_1_0_More_Than_One_Signer = "2.1.0"
