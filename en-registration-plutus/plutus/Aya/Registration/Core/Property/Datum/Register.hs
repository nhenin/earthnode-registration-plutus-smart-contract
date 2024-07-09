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
-- Options
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

{-# HLINT ignore "Use camelCase" #-}

module Aya.Registration.Core.Property.Datum.Register (
  v_3_0_1_Registration_Datum_Field_ENNFT_TokenName_Not_Valid,
  v_3_0_2_Registration_Datum_Field_ENOP_NFT_Currency_Symbol_Not_Valid,
  v_3_1_Registration_Datum_Not_Deserializable,
  v_3_2_Registration_Validator_Datim_Not_Authentic,
  v_3_3_Registration_Validator_Output_NotFound,
  v_3_4_Registration_Validator_Has_No_Datum,
  v_3_5_Registration_Validator_Has_Only_Hashed_Datum,
  v_3_6_More_Than_1_Registration_Validator_Output,
) where

import PlutusTx.Prelude (BuiltinString)

{-# INLINEABLE v_3_0_1_Registration_Datum_Field_ENNFT_TokenName_Not_Valid #-}
{-# INLINEABLE v_3_0_2_Registration_Datum_Field_ENOP_NFT_Currency_Symbol_Not_Valid #-}
{-# INLINEABLE v_3_1_Registration_Datum_Not_Deserializable #-}
{-# INLINEABLE v_3_2_Registration_Validator_Datim_Not_Authentic #-}
{-# INLINEABLE v_3_3_Registration_Validator_Output_NotFound #-}
{-# INLINEABLE v_3_4_Registration_Validator_Has_No_Datum #-}
{-# INLINEABLE v_3_5_Registration_Validator_Has_Only_Hashed_Datum #-}
{-# INLINEABLE v_3_6_More_Than_1_Registration_Validator_Output #-}
v_3_0_1_Registration_Datum_Field_ENNFT_TokenName_Not_Valid
  , v_3_0_2_Registration_Datum_Field_ENOP_NFT_Currency_Symbol_Not_Valid
  , v_3_1_Registration_Datum_Not_Deserializable
  , v_3_2_Registration_Validator_Datim_Not_Authentic
  , v_3_3_Registration_Validator_Output_NotFound
  , v_3_4_Registration_Validator_Has_No_Datum
  , v_3_5_Registration_Validator_Has_Only_Hashed_Datum
  , v_3_6_More_Than_1_Registration_Validator_Output
    :: BuiltinString
v_3_0_1_Registration_Datum_Field_ENNFT_TokenName_Not_Valid = "3.0.1"
v_3_0_2_Registration_Datum_Field_ENOP_NFT_Currency_Symbol_Not_Valid = "3.0.2"
v_3_1_Registration_Datum_Not_Deserializable = "3.1"
v_3_2_Registration_Validator_Datim_Not_Authentic = "3.2"
v_3_3_Registration_Validator_Output_NotFound = "3.3"
v_3_4_Registration_Validator_Has_No_Datum = "3.4"
v_3_5_Registration_Validator_Has_Only_Hashed_Datum = "3.5"
v_3_6_More_Than_1_Registration_Validator_Output = "3.6"
