{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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

{-# HLINT ignore "Use second" #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Adapter.Plutus.OnChain (
    propertyViolation,
    propertyViolationIfFalse,
    fromMaybe',
) where

import PlutusTx.Prelude

-- | This function is normally available in the PlutusTx.Maybe module
-- | But its behaviour is unexpected and so it has been redefined here

{-# INLINEABLE fromMaybe' #-}
fromMaybe' :: a -> Maybe a -> a
fromMaybe' d x = case x of {Nothing -> d;Just v  -> v}

{-# INLINEABLE propertyViolation #-}
propertyViolation :: BuiltinString -> a
propertyViolation = traceError

{-# INLINEABLE propertyViolationIfFalse #-}
propertyViolationIfFalse :: BuiltinString -> Bool -> Bool
propertyViolationIfFalse = traceIfFalse

