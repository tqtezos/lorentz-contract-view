{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-do-bind #-}

module Lorentz.Contracts.ConstView where

import Lorentz

-- | A contract storing (and accepting) a single variable
-- of given type
constViewContract :: forall a. (NiceConstant a, NiceParameter a)
  => a
  -> Contract (View () a) ()
constViewContract x = do
  unpair
  view_ $ do
    drop
    push x

