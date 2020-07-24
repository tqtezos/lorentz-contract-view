{-# OPTIONS -Wno-missing-export-lists -Wno-unused-do-bind -Wno-orphans #-}

module Lorentz.Contracts.Util.ParameterEntryPoints where

import Lorentz
import Michelson.Typed.Scope
import Michelson.Typed.Instr
import Michelson.Typed.Value

import Prelude (id)
import Data.Typeable

import Data.Singletons

-- Lorentz.Contracts.Util
instance IsoValue (Value' Instr t) where
  type ToT (Value' Instr t) = t
  toVal = id
  fromVal = id

instance ParameterEntryPoints Natural where
  parameterEntryPoints = pepNone

instance ProperParameterBetterErrors t =>
         ParameterEntryPoints (Value' Instr t) where
  parameterEntryPoints = pepNone

instance ( NiceParameter a
         , HasNoOp (ToT a)
         , HasNoNestedBigMaps (ToT a)
         , SingI (ToT r)
         , Typeable (ToT r)
         , HasNoOp (ToT r)
         , HasNoNestedBigMaps (ToT r)
         ) =>
         ParameterEntryPoints (View a r) where
  parameterEntryPoints = pepNone

