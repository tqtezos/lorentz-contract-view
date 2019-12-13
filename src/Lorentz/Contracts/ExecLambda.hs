{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS -Wno-missing-export-lists -Wno-unused-do-bind -Wno-orphans #-}

module Lorentz.Contracts.ExecLambda where

import Lorentz
import Michelson.Text
import Michelson.Typed.Scope

import Lorentz.Contracts.Util.Address ()
import Lorentz.Contracts.Util.ParameterEntryPoints ()

import Data.Coerce
import Prelude (Enum(..))
import Data.Typeable
import Text.Show

import qualified Data.Text as T

-- contractAddr :: forall p s. NiceParameter p => Address -> s :-> ContractRef p & s
-- contractAddr contractAddr' = do
--   push contractAddr'
--   assertContract

---- | Push a value of @contract@ type.
----
---- Doing this via 'push' instruction is not possible, so we need to perform
---- extra actions here.
----
---- Aside from @contract@ value itself you will need to specify which error to
---- throw in case this value is not valid.
--pushContractRef
--  :: NiceParameter arg
--  => (forall s0. Address : s :-> s0)
--  -> ContractRef arg
--  -> (s :-> ContractRef arg : s)
--pushContractRef onContractNotFound (ContractRef addr sepc :: ContractRef arg) =
--  withDict (niceParameterEvi @arg) $
--    push addr # dup #
--    I (CONTRACT starNotes (sepcName sepc)) #
--    ifNone onContractNotFound (dip drop)


assertContract :: forall p s. NiceParameter p => Address & s :-> ContractRef p & s
assertContract = do
  contract @p
  assertSome . mkMTextUnsafe $
    "Not " <> T.pack (show $ typeRep $ Proxy @(ToT p))

callContractAddressNoMutez :: forall p s. NiceParameter p
  => ContractRef p
  -> p & s :-> [Operation] & s
callContractAddressNoMutez contractAddr' = do
  dip $ do
    -- contractAddr contractAddr'
    pushContractRef
      failWith
      contractAddr'
    push (toEnum 0 :: Mutez)
  transferTokens @p
  dip $ nil @Operation
  cons

---- | Push a value of @contract@ type.
----
---- Doing this via 'push' instruction is not possible, so we need to perform
---- extra actions here.
----
---- Aside from @contract@ value itself you will need to specify which error to
---- throw in case this value is not valid.
--pushContractRef
--  :: NiceParameter arg
--  => (forall s0. Address : s :-> s0)
--  -> ContractRef arg
--  -> (s :-> ContractRef arg : s)
--pushContractRef onContractNotFound (ContractRef addr sepc :: ContractRef arg) =
--  withDict (niceParameterEvi @arg) $
--    push addr # dup #
--    I (CONTRACT starNotes (sepcName sepc)) #
--    ifNone onContractNotFound (dip drop)


newtype Parameter
  = ExecLambda (Lambda () ([Operation], ()))
  deriving stock (Generic)
  deriving stock (Show)
  deriving anyclass (IsoValue)

instance ParameterEntryPoints Parameter where
  parameterEntryPoints = pepNone

execLambdaContract :: Contract Parameter ()
execLambdaContract = coerceParameter $ do
  car
  unit
  exec
  where
    coerceParameter :: Contract (Lambda () ([Operation], ())) () -> Contract Parameter ()
    coerceParameter = coerce

toView :: forall a r s. a & ContractRef r & s :-> View a r & s
toView = do
  pair
  coerce_ @(a, ContractRef r) @(View a r)

viewToVoidParameter :: forall a r. (NiceConstant a, NiceParameter a, NiceParameter r, ParameterEntryPoints r)
  => ContractRef Parameter
  -> ContractRef (View a r)
  -> a
  -> Parameter
viewToVoidParameter execLambdaAddr' viewContractAddr' viewParam' =
  forbiddenOp @(ToT a) $
  forbiddenNestedBigMaps @(ToT a) $
  callbackToFailWithParameter toParam' execLambdaAddr' viewContractAddr'
    where
      toParam' :: forall s. ContractRef r & s :-> View a r & s
      toParam' = do
        push @a viewParam'
        toView

-- | Given a method to convert a `ContractRef` of the appropriate type
-- to a parameter for some contract, produce a `Parameter` that
-- calls the contract and "returns" the result as an error.
callbackToFailWithParameter :: forall p a. (NiceParameter p, NiceParameter a, ParameterEntryPoints p)
  => (forall s. ContractRef p & s :-> a & s) -- ^ Convert a `ContractRef` to a parameter
  -> ContractRef Parameter             -- ^ The `execLambdaContract`
  -> ContractRef a                      -- ^ The contract accepting a callback
  -> Parameter
callbackToFailWithParameter toParam' execLambdaAddr' callingBackContractAddr' = ExecLambda $ do
  push (toEnum 0 :: Mutez)
  none @KeyHash
  createContract @p @() failWith
  dip $ do
    dip $ do
      lambda @('[(Address, ())]) @('[([Operation], ())]) $ do
        car
        assertContract @p
        toParam'
        callContractAddressNoMutez @a callingBackContractAddr'
        dip unit
        pair
    apply
    callContractAddressNoMutez @(Lambda () ([Operation], ())) $
      coerceContractRef @Parameter @(Lambda () ([Operation], ())) execLambdaAddr'
  cons
  dip unit
  pair

exampleParameter :: Parameter
exampleParameter = ExecLambda $ do
  push (toEnum 0 :: Mutez)
  none @KeyHash
  createContract @Natural @() failWith
  dip $ do
    dip $ do
      lambda @('[(Address, ())]) @('[([Operation], ())]) $ do
        car
        assertContract @Natural
        callContractAddressNoMutez @(ContractRef Natural)
          (toContractRef @(ContractRef Natural) @Address "KT1WMgBpt12qcvsAWEEJCTkKpDCgJsqKup8X")
        dip unit
        pair
    apply
    callContractAddressNoMutez @(Lambda () ([Operation], ()))
      (toContractRef @(Lambda () ([Operation], ())) @Address "KT1JCkYWTEv3YLF6JKc9XKiBHMRsjY2w1afk")
  cons
  dip unit
  pair

