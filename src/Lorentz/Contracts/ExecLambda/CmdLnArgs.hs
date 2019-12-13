
{-# OPTIONS -Wall -Wno-missing-export-lists -Wno-unused-do-bind -Wno-partial-fields #-}

-- module Lorentz.Contracts.Oracle.CmdLnArgs where
module Lorentz.Contracts.ExecLambda.CmdLnArgs where

import Control.Applicative
import Control.Monad.Fail
import Text.Show (Show(..))
import Data.List
import Data.Bool
import Data.Either
import Data.Function (id, flip, ($), (.))
import Prelude (FilePath, IO, Natural, (>>=), (<>), mconcat, return)
import Data.String (IsString(..), String)
import Data.Maybe
import Data.Typeable

import Lorentz (ToContractRef(..), Address, EpAddress(..), View(..), Value, printLorentzValue, error, def, printLorentzContract)
import Michelson.Typed.T
import Michelson.Typed.Scope
import Michelson.Typed.Sing
import Michelson.Typed.EntryPoints (EpName(..))
import qualified Michelson.Untyped.Type as U
import Michelson.Parser
import Michelson.Macro
import Michelson.TypeCheck
import Util.IO

import Fmt (prettyLn)
import Control.Monad.Trans.Reader
import qualified Options.Applicative as Opt
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import Data.Constraint
import Data.Singletons
import Text.Megaparsec (eof)

import qualified Lorentz.Contracts.ExecLambda as ExecLambda
import qualified Lorentz.Contracts.ConstView as ConstView


data IsView (t :: T) where
  IsView :: Sing a -> Sing b -> IsView ('TPair a ('TContract b))

isView :: Sing t -> Either String (IsView t)
isView st =
  case st of
    STPair sa sb ->
      case sb of
        STContract sb' -> return $ IsView sa sb'
        _ ->
          Left $ "Expected STContract, but found: " ++ show (fromSing sb)
    _ -> Left $ "Expected STPair, but found: " ++ show (fromSing st)


assertOpAbsense :: forall (t :: T) a. SingI t => (HasNoOp t => a) -> a
assertOpAbsense f =
  case opAbsense (sing @t) of
    Nothing -> error "assertOpAbsense"
    Just Dict -> forbiddenOp @t f

assertContractTypeAbsense :: forall (t :: T) a. SingI t => (HasNoContract t => a) -> a
assertContractTypeAbsense f =
  case contractTypeAbsense  (sing @t) of
    Nothing -> error "assertContractTypeAbsense"
    Just Dict -> forbiddenContractType  @t f

assertBigMapAbsense :: forall (t :: T) a. SingI t => (HasNoBigMap t => a) -> a
assertBigMapAbsense f =
  case bigMapAbsense (sing @t) of
    Nothing -> error "assertBigMapAbsense"
    Just Dict -> forbiddenBigMap @t f

assertNestedBigMapAbsense :: forall (t :: T) a. SingI t => (HasNoNestedBigMaps t => a) -> a
assertNestedBigMapAbsense f =
  case nestedBigMapsAbsense (sing @t) of
    Nothing -> error "assertNestedBigMapAbsense"
    Just Dict -> forbiddenNestedBigMaps @t f


singTypeableCT :: forall (t :: CT). Sing t -> Dict (Typeable t)
singTypeableCT SCInt = Dict
singTypeableCT SCNat = Dict
singTypeableCT SCString = Dict
singTypeableCT SCBytes = Dict
singTypeableCT SCMutez = Dict
singTypeableCT SCBool = Dict
singTypeableCT SCKeyHash = Dict
singTypeableCT SCTimestamp = Dict
singTypeableCT SCAddress = Dict

singTypeableT :: forall (t :: T). Sing t -> Dict (Typeable t)
singTypeableT (STc ct) =
  withDict (singTypeableCT ct) $
  Dict
singTypeableT STKey = Dict
singTypeableT STUnit = Dict
singTypeableT STSignature = Dict
singTypeableT STChainId = Dict
singTypeableT (STOption st) =
  withDict (singTypeableT st) $
  Dict
singTypeableT (STList st) =
  withDict (singTypeableT st) $
  Dict
singTypeableT (STSet st) =
  withDict (singTypeableCT st) $
  Dict
singTypeableT STOperation  = Dict
singTypeableT (STContract st) =
  withDict (singTypeableT st) $
  Dict
singTypeableT (STPair st su) =
  withDict (singTypeableT st) $
  withDict (singTypeableT su) $
  Dict
singTypeableT (STOr st su) =
  withDict (singTypeableT st) $
  withDict (singTypeableT su) $
  Dict
singTypeableT (STLambda st su) =
  withDict (singTypeableT st) $
  withDict (singTypeableT su) $
  Dict
singTypeableT (STMap st su) =
  withDict (singTypeableCT st) $
  withDict (singTypeableT su) $
  Dict
singTypeableT (STBigMap st su) =
  withDict (singTypeableCT st) $
  withDict (singTypeableT su) $
  Dict

singICT :: forall (t :: CT). Sing t -> Dict (SingI t)
singICT SCInt = Dict
singICT SCNat = Dict
singICT SCString = Dict
singICT SCBytes = Dict
singICT SCMutez = Dict
singICT SCBool = Dict
singICT SCKeyHash = Dict
singICT SCTimestamp = Dict
singICT SCAddress = Dict

singIT :: forall (t :: T). Sing t -> Dict (SingI t)
singIT (STc ct) =
  withDict (singICT ct) $
  Dict
singIT STKey = Dict
singIT STUnit = Dict
singIT STSignature = Dict
singIT STChainId = Dict
singIT (STOption st) =
  withDict (singIT st) $
  Dict
singIT (STList st) =
  withDict (singIT st) $
  Dict
singIT (STSet st) =
  withDict (singICT st) $
  Dict
singIT STOperation  = Dict
singIT (STContract st) =
  withDict (singIT st) $
  Dict
singIT (STPair st su) =
  withDict (singIT st) $
  withDict (singIT su) $
  Dict
singIT (STOr st su) =
  withDict (singIT st) $
  withDict (singIT su) $
  Dict
singIT (STLambda st su) =
  withDict (singIT st) $
  withDict (singIT su) $
  Dict
singIT (STMap st su) =
  withDict (singICT st) $
  withDict (singIT su) $
  Dict
singIT (STBigMap st su) =
  withDict (singICT st) $
  withDict (singIT su) $
  Dict

data CmdLnArgs
  = Print (Maybe FilePath) Bool
  | PrintConstView Natural (Maybe FilePath) Bool
  | ViewToParams (SomeSing T)
  | ViewToVoid
      { execLambda :: Address
      , parameterType :: SomeSing T
      , callbackType :: SomeSing T
      , viewEntrypoint :: EpAddress
      , viewParameter :: Text
      }

unExplicitType :: U.Type -> U.T
unExplicitType =
  \case
    U.Type t _ -> t
    -- U.TypeParameter -> error "U.TypeParameter"
    -- U.TypeStorage -> error "U.TypeStorage"

fromUntypedComparable :: U.Comparable -> CT
fromUntypedComparable (U.Comparable ct _) = ct

fromUntypedT' :: U.Type -> T
fromUntypedT' = fromUntypedT . unExplicitType

fromUntypedT :: U.T -> T
fromUntypedT (U.Tc ct) = Tc ct
fromUntypedT U.TKey = TKey
fromUntypedT U.TUnit = TUnit
fromUntypedT U.TChainId = TChainId
fromUntypedT U.TSignature = TSignature
fromUntypedT (U.TOption x) = TOption $ fromUntypedT' x
fromUntypedT (U.TList x) = TList $ fromUntypedT' x
fromUntypedT (U.TSet ct) = TSet $ fromUntypedComparable ct
fromUntypedT U.TOperation = TOperation
fromUntypedT (U.TContract x) = TContract $ fromUntypedT' x
fromUntypedT (U.TPair _ _ x y) = TPair (fromUntypedT' x) (fromUntypedT' y)
fromUntypedT (U.TOr _ _ x y) = TOr (fromUntypedT' x) (fromUntypedT' y)
fromUntypedT (U.TLambda x y) = TLambda (fromUntypedT' x) (fromUntypedT' y)
fromUntypedT (U.TMap ct x) = TMap (fromUntypedComparable ct) $ fromUntypedT' x
fromUntypedT (U.TBigMap ct x) = TBigMap (fromUntypedComparable ct) $ fromUntypedT' x

parseSomeT :: String -> Opt.Parser (SomeSing T)
parseSomeT name =
  (\typeStr ->
    let parsedType = parseNoEnv
          type_ -- _ -- explicitType
          name
          typeStr
     in let type' = either (error . T.pack . show) unExplicitType parsedType
     in withSomeSingT (fromUntypedT type') SomeSing
  ) <$>
  Opt.strOption @Text
    (mconcat
      [ Opt.long $ name ++ "Type"
      , Opt.metavar "Michelson Type"
      , Opt.help $ "The Michelson Type of " ++ name
      ])

-- | Parse a `String`
parseString :: IsString s => String -> String -> Opt.Parser s
parseString name description' = Opt.strOption $ mconcat
  [ Opt.long name
  , Opt.metavar "STRING"
  , Opt.help description'
  ]

-- | Parse a natural number argument, given its field name
-- Lorentz.Contracts.Parse
parseNatural :: String -> Opt.Parser Natural
parseNatural name =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar "NATURAL"
    , Opt.help $ "Natural number representing " ++ name ++ "."
    ]

-- | Parse an `Address` argument, given its field name
-- Lorentz.Contracts.Parse
parseAddress :: String -> String -> Opt.Parser Address
parseAddress name description' =
  Opt.option Opt.auto $
  mconcat
    [ Opt.long name
    , Opt.metavar "ADDRESS"
    , Opt.help description'
    ]

-- | Parse whether to output on one line
-- Lorentz.Contracts.Parse
onelineOption :: Opt.Parser Bool
onelineOption = Opt.switch (
  Opt.long "oneline" <>
  Opt.help "Force single line output")

-- | Parse the output `FilePath`
outputOptions :: Opt.Parser (Maybe FilePath)
outputOptions = optional . Opt.strOption $ mconcat
  [ Opt.short 'o'
  , Opt.long "output"
  , Opt.metavar "FILEPATH"
  , Opt.help "File to use as output. If not specified, stdout is used."
  ]

argParser :: Opt.Parser CmdLnArgs
argParser = Opt.hsubparser $ mconcat
  [ printSubCmd
  , printConstViewSubCmd
  , viewToParamsSubCmd
  , viewToVoidSubCmd
  ]
  where
    mkCommandParser commandName parser desc =
      Opt.command commandName $
      Opt.info (Opt.helper <*> parser) $
      Opt.progDesc desc

    printSubCmd =
      mkCommandParser "print"
      (Print <$> outputOptions <*> onelineOption)
      "Dump the ExecLambda contract in form of Michelson code"

    printConstViewSubCmd =
      mkCommandParser "print-const-view"
      (PrintConstView <$>
        parseNatural "constToView" <*>
        outputOptions <*>
        onelineOption)
      "Dump the ConstView contract (specialized to 'nat') in form of Michelson code"

    viewToParamsSubCmd =
      mkCommandParser "view-to-params"
      (ViewToParams <$>
        parseSomeT "parameter"
      )
      ("Convert a Michelson parameter type to arguments to a View, i.e. " <>
       "convert it into the form: (pair FOO (contract BAR))")

    viewToVoidSubCmd =
      mkCommandParser "view-to-void"
      (ViewToVoid <$>
        parseAddress
          "execLambda"
          "The ExecLambda contract's Address" <*>
        parseSomeT "parameter" <*>
        parseSomeT "callback" <*>
        parseEpAddress <*>
        parseString
          "viewParameter"
          "The parameter value for the 'View' entrypoint you want to call"
      )
      ("Make an ExecLambda parameter to access a contract's View parameter " <>
       "off-chain using FAILWITH, i.e. like a Void_ parameter.")

    parseEpAddress :: Opt.Parser EpAddress
    parseEpAddress =
      EpAddress <$>
      parseAddress
        "viewContract"
        "The contract whose view parameter you want to call" <*>
      (EpNameUnsafe <$>
        (parseString
          "viewEntrypoint"
          "The entrypoint name of the view you want to call, or empty for default" <|>
         pure ""
        )
      )

infoMod :: Opt.InfoMod CmdLnArgs
infoMod = mconcat
  [ Opt.fullDesc
  , Opt.progDesc "ExecLambda contract CLI interface"
  ]

-- | Parse and typecheck a Michelson value
-- Lorentz.Contracts.GenericMultisig.Wrapper
parseTypeCheckValue ::
     forall t. (Typeable t, SingI t)
  => Parser (Value t)
parseTypeCheckValue =
  (>>= either (fail . show) return) $
  runTypeCheckIsolated . flip runReaderT def . typeVerifyValue . expandValue <$>
  (value <* eof)

runViewToVoid :: forall (a :: T) (r :: T). (ProperConstantBetterErrors a, ProperParameterBetterErrors a, ProperParameterBetterErrors r)
  => Address
  -> EpAddress
  -> Text
  -> ExecLambda.Parameter
runViewToVoid execLambda' viewEntrypoint' viewParameterText =
  forbiddenOp @a $
  forbiddenNestedBigMaps @a $
  let parsedValue =
        either (error . T.pack . show) id $
        parseNoEnv
          (parseTypeCheckValue @a)
          "ExecLambdaContract"
          viewParameterText
   in ExecLambda.viewToVoidParameter
        (toContractRef @ExecLambda.Parameter execLambda')
        (toContractRef @(View (Value a) (Value r)) viewEntrypoint')
        parsedValue

runCmdLnArgs :: CmdLnArgs -> IO ()
runCmdLnArgs = \case
  Print mOutput forceOneLine ->
    maybe TL.putStrLn writeFileUtf8 mOutput $
    printLorentzContract forceOneLine ExecLambda.execLambdaContract
  PrintConstView x mOutput forceOneLine ->
    maybe TL.putStrLn writeFileUtf8 mOutput $
    printLorentzContract forceOneLine $ ConstView.constViewContract x
  ViewToParams (SomeSing st) ->
    case either (error . T.pack) id $ isView st of
      IsView sa sb -> do
        prettyLn $ fromSing sa
        prettyLn $ fromSing sb

-- data IsView (t :: T) where
--   IsView :: Sing a -> Sing b -> IsView ('TPair a ('TContract b))

-- isView :: Sing t -> Either String (IsView t)
-- isView st =
--   case st of
--     STPair sa sb ->
--       case sb of
--         STContract sb' -> return $ IsView sa sb'
--         _ ->
--           Left $ "Expected STContract, but found: " ++ show (fromSing sb')
--     _ -> Left $ "Expected STPair, but found: " ++ show (fromSing st)

  ViewToVoid {..} ->
    case (parameterType, callbackType) of
      (SomeSing (sa :: Sing a), SomeSing (sr :: Sing r)) ->
        withDict (singIT sa) $
        withDict (singTypeableT sa) $
        assertOpAbsense @a $
        assertBigMapAbsense @a $
        assertNestedBigMapAbsense @a $
        assertContractTypeAbsense @a $
        withDict (singIT sr) $
        withDict (singTypeableT sr) $
        assertOpAbsense @r $
        assertBigMapAbsense @r $
        assertNestedBigMapAbsense @r $
        TL.putStrLn . printLorentzValue forceSingleLine $
        runViewToVoid @a @r execLambda viewEntrypoint viewParameter
  where
    forceSingleLine = True

