{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS -Wno-partial-fields -Wno-orphans -Wno-missing-export-lists #-}

module Main where

import Control.Applicative
-- import Control.Monad hiding (fail)
import Data.Function
-- import Data.List
-- import Data.String
import System.IO
-- import Text.Read
-- import Text.Show
-- import Text.ParserCombinators.ReadP (ReadP)
-- import qualified Text.ParserCombinators.ReadP as P
import Prelude (die, displayException, catchAny) -- const, id, maybe,

import Lorentz
-- import qualified Tezos.Address as Tezos
-- import Michelson.Typed.Scope
import Util.IO
-- import Michelson.Printer
-- import Michelson.Typed.Instr (toFullContract)
-- import Michelson.Optimizer

-- import Named
-- import Util.Named
import qualified Options.Applicative as Opt
-- import qualified Options.Applicative.Common as Opt
-- import qualified Options.Applicative.Types as Opt
-- import qualified Data.Text as T
-- import qualified Data.Text.Lazy as TL
-- import qualified Data.Text.Lazy.IO as TL
-- import Data.Singletons
import Text.PrettyPrint.ANSI.Leijen.Internal (Doc, linebreak)

import qualified Lorentz.Contracts.ExecLambda.CmdLnArgs as ExecLambdaCmdLnArgs

-- data CmdLnArgs
--   = ExecLambdaCmdLnArgs { execLambdaCmdLnArgs :: ExecLambdaCmdLnArgs.CmdLnArgs }
--
-- argParser :: Opt.Parser CmdLnArgs
-- argParser = Opt.hsubparser $ mconcat []
--   [ Opt.command "ExecLambda" $ fmap ExecLambdaCmdLnArgs $ Opt.info ExecLambdaCmdLnArgs.argParser ExecLambdaCmdLnArgs.infoMod
--   ]

programInfo :: Opt.ParserInfo ExecLambdaCmdLnArgs.CmdLnArgs
programInfo =
  -- Opt.info (Opt.helper <*> versionOption <*> argParser) $
  Opt.info (Opt.helper <*> ExecLambdaCmdLnArgs.argParser) $
  mconcat
    [ Opt.fullDesc
    , Opt.progDesc "ExecLambda contract parameter generation helper"
    , Opt.header "Lorentz tools"
    , Opt.footerDoc usageDoc
    ]
  -- where
  --   versionOption =
  --     Opt.infoOption
  --       ("lorentz-contract-oracle-" <> versionStr)
  --       (Opt.long "version" <> Opt.help "Show version.")
  --   versionStr = "0.1.0.0" -- showVersion version

usageDoc :: Maybe Doc
usageDoc =
  Just $
  mconcat
    [ "You can use help for specific COMMAND"
    , linebreak
    , "EXAMPLE:"
    , linebreak
    , "  lorentz-contract-view COMMAND --help"
    , linebreak
    ]

main :: IO ()
main = do
  hSetTranslit stdout
  hSetTranslit stderr

  cmdLnArgs <- Opt.execParser programInfo
  run cmdLnArgs `catchAny` (die . displayException)
  where
    run :: ExecLambdaCmdLnArgs.CmdLnArgs -> IO ()
    run = ExecLambdaCmdLnArgs.runCmdLnArgs
      -- \case { ExecLambdaCmdLnArgs {..} -> ExecLambdaCmdLnArgs.runCmdLnArgs execLambdaCmdLnArgs }

