{-# OPTIONS -Wno-missing-export-lists -Wno-orphans #-}

module Lorentz.Contracts.Util.Address where

import Tezos.Address

import Lorentz

import Prelude ((>>=), id, const)
import Control.Applicative
import Control.Monad.Fail
import Data.Char
import Data.String
import Text.Show
import Text.Read
import qualified Text.ParserCombinators.ReadP as P
import Text.ParserCombinators.ReadP (ReadP)

import qualified Data.Text as T


-- | Parse something between the two given `Char`'s
betweenChars :: Char -> Char -> ReadP a -> ReadP a
betweenChars beforeChar afterChar =
  P.char beforeChar `P.between` P.char afterChar

-- | Parse something in parentheses
inParensP :: ReadP a -> ReadP a
inParensP = '(' `betweenChars` ')'

-- | Parse something in double-quotes: @"[something]"@
inQuotesP :: ReadP a -> ReadP a
inQuotesP = '"' `betweenChars` '"'

-- | Attempt to parse with given modifier, otherwise parse without
maybeLiftP :: (ReadP a -> ReadP a) -> ReadP a -> ReadP a
maybeLiftP liftP = liftA2 (<|>) liftP id

-- | Attempt to parse `inParensP`, else parse without
maybeInParensP :: ReadP a -> ReadP a
maybeInParensP = maybeLiftP inParensP

-- | Attempt to parse `inQuotesP`, else parse without
maybeInQuotesP :: ReadP a -> ReadP a
maybeInQuotesP = maybeLiftP inQuotesP

-- | Read an `Address`, inside or outside of @""@'s
readAddressP :: ReadP Address
readAddressP =
      maybeInParensP . maybeInQuotesP $
        ensureAddressPrefix *>
        P.munch1 isAlphaNum >>=
        \addressStr -> case parseAddress $ T.pack addressStr of
          Left err -> fail $ show err
          Right address' -> return address'
  where
    ensureAddressPrefix = ensureTZ1 <|> ensureKT1
    ensureTZ1 = P.look >>= \case { ('t':'z':'1':_) -> return (); _ -> fail "expected tz1_" }
    ensureKT1 = P.look >>= \case { ('K':'T':'1':_) -> return (); _ -> fail "expected KT1_" }

instance Read Address where
  readPrec = readP_to_Prec $ const readAddressP

instance IsString Address where
  fromString = read

