{-# LANGUAGE OverloadedStrings #-}
module System.Process.Simple
  ( sh
  ) where

import Prelude hiding (takeWhile)
import System.Process hiding (env)
import System.Exit

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import Data.Monoid
{-import Data.Easy-}
import Data.Char
import Data.List as L hiding (takeWhile)
import Data.Text (Text)
{-import Data.Attoparsec-}
import Data.Attoparsec.Text as AT
{-import Data.Text.Util-}
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Exception.Enclosed.Either
import System.Exit.Util

import Data.IORef
import qualified Data.Map    as Map
import qualified Data.IntMap as IM

-- | Split a command into an argument list
argParser :: Text -> [String]
argParser = L.map T.unpack . nonEmpty . parseCommand . T.strip

-- | Execute a command with the given argument list, and return the Output
-- (@STDOUT@) as a Right value if everything was ok, or a Left value with either:
-- * @STDERR@ output
-- * A raised exception text
-- * A non-zero return code associated failure
sh :: String -> Text -> EitherT Text IO Text
sh cmd args = do
  liftIO resetErrno
  ret <- eIOExTxIO $ readProcessWithExitCode cmd (argParser args) ""
  err <- liftIO getLastError
  processResult ret err

processResult
  :: Monad m
  => (ExitCode, String, String)
  -> String
  -> EitherT Text m Text
processResult (exitCode,stdo,stde) err
      | exitCode /= ExitSuccess = left  . T.pack
                                $ stde <§> analyseExitCode exitCode err
      | (not . L.null) stde     = left  . T.pack $ stde
      | otherwise               = right . T.pack $ stdo

parseSh :: Text -> [Text]
parseSh = L.filter (not . T.null) . T.split (\c -> c == '\n' || c == '\r')

{-# ANN parseCommand'' ("HLint: ignore Use notElem"::String) #-}
-- ATTN: does not properly handle two consecutive spaces
parseCommand'' :: Parser Text
parseCommand'' = do
  start <- takeWhile (\c -> c /= '\\' && c /= '"' && c /= ' ')
  sepr  <- try (char '\\') <|> try (char '"') <|> try (char ' ') <|> return ' '
  case sepr of

    '\\' -> do
              plusOne <- try (AT.take 1)
              return $ start <> T.singleton sepr <> plusOne

    '"'  -> do
              inQuotes <- try parseTillEndOfQuotes
              return $ start <> inQuotes
              {-return $ start <> (T.singleton sepr) <> inQuotes-}

    _   ->  if T.null start
              then fail $ "Empty result:" ++ T.unpack start
              else return start



parseCommand' :: Parser [Text]
parseCommand' = do
  comms <- many (try parseCommand'')
  nonEmpty <$> if Prelude.null comms
    then fmap (: []) takeText
    else do
      lastc <- takeText
      return $ comms ++ [lastc]


parseCommand :: Text -> [Text]
parseCommand cs = either (\x -> error $ "Invalid command - cannot parse:" ++ x)
                         (fromMonoid $ error "Empty command")
                       $ parseOnly parseCommand' cs

parseTillEndOfQuotes :: Parser Text
parseTillEndOfQuotes = do
  start <- takeWhile (\c -> c /= '\\' && c /= '"')
  sepr  <- char '\\' <|> char '"'
  case sepr of

    '\\' -> do
              plusOne <- AT.take 1
              remain  <- parseTillEndOfQuotes
              return $ start <> T.singleton sepr <> plusOne <> remain

    _   -> do
      nc <- peekChar
      if nc == Just ' '
        then char ' ' >> return start
        else return start

------------------------------------------------------------------------------
-- Monoid operations ---------------------------------------------------------
------------------------------------------------------------------------------
-- | Not perfect, but enough for current use
nonEmpty :: (Eq a, Monoid a) => [a] -> [a]
nonEmpty = filter notEmpty

-- | Check it is mempty
{-# INLINE isEmpty #-}
isEmpty :: (Monoid a, Eq a) => a -> Bool
isEmpty = (==) mempty

-- | Alias for @'isNotEmpty'@.
{-# INLINE notEmpty #-}
notEmpty :: (Monoid a, Eq a) => a -> Bool
notEmpty = isNotEmpty

-- | Check that a monoid is not mempty
{-# INLINE isNotEmpty #-}
isNotEmpty :: (Monoid a, Eq a) => a -> Bool
isNotEmpty = (/=) mempty

-- | fromMonoid keeps the monoid value if it is not empty,
-- otherwise it replaces it with the provided default value
--
-- /Note/: No check is made to see if default value is itself mempty
--
-- /Note/: This differs from @fromMaybe@ in the sense it is not possible
-- to extract values from monoid
--
-- /Note/: similar to @flip <|>@ for the appropriate types.
--
fromMonoid :: (Eq a, Monoid a) => a -> a -> a
fromMonoid def mon = if isEmpty mon then def else mon

(<§>) :: (Monoid a, Eq a) => a -> a -> a
(<§>) a b = if isEmpty a then b else a

-- | Just like (@'$'@), but with higher precedence than (@'<>'@), but still lower
-- than (@'.'@). Similar to "Diagrams.Util" @'#'@, but without flipped arguments.
{-# INLINE (§) #-}
(§) :: (a -> b) -> a -> b
f § x =  f x
infixr 8 §


