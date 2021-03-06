{-# LANGUAGE OverloadedStrings #-}
module System.Process.Simple
  ( shT
  , shT'
  , shell
  , parseCommand
  ) where

import Prelude hiding (takeWhile)
import System.Process hiding (env,shell)
import System.Exit

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Control.Exception

import Data.Monoid
import Data.List as L hiding (takeWhile)
import Data.Text (Text)
import Data.Attoparsec.Text as AT
import qualified Data.Text as T
{-import qualified Data.Text.IO as T-}

import Control.Exception.Enclosed.Either
import System.Exit.Util
import GHC.IO.Exception


-- | Split a command into an argument list
argParser :: Text -> [String]
argParser = L.map T.unpack . nonEmpty . parseCommand . T.strip

-- | Execute a command with the given argument list, and return the Output
-- (@STDOUT@) as a Right value if everything was ok, or a Left value with either:
-- * @STDERR@ output
-- * A raised exception text
-- * A non-zero return code associated failure
shT :: String -> Text -> EitherT Text IO Text
shT cmd args = do
  liftIO resetErrno
  ret <- eIOExTxIO $ readProcessWithExitCode cmd (argParser args) ""
  err <- liftIO getLastError
  processResult ret err

shT' :: String -> [Text] -> EitherT Text IO Text
shT' cmd args = do
  liftIO resetErrno
  ret <- eIOExTxIO $ readProcessWithExitCode cmd (fmap T.unpack args) ""
  err <- liftIO getLastError
  processResult ret err

-- | Executed a shell command with the given argument list.
-- If the exit code is non-zero or stderr is non null, it raises an IOException.
shell :: Text -> [Text] -> IO Text
shell cmd args = do
  (ec,stdo,stde) <- readProcessWithExitCode (T.unpack cmd) (fmap T.unpack args) ""
  evalExitError ec
  unless (L.null stde)
         (throwIO $ IOError Nothing OtherError "stderr reported"
                                               (dropLastNL stde)
                                               Nothing
                                               Nothing)
  return . T.pack $ stdo

dropLastNL :: String -> String
dropLastNL [] = []
dropLastNL s = init s ++ (if last s == '\n' then [] else [last s])

processResult
  :: Monad m
  => (ExitCode, String, String)
  -> String
  -> EitherT Text m Text
processResult (exitCode,stdo,stde) err
      | exitCode /= ExitSuccess = left  . T.pack
                                $ stde <\> analyseExitCode exitCode err
      | (not . L.null) stde     = left  . T.pack $ stde
      | otherwise               = right . T.pack $ stdo

parseCommand' :: Parser [Text]
parseCommand' = do
  comms <- many parseC'
  nonEmpty <$> if Prelude.null comms
    then fmap (: []) takeText
    else do
      lastc <- takeText
      return $ comms ++ [lastc]

{-# ANN parseC' ("HLint: ignore Use notElem"::String) #-}
parseC' :: Parser Text
parseC' = do
  start <- takeWhile ( `notElem` ['\\','"',' '])
  nc    <- peekChar
  case nc of
    Just ' ' -> do
      _   <- skipSpace
      return start
    Just '\\' -> do
      plusTwo <- (AT.take 2) <|> (AT.take 1)
      remain <- parseC'
      return $ start <> plusTwo <> remain
    Just '"' ->
      if T.null start
        then do
          _ <- AT.take 1
          inQuotes <- parseTillEndOfQuotes'
          return $ start <> inQuotes

        else do
          plusOne <- AT.take 1
          remain <- parseC'
          return $ start <> plusOne <> remain
    Nothing -> if T.null start then fail "No more text" else return start


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

parseTillEndOfQuotes' :: Parser Text
parseTillEndOfQuotes' = do
  start <- takeWhile (\c -> c /= '\\' && c /= '"')
  sepr  <- char '\\' <|> char '"'
  case sepr of

    '\\' -> do
              plusOne <- AT.take 1
              remain  <- parseTillEndOfQuotes'
              return $ start <> T.singleton sepr <> plusOne <> remain

    _   -> do
      nc <- peekChar
      if nc == Just ' '
        then char ' ' >> return start
        else do
          remain <- parseTillEndOfQuotes'
          return $ start <> T.singleton sepr <> remain


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

(<\>) :: (Monoid a, Eq a) => a -> a -> a
(<\>) a b = if isEmpty a then b else a

-- | Just like (@'$'@), but with higher precedence than (@'<>'@), but still lower
-- than (@'.'@). Similar to "Diagrams.Util" @'#'@, but without flipped arguments.
{-# INLINE (§) #-}
(§) :: (a -> b) -> a -> b
f § x =  f x
infixr 8 §


