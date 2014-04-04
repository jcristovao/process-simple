{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Exit.Util
  ( getErrno
  , resetErrno
  , getLastError
  , decodeExitCode
  , analyseExitCode
  ) where

import System.Exit
import Foreign.C.Error
import Control.DeepSeq

instance NFData ExitCode

-- | Gets C @errno@ and returns it as a string
getLastError :: IO String
getLastError = do
  err <- getErrno
  return . show $ errnoToIOError "" err Nothing Nothing

data ExitCodeError
  = NoError
  | ThrowError String
  | RelayError String
  deriving (Eq)

{-instance Show ExitCodeError where-}
  {-show NoError = error "Exit code successfull, no error condition reported"-}
  {-show (ThrowError err) = err-}
  {-show (RelayError err) = err-}

analyseExitCode :: ExitCode -> String -> String
analyseExitCode ec err = case decodeExitCode ec of
  RelayError err' -> if Prelude.null err' then err else err'
  ThrowError err' -> err'
  NoError         -> "Internal inconsistency error"


-- | Decodes an exit code into a ExitCodeError / String
decodeExitCode :: ExitCode -> ExitCodeError
decodeExitCode ExitSuccess = NoError
decodeExitCode (ExitFailure err)
  -- linux (unix?)
  | err == 0   = NoError
  | err == 64  = ThrowError "command line usage error"
  | err == 65  = ThrowError "data format error"
  | err == 66  = ThrowError "cannot open input"
  | err == 67  = ThrowError "addressee unknown"
  | err == 68  = ThrowError "host name unknown"
  | err == 69  = ThrowError "service unavailable"
  | err == 70  = ThrowError "internal software error"
  | err == 71  = ThrowError "system error"
  | err == 72  = ThrowError "critical OS file missing"
  | err == 73  = ThrowError "can't create (user) output file"
  | err == 74  = ThrowError "input/output error"
  | err == 75  = ThrowError "temporary fail, please retry"
  | err == 76  = ThrowError "remote error in protocol"
  | err == 77  = ThrowError "permission denied"
  | err == 78  = ThrowError "configuration error"
  -- bash specific codes
  | err == 2   = ThrowError "Misuse of shell builtins"
  | err == 126 = ThrowError "Command found, but cannot execute"
  | err == 127 = ThrowError "Command not found"
  | err == 128 = ThrowError "Invalid argument to exit"
  | err >= 255 = ThrowError "Exit status out of range"
  | err > 128  = RelayError $ "Fatal error signal " ++ show err
  | otherwise  = RelayError $ "Unspecified error " ++ show err

