{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Exit.Util
  ( getErrno
  , resetErrno
  , getLastError
  , decodeExitCode
  , analyseExitCode
  , exitCodeToIOError
  , evalExitError
  , evalExitErrorNote
  ) where

import System.Exit
import Foreign.C.Error
import System.IO
import GHC.IO.Exception
import Control.DeepSeq
import Control.Exception

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

-- | Construct an 'IOError' based on the given processs exitCode.
-- The optional information can be used to improve the accuracy of
-- error messages.
--
exitCodeToIOError
  :: String -- ^ optional location where the error occurred
  -> Maybe Handle -- ^ optional handle associated with the error
  -> Maybe String -- ^ optional filename associated with the error
  -> ExitCode     -- ^ returned exit code
  -> Maybe IOError -- ^ Just IOError if exit code /= 0
exitCodeToIOError loc maybeHdl maybeName exCode = case exCode of
    ExitSuccess -> Nothing
    (ExitFailure exitCode) -> Just $
      IOError maybeHdl (fst errType) loc errStr Nothing maybeName
        where
          errStr = snd errType
          errType
              | exitCode == 64   = (InvalidArgument   , "command line usage error")
              | exitCode == 65   = (InappropriateType , "data format error")
              | exitCode == 66   = (PermissionDenied  , "cannot open input")
              | exitCode == 67   = (InvalidArgument   , "addressee unknown")
              | exitCode == 68   = (InvalidArgument   , "host name unknown")
              | exitCode == 69   = (NoSuchThing       , "service unavailable")
              | exitCode == 70   = (SystemError       , "internal software error")
              | exitCode == 71   = (SystemError       , "system error")
              | exitCode == 72   = (UnsatisfiedConstraints, "critical OS file missing")
              | exitCode == 73   = (PermissionDenied  , "can't create (user) output file")
              | exitCode == 74   = (PermissionDenied  , "input/output error")
              | exitCode == 75   = (ResourceBusy      , "temporary fail, please retry")
              | exitCode == 76   = (ProtocolError     , "remote error in protocol")
              | exitCode == 77   = (PermissionDenied  , "permission denied")
              | exitCode == 78   = (UserError         , "configuration error")
              | exitCode == 2    = (UnsupportedOperation, "Misuse of shell builtins")
              | exitCode == 126  = (IllegalOperation  , "Command found, but cannot execute")
              | exitCode == 127  = (NoSuchThing       , "Command not found")
              | exitCode == 128  = (InvalidArgument   , "Invalid argument to exit")
              | exitCode >= 255  = (InvalidArgument   , "Exit status out of range" ++ show exitCode)
              | exitCode >  128  = (SystemError       , "Fatal error signal " ++ show exitCode)
              | otherwise        = (OtherError        , "Unknown error " ++  show exitCode)

-- | Raise an IOException based on the exit code, or return () for ExitSuccess
evalExitError :: ExitCode -> IO ()
evalExitError = maybe (return ()) throwIO . exitCodeToIOError "" Nothing Nothing

-- | Like evalExitError but with an additional information note
evalExitErrorNote :: String -> ExitCode -> IO ()
evalExitErrorNote note = maybe (return ()) throwIO
                       . exitCodeToIOError note Nothing Nothing
