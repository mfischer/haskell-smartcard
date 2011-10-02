{-# LANGUAGE ForeignFunctionInterface #-}

module Internal.PCSCLite where

import Data.Int
import Foreign
import Foreign.C

#include <pcsclite.h>


-- Error type, for now we don't care as we can just 'show' it via stringify ;-)
data SCardError = SCardError {unSCardError :: Int}

instance Show SCardError where
  show = errorToString

instance Eq SCardError where
  (==) a b = unSCardError a == unSCardError b

toSCardError :: CLong -> SCardError
toSCardError e = SCardError {unSCardError = fromIntegral e}

errToString_ :: Int -> IO (String)
errToString_ = {#fun pcsc_stringify_error as ^ {`Int'} -> `String' #}

errorToString :: SCardError -> String
errorToString e = unsafePerformIO $ errToString_ $ unSCardError e

{#
enum define SCardScope { SCARD_SCOPE_USER as UserScope
                       , SCARD_SCOPE_TERMINAL as TerminalScope
                       , SCARD_SCOPE_SYSTEM as SystemScope}
#}


type SCardContext = {#type SCARDCONTEXT#}
