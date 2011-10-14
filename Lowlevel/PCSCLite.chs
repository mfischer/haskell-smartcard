{-# LANGUAGE ForeignFunctionInterface #-}

module Lowlevel.PCSCLite ( statusToString
                         , SCardStatus (..)
                         , SCardShare (..)
                         , SCardProtocol (..)
                         , SCardScope (..)
                         , SCardContext
                         , SCardAction (..)
                         , SCardCardState (..)
                         , fromCLong
                         , SCardIORequest
                         , toSCardProtocol
                         , mkSCardIORequestT0
                         , mkSCardIORequestT1
                         , mkSCardIORequestRaw)
where


import Data.Int
import Foreign
import Foreign.C
import Foreign.Storable
import Control.Monad
import Control.Applicative

#include <pcsclite_patched.h>

-- | Converts a 'SCardStatus' to the error message.
statusToString :: SCardStatus -> String
statusToString s = unsafePerformIO $
                   {#call pcsc_stringify_error as ^#} (fromIntegral $ fromEnum s) >>= peekCString

-- | Returncodes given by the PC\/SC daemon.
{#enum define SCardStatus { SCARD_S_SUCCESS             as Ok
                          , SCARD_F_INTERNAL_ERROR      as InternalError
                          , SCARD_E_CANCELLED           as Cancelled
                          , SCARD_E_INVALID_HANDLE      as InvalidHandle
                          , SCARD_E_INVALID_PARAMETER   as InvalidParameter
                          , SCARD_E_INVALID_TARGET      as InvalidTarget
                          , SCARD_E_NO_MEMORY           as NoMemory
                          , SCARD_F_WAITED_TOO_LONG     as WaitTooLong
                          , SCARD_E_INSUFFICIENT_BUFFER as InsufficientBuffer
                          , SCARD_E_UNKNOWN_READER      as ReaderUnknown
                          , SCARD_E_TIMEOUT             as Timeout
                          , SCARD_E_SHARING_VIOLATION   as SharingViolation
                          , SCARD_E_NO_SMARTCARD        as NoSmartCard
                          , SCARD_E_UNKNOWN_CARD        as UnknownCard
                          , SCARD_E_CANT_DISPOSE        as CannotDispose
                          , SCARD_E_PROTO_MISMATCH      as ProtocolMismatch
                          , SCARD_E_NOT_READY           as NotReady
                          , SCARD_E_INVALID_VALUE       as InvalidValue
                          , SCARD_E_SYSTEM_CANCELLED    as SystemCancelled
                          , SCARD_F_COMM_ERROR          as CommunicationError
                          , SCARD_F_UNKNOWN_ERROR       as UnknownError
                          , SCARD_E_INVALID_ATR         as InvalidATR
                          , SCARD_E_NOT_TRANSACTED      as NotTransacted
                          , SCARD_E_READER_UNAVAILABLE  as ReaderUnavailable
                          , SCARD_P_SHUTDOWN            as Shutdown
                          , SCARD_E_PCI_TOO_SMALL       as PciToSmall
                          , SCARD_E_READER_UNSUPPORTED  as ReaderUnsupported
                          , SCARD_E_DUPLICATE_READER    as DuplicateReader
                          , SCARD_E_CARD_UNSUPPORTED    as CardUnsupportedError
                          , SCARD_E_NO_SERVICE          as NoService
                          , SCARD_E_SERVICE_STOPPED     as ServiceStopped
                          , SCARD_E_UNSUPPORTED_FEATURE as UnsupportedFeatureOrUnexpected
                          , SCARD_E_ICC_INSTALLATION    as IccInstallation
                          , SCARD_E_ICC_CREATEORDER     as IccCreateOrder
                          , SCARD_E_DIR_NOT_FOUND       as DirectoryNotFound
                          , SCARD_E_FILE_NOT_FOUND      as FileNotFound
                          , SCARD_E_NO_DIR              as NoDirectory
                          , SCARD_E_NO_FILE             as NoFile
                          , SCARD_E_NO_ACCESS           as NoAccess
                          , SCARD_E_WRITE_TOO_MANY      as TooManyWrites
                          , SCARD_E_BAD_SEEK            as BadSeek
                          , SCARD_E_INVALID_CHV         as InvalidCHV
                          , SCARD_E_UNKNOWN_RES_MNG     as UnknownResMNG
                          , SCARD_E_NO_SUCH_CERTIFICATE as NoSuchCertificate
                          , SCARD_E_CERTIFICATE_UNAVAILABLE as CertificateUnavailable
                          , SCARD_E_NO_READERS_AVAILABLE as NoReaderAvailable
                          , SCARD_E_COMM_DATA_LOST      as CommunicationDataLost
                          , SCARD_E_NO_KEY_CONTAINER    as NoKeyContainer
                          , SCARD_E_SERVER_TOO_BUSY     as ServerToBusy
                          , SCARD_W_UNSUPPORTED_CARD    as CardUnsupportedWarning
                          , SCARD_W_UNRESPONSIVE_CARD   as UnresponsiveCard
                          , SCARD_W_UNPOWERED_CARD      as CardUnpowered
                          , SCARD_W_RESET_CARD          as CardReset
                          , SCARD_W_REMOVED_CARD        as CardRemoved
                          , SCARD_W_SECURITY_VIOLATION  as SecurityViolation
                          , SCARD_W_WRONG_CHV           as WrongCHV
                          , SCARD_W_CHV_BLOCKED         as CHVBlocked
                          , SCARD_W_EOF                 as EOF
                          , SCARD_W_CANCELLED_BY_USER   as CancelledByUser
                          , SCARD_W_CARD_NOT_AUTHENTICATED as CardNotAuthenticated}
                          deriving (Eq)
#}

instance Show SCardStatus where
  show = statusToString

fromCLong :: (Integral a, Enum c) => a -> c
fromCLong = toEnum . fromIntegral



{#
enum define SCardScope { SCARD_SCOPE_USER     as UserScope
                       , SCARD_SCOPE_TERMINAL as TerminalScope
                       , SCARD_SCOPE_SYSTEM   as SystemScope}
#}


type SCardContext = {#type SCARDCONTEXT#}


-- | These are the possible sharing modes for 'establishContext'
{#
enum define SCardShare { SCARD_SHARE_EXCLUSIVE as Exclusive
                       , SCARD_SHARE_SHARED    as Shared
                       , SCARD_SHARE_DIRECT    as Direct}
#}

{#
enum define SCardProtocol { SCARD_PROTOCOL_T1        as T1
                          , SCARD_PROTOCOL_T0        as T0
                          , SCARD_PROTOCOL_T15       as T15
                          , SCARD_PROTOCOL_UNDEFINED as Undefined
                          , SCARD_PROTOCOL_RAW       as Raw}
#}

-- | Converts a 'CULong' to the 'SCardProtocol'.
toSCardProtocol :: CULong -> SCardProtocol
toSCardProtocol = toEnum . fromIntegral

instance Show SCardProtocol where
  show T0        = "T0"
  show T1        = "T1"
  show T15       = "T15"
  show Raw       = "Raw"
  show Undefined = "Undefined"

{#
enum define SCardAction { SCARD_LEAVE_CARD   as LeaveCard
                        , SCARD_RESET_CARD   as ResetCard
                        , SCARD_UNPOWER_CARD as UnpowerCard
                        , SCARD_EJECT_CARD   as EjectCard}
#}

-- | Creates a 'SCardIORequest' setup for the 'T0' protocol.
mkSCardIORequestT0 :: SCardIORequest
mkSCardIORequestT0  = SCardIORequest { getProtocol = T0
                                     , getSize = {#sizeof SCARD_IO_REQUEST#}}

-- | Creates a 'SCardIORequest' setup for the 'T1' protocol.
mkSCardIORequestT1 :: SCardIORequest
mkSCardIORequestT1  = SCardIORequest { getProtocol = T1
                                     , getSize = {#sizeof SCARD_IO_REQUEST#}}

-- | Creates a 'SCardIORequest' setup for the 'Raw' protocol.
mkSCardIORequestRaw :: SCardIORequest
mkSCardIORequestRaw = SCardIORequest { getProtocol = Raw
                                     , getSize = {#sizeof SCARD_IO_REQUEST#}}

data SCardIORequest = SCardIORequest { getProtocol :: SCardProtocol
                                     , getSize     :: Int}

instance Storable SCardIORequest where
  sizeOf    _ = {#sizeof SCARD_IO_REQUEST #}
  alignment _ = {#alignof SCARD_IO_REQUEST#}
  peek p = SCardIORequest
       <$> liftM toSCardProtocol ({#get SCARD_IO_REQUEST->dwProtocol #} p)
       <*> liftM fromIntegral    ({#get SCARD_IO_REQUEST->cbPciLength #} p)
  poke p x = do
    {#set SCARD_IO_REQUEST.dwProtocol  #} p $ fromIntegral . fromEnum $ getProtocol x
    {#set SCARD_IO_REQUEST.cbPciLength #} p $ fromIntegral $ getSize x


{#
enum define SCardCardState { SCARD_UNKNOWN as Unknown
                           , SCARD_ABSENT  as Absent
                           , SCARD_PRESENT as Present
                           , SCARD_POWERED as Powered
                           , SCARD_NEGOTIABLE as Negotiable
                           , SCARD_SPECIFIC as Specific}
#}

instance Show SCardCardState where
  show Unknown    = "Unknown"
  show Absent     = "Absent"
  show Present    = "Present"
  show Powered    = "Powered"
  show Negotiable = "Negotiable"
  show Specific   = "Specific"
