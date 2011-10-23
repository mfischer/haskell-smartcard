{-# LANGUAGE ForeignFunctionInterface #-}

module System.Smartcard.Lowlevel.Reader ( AttrTag (..)
                                        , AttrRequest (..)
                                        , mkRequest
                                        )

where

import Foreign
import Foreign.C
import Data.Maybe
import Data.Tuple (swap)

#include <reader.h>
#include <wintypes.h>
#include <to_attr.h>

-- | Represents the diferent classes of attribute set/get requests.
{#enum define SCardAttrClass { SCARD_CLASS_VENDOR_INFO    as SCardAttrVendorInfo
                             , SCARD_CLASS_COMMUNICATIONS as SCardAttrCommunications
                             , SCARD_CLASS_PROTOCOL       as SCardAttrProtocol
                             , SCARD_CLASS_POWER_MGMT     as SCardAttrPowerManagement
                             , SCARD_CLASS_SECURITY       as SCardAttrSecurity
                             , SCARD_CLASS_MECHANICAL     as SCardAttrMechanical
                             , SCARD_CLASS_VENDOR_DEFINED as SCardAttrVendorDefined
                             , SCARD_CLASS_IFD_PROTOCOL   as SCardAttrIFDProtocol
                             , SCARD_CLASS_ICC_STATE      as SCardAttrICCState
                             , SCARD_CLASS_SYSTEM         as SCardAttrSystem}
#}

-- | Creates the values needed by the IFD handler from the given 'SCardAttrClass' and the 'AttrTag'.
toSCardAttrValue :: SCardAttrClass -> AttrTag -> CULong
toSCardAttrValue c t = let c' = fromIntegral $ fromEnum c
                           t' = fromIntegral $ fromEnum t
                       in unsafePerformIO $ {#call to_attr as ^#} c' t'



-- | The possible values that can be queried from the reader. Note that not all of them are necessarily implemented by your reader.
data AttrTag = VendorName
             | IFDType
             | IFDSerialNo
             | ChannelID
             | AsyncProtocolTypes
             | DefaultClock
             | MaxClock
             | DefaultDataRate
             | MaxDataRate
             | MaxIFSD
             | SyncProtocolTypes
             | PowerManagement
             | UserToCardAuthDevice
             | UserToCardInputDevice
             | Characteristics
             | CurrentProtocol
             | CurrentClock
             | CurrentF
             | CurrentD
             | CurrentW
             | CurrentIFSC
             | CurrentIFSD
             | CurrentBWT
             | CurrentCWT
             | CurrentEBCEncoding
             | CurrentExtendedBWT
             | ICCPresence
             | ICCInterfaceStatus
             | CurrentIOState
             | ATRString
             | ICCTypePerAtr
             | ESCReset
             | ESCCancel
             | ESCAuthRequest
             | ESCMaxInput
             | DeviceUnit
             | DeviceInUse
             | DeviceSystemNameW
             | DeviceSystemNameA
             | DeviceFriendlyNameW
             | DeviceFriendlyNameA
             | SuppressT1IFSRequest
             deriving (Eq)

pairs = [ (VendorName            , 0x0100)
        , (IFDType               , 0x0102)
        , (IFDSerialNo           , 0x0103)
        , (ChannelID             , 0x0110)
        , (AsyncProtocolTypes    , 0x0120)
        , (DefaultClock          , 0x0121)
        , (MaxClock              , 0x0122)
        , (DefaultDataRate       , 0x0123)
        , (MaxDataRate           , 0x0124)
        , (MaxIFSD               , 0x0125)
        , (SyncProtocolTypes     , 0x0126)
        , (PowerManagement       , 0x0131)
        , (UserToCardAuthDevice  , 0x0140)
        , (UserToCardInputDevice , 0x0142)
        , (Characteristics       , 0x0150)
        , (CurrentProtocol       , 0x0201)
        , (CurrentClock          , 0x0202)
        , (CurrentF              , 0x0203)
        , (CurrentD              , 0x0204)
        , (CurrentW              , 0x0206)
        , (CurrentIFSC           , 0x0207)
        , (CurrentIFSD           , 0x0208)
        , (CurrentBWT            , 0x0209)
        , (CurrentCWT            , 0x020a)
        , (CurrentEBCEncoding    , 0x020b)
        , (CurrentExtendedBWT    , 0x020c)
        , (ICCPresence           , 0x0300)
        , (ICCInterfaceStatus    , 0x0301)
        , (CurrentIOState        , 0x0302)
        , (ATRString             , 0x0303)
        , (ICCTypePerAtr         , 0x0304)
        , (ESCReset              , 0xA000)
        , (ESCCancel             , 0xA003)
        , (ESCAuthRequest        , 0xA005)
        , (ESCMaxInput           , 0xA007)
        , (DeviceUnit            , 0x0001)
        , (DeviceInUse           , 0x0002)
        , (DeviceFriendlyNameA   , 0x0003)
        , (DeviceSystemNameA     , 0x0004)
        , (DeviceFriendlyNameW   , 0x0005)
        , (DeviceSystemNameW     , 0x0006)
        , (SuppressT1IFSRequest  , 0x0007)
        ]

reqlu = [ (VendorName            , SCardAttrVendorInfo)
        , (IFDType               , SCardAttrVendorInfo)
        , (IFDSerialNo           , SCardAttrVendorInfo)
        , (ChannelID             , SCardAttrCommunications)
        , (AsyncProtocolTypes    , SCardAttrProtocol)
        , (DefaultClock          , SCardAttrProtocol)
        , (MaxClock              , SCardAttrProtocol)
        , (DefaultDataRate       , SCardAttrProtocol)
        , (MaxDataRate           , SCardAttrProtocol)
        , (MaxIFSD               , SCardAttrProtocol)
        , (SyncProtocolTypes     , SCardAttrProtocol)
        , (PowerManagement       , SCardAttrPowerManagement)
        , (UserToCardAuthDevice  , SCardAttrSecurity)
        , (UserToCardInputDevice , SCardAttrSecurity)
        , (Characteristics       , SCardAttrMechanical)
        , (CurrentProtocol       , SCardAttrIFDProtocol)
        , (CurrentClock          , SCardAttrIFDProtocol)
        , (CurrentF              , SCardAttrIFDProtocol)
        , (CurrentD              , SCardAttrIFDProtocol)
        , (CurrentW              , SCardAttrIFDProtocol)
        , (CurrentIFSC           , SCardAttrIFDProtocol)
        , (CurrentIFSD           , SCardAttrIFDProtocol)
        , (CurrentBWT            , SCardAttrIFDProtocol)
        , (CurrentCWT            , SCardAttrIFDProtocol)
        , (CurrentEBCEncoding    , SCardAttrIFDProtocol)
        , (CurrentExtendedBWT    , SCardAttrIFDProtocol)
        , (ICCPresence           , SCardAttrICCState)
        , (ICCInterfaceStatus    , SCardAttrICCState)
        , (CurrentIOState        , SCardAttrICCState)
        , (ATRString             , SCardAttrICCState)
        , (ICCTypePerAtr         , SCardAttrICCState)
        , (ESCReset              , SCardAttrVendorDefined)
        , (ESCCancel             , SCardAttrVendorDefined)
        , (ESCAuthRequest        , SCardAttrVendorDefined)
        , (ESCMaxInput           , SCardAttrVendorDefined)
        , (DeviceUnit            , SCardAttrSystem)
        , (DeviceInUse           , SCardAttrSystem)
        , (DeviceFriendlyNameA   , SCardAttrSystem)
        , (DeviceSystemNameA     , SCardAttrSystem)
        , (DeviceFriendlyNameW   , SCardAttrSystem)
        , (DeviceSystemNameW     , SCardAttrSystem)
        , (SuppressT1IFSRequest  , SCardAttrSystem)
        ]

instance Enum AttrTag where
  fromEnum x = fromJust $ lookup x pairs
  toEnum   x = let pairs' = map swap pairs
               in  fromJust $ lookup x pairs'

-- | Generates a request that can be used to query the attributes of the reader with 'getAttribute'.
data AttrRequest = AttrRequest CULong

-- | Given the 'AttrTag' you're looking for it will create a request, that can be used with 'getAttribute'.
mkRequest :: AttrTag -> AttrRequest
mkRequest t = let v = fromJust $ lookup t reqlu
              in  AttrRequest $ toSCardAttrValue v t
