module Internal.WinSCard where

import Foreign
import Foreign.C
import Internal.PCSCLite

#include <winscard.h>

type SCardContext = {#type SCARDCONTEXT#}
