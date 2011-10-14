{-# LANGUAGE ForeignFunctionInterface #-}

-- | The WinSCard module wraps the WinSCard API.
-- It should be regarded as a low-level wrapper, though, as it still feels rather C-ish.
module Lowlevel.WinSCard ( establishContext
                         , releaseContext
                         , validateContext
                         , listReaders
                         , listReaderGroups
                         , transmit
                         , connect
                         , disconnect
                         , reconnect
                         , status
                         , beginTransaction
                         , endTransaction )
where

import Foreign
import Foreign.C
import Foreign.C.String
import Lowlevel.PCSCLite
import Data.List
import Data.List.Utils
import Data.Bits
import Control.Monad

#include <winscard.h>

-- | The 'establishContext' function needs to be invoked first when talking to the pcscd.
establishContext :: SCardScope -> IO (Either SCardStatus SCardContext)
establishContext s = alloca $ \ctx_ptr ->
                        do rt  <- liftM fromCLong $ {#call SCardEstablishContext as ^#} (fromIntegral(fromEnum s)) nullPtr nullPtr ctx_ptr
                           ctx <- peek ctx_ptr
                           if rt == Ok then return $Right ctx
                                       else return $Left rt

-- | The 'releaseContext' function needs to be invoked last in the program, to cleanup.
releaseContext :: SCardContext -> IO (SCardStatus)
releaseContext c = liftM fromCLong $ {#call SCardReleaseContext as ^#} c

-- | Given a context, 'validateContext' will return True if the given context is still valid.
validateContext :: SCardContext -> IO (Bool)
validateContext c = do rt <- liftM fromCLong $ {#call SCardIsValidContext as ^#} c
                       return $ rt == Ok

listReaders_ = {#call SCardListReaders as ^#}

getReaderSize_ :: SCardContext -> IO (Int)
getReaderSize_ c = alloca $ \s -> do listReaders_ c nullPtr nullPtr s
                                     s' <- peek s 
                                     return $fromIntegral s'

-- | Given a context, 'listReaders' lists the connected readers, or gives an error.
listReaders :: SCardContext -> IO (Either SCardStatus [String])
listReaders c = do n <- getReaderSize_ c
                   allocaArray n $ \rs ->
                        do alloca $ \size -> do poke size $fromIntegral n
                                                rt  <- liftM fromCLong $ listReaders_ c nullPtr rs size
                                                n'  <- peek size
                                                rs' <- peekArray (fromIntegral n') rs
                                                if (rt == Ok) then return $ Right $ filter (not . null) $ f rs'
                                                              else return $ Left  $ rt
                                                                   where f  = Data.List.Utils.split "\0" . map castCCharToChar
type SCardHandle = {#type SCARDHANDLE#}

combine :: [SCardProtocol] -> CULong
combine = fromIntegral . foldr ((.|.) . fromEnum) 0

-- | Given a context, the function 'connect' establishes a connection to the reader specified as a 'String'.
-- A 'SCardShare' describing the type of connection can be given as well as a list of possible 'SCardProtocol's.
-- The first connection will power up and perform a reset on the card.
connect :: SCardContext -> String -> SCardShare -> [SCardProtocol] -> IO (Either SCardStatus (SCardProtocol, SCardHandle))
connect c r s ps = let connect_ = {#call SCardConnect as ^#}
                   in alloca $ \p ->
                        alloca $ \h ->
                          withCString r $ \r' ->
                            do rt <- liftM fromCLong $ connect_ c r' (fromIntegral $ fromEnum s) (combine ps) h p
                               p' <- liftM toSCardProtocol $ peek p
                               h' <- peek h
                               if (rt == Ok) then return $Right (p', h')
                                             else return $Left  rt


-- | This function reestablishes a connection to a reader that was previously connected to using 'connect'.
-- In a multi application environment it is possible for an application to reset the card in 'Shared' mode.
-- When this occurs any other application trying to access certain commands will be returned the value 'CardReset'.
-- When this occurs 'reconnect' must be called in order to acknowledge that the card was reset and allow it to change it's state accordingly.
reconnect:: SCardHandle -> SCardShare -> [SCardProtocol] -> SCardAction-> IO (Either SCardStatus SCardProtocol)
reconnect h s ps a = let reconnect_ = {#call SCardReconnect as ^#}
                     in alloca $ \p ->
                           do rt <- liftM fromCLong $ reconnect_ h (fromIntegral $ fromEnum s) (combine ps) (fromIntegral $ fromEnum a) p
                              p' <- liftM toSCardProtocol $ peek p
                              if (rt == Ok) then return $Right p'
                                            else return $Left rt

-- | The function 'disconnect' disconnects a connection, using the specified 'SCardAction'.
disconnect :: SCardHandle -> SCardAction -> IO (SCardStatus)
disconnect h a = let disconnect_ = {#call SCardDisconnect as ^ #}
                 in  liftM fromCLong $ disconnect_ h (fromIntegral $ fromEnum a)

-- | The function 'beginTransaction' establishes a temporary exclusive access mode for doing a series of commands or transaction.
-- You might want to use this when you are selecting a few files and then writing a large file so you can make sure
-- that another application will not change the current file.
-- If another application has a lock on this reader or this application is in 'Exclusive' there will be no action taken.
beginTransaction :: SCardHandle -> IO (SCardStatus)
beginTransaction h = liftM fromCLong $ {#call SCardBeginTransaction as ^#} h

-- | The function 'endTransaction' ends a previously begun transaction.
-- The calling application must be the owner of the previously begun transaction or an error will occur.
-- 'a' can have the following values:
-- 'LeaveCard', 'ResetCard', 'UnpowerCard', 'EjectCard'
-- The disposition action is not currently used in this release. 
endTransaction :: SCardHandle -> SCardAction -> IO (SCardStatus)
endTransaction h a = let end_ = {#call SCardDisconnect as ^ #}
                     in  liftM fromCLong $ end_ h (fromIntegral $ fromEnum a)

-- | The function cancels all pending blocking requests on the 'getStatusChange' function.
cancel :: SCardHandle -> IO (SCardStatus)
cancel h = liftM fromCLong $ {#call SCardCancel as ^ #} h

-- | 'control' sends a command directly to the IFD Handler to be processed by the reader.
control :: SCardHandle -> Int -> [Word8] -> Int -> IO (Either SCardStatus [Word8])
control h op cmd lr = let f  = fromIntegral . length
                          l  = length
                          cp = castPtr
                          fi = fromIntegral
                      in allocaArray (l cmd) $ \sb ->
                           allocaArray lr $ \rb ->
                             alloca $ \lr' ->
                               do rt <- liftM fromCLong $ {#call SCardControl as ^ #} h (fi op) (cp (sb :: Ptr Word8)) (f cmd) (cp (rb :: Ptr Word8)) (fi lr) lr'
                                  lr'' <- peek lr'
                                  res  <- peekArray (fromIntegral lr'') (castPtr rb)
                                  if (rt == Ok) then return $Right (res :: [Word8])
                                                else return $Left rt


-- | The function 'transmit' sends an APDU given as list of 'Word8' to the smartcard / RFID tag connected to by 'connect'.
-- The maximum expected length for the given request is given as lr and the protocol to be used as p.
transmit :: SCardHandle -> [Word8] -> Int -> SCardProtocol -> IO (Either SCardStatus [Word8])
transmit h d lr p = allocaArray (length d) $ \sb ->
                      allocaArray lr $ \rb ->
                         alloca $ \lo ->
                           alloca $ \pci -> do poke pci mkSCardIORequestTO
                                               poke lo $fromIntegral lr
                                               pokeArray sb d
                                               rt <- transmit_ (fromCLong h) (castPtr pci) (castPtr (sb :: Ptr Word8)) ((fromIntegral . length) d) (castPtr pci) (rb :: Ptr Word8) lo
                                               len <- peek lo
                                               res <- peekArray (fromIntegral len) rb
                                               if (rt == Ok) then return $Right res
                                                             else return $Left rt
transmit_ = {#fun SCardTransmit as ^{ `Int'
                                    , castPtr `Ptr SCardIORequest'
                                    , castPtr `Ptr Word8'
                                    , fromIntegral `Int'
                                    , castPtr `Ptr SCardIORequest'
                                    , castPtr `Ptr Word8'
                                    , castPtr `Ptr CULong'
                                    } -> `SCardStatus' fromCLong#}

-- | Given a handle and the lengths for the send and receive buffers this function returns the current status of the reader connected to.
-- It's friendly name will be returned as a 'String'.
-- The current state is returned as a List of 'SCardCardState', and the Protocol as 'SCardProtocol'.
status :: SCardHandle -> Int -> Int -> IO (Either SCardStatus (String, [SCardCardState], SCardProtocol, [Word8]))
status h lr al = allocaArray lr $ \rs ->
                   alloca $ \s ->
                     alloca $ \st ->
                       alloca $ \p ->
                         alloca $ \atr ->
                           alloca $ \atrl -> do poke s $ fromIntegral lr
                                                poke atrl $ fromIntegral al
                                                rt    <- liftM fromCLong $ {#call SCardStatus as ^ #} (fromCLong h) rs s st p atr atrl
                                                rs'   <- peekCAString rs
                                                st'   <- peek st
                                                p'    <- peek p
                                                atrl' <- peek atrl
                                                atr'  <- peekArray (fromIntegral atrl') atr
                                                if (rt == Ok) then return $ Right (rs', (fromSCardStates . fromIntegral) st', toSCardProtocol p', map fromIntegral atr')
                                                              else return $ Left  rt

-- | 'listReaderGroups' returns a list of currently available reader groups.
listReaderGroups :: SCardContext -> Int -> IO (Either SCardStatus [String])
listReaderGroups c lr = allocaArray lr $ \rb ->
                          alloca $ \s -> do poke s $ fromIntegral lr
                                            rt <- liftM fromCLong $ {#call SCardListReaderGroups as ^ #} c rb s
                                            s' <- peek s
                                            rb' <- peekArray (fromIntegral s') rb
                                            if (rt == Ok) then return $ Right $ filter (not . null) $ f rb'
                                                          else return $ Left  $ rt
                                                          where f  = Data.List.Utils.split "\0" . map castCCharToChar
