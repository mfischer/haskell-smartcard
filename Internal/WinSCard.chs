{-# LANGUAGE ForeignFunctionInterface #-}
module Internal.WinSCard ( establishContext
                         , releaseContext
                         , validateContext
                         , listReaders
                         , transmit
                         , connect
                         , disconnect
                         , reconnect
                         , beginTransaction
                         , endTransaction )
where

import Foreign
import Foreign.C
import Foreign.C.String
import Internal.PCSCLite
import Data.List
import Data.List.Utils
import Data.Bits
import Data.ByteString hiding (filter, null, foldr, length, putStrLn)
import Control.Monad

#include <winscard.h>

-- | The 'establishContext' function needs to be invoked first when talking to the pcscd.
establishContext :: SCardScope -> IO (Either SCardStatus SCardContext)
establishContext s = alloca $ \ctx_ptr ->
                        do rt  <- liftM fromCLong $ {#call SCardEstablishContext as ^#} (fromIntegral(fromEnum s)) nullPtr nullPtr ctx_ptr
                           ctx <- peek ctx_ptr
                           if rt == Ok then return $Right ctx
                                       else return $Left rt

-- | the 'releaseContext' function needs to be invoked last in the program, to cleanup.
releaseContext :: SCardContext -> IO (SCardStatus)
releaseContext c = liftM fromCLong $ {#call SCardReleaseContext as ^#} c

-- | Given a context 'validateContext' will return True if the given context is still valid.
validateContext :: SCardContext -> IO (Bool)
validateContext c = do rt <- liftM fromCLong $ {#call SCardIsValidContext as ^#} c
                       return $ rt == Ok

listReaders_ = {#call SCardListReaders as ^#}

getReaderSize_ :: SCardContext -> IO (Int)
getReaderSize_ c = alloca $ \s -> do listReaders_ c nullPtr nullPtr s
                                     s' <- peek s 
                                     return $fromIntegral s'

-- | 'listReaders' lists the connected readers, or gives an error.
listReaders :: SCardContext -> IO (Either SCardStatus [String])
listReaders c = do n <- getReaderSize_ c
                   allocaArray n $ \rs ->
                        do alloca $ \size -> do poke size $fromIntegral n
                                                rt  <- liftM fromCLong $ listReaders_ c nullPtr rs size
                                                n'  <- peek size
                                                rs' <- peekArray (fromIntegral n') rs
                                                if (rt == Ok) then return $ Right $ filter (not . null) $ f rs'
                                                              else return $ Left  $ rt
                                                                   where f  = Data.List.Utils.split "\0" . Prelude.map castCCharToChar
type SCardHandle = {#type SCARDHANDLE#}

combine :: [SCardProtocol] -> CULong
combine = fromIntegral . foldr ((.|.) . fromEnum) 0

-- | 'connect' establishes a connection to the friendly name of the reader specified. The first connection will power up and perform a reset on the card.
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


-- | 'reconnect' reconnects a lost connection, using a SCardAction.
reconnect:: SCardHandle -> SCardShare -> [SCardProtocol] -> SCardAction-> IO (Either SCardStatus SCardProtocol)
reconnect h s ps a = let reconnect_ = {#call SCardReconnect as ^#}
                     in alloca $ \p ->
                           do rt <- liftM fromCLong $ reconnect_ h (fromIntegral $ fromEnum s) (combine ps) (fromIntegral $ fromEnum a) p
                              p' <- liftM toSCardProtocol $ peek p
                              if (rt == Ok) then return $Right p' 
                                            else return $Left rt

-- | 'disconnect' disconnects a connection, using the specified SCardAction
disconnect :: SCardHandle -> SCardAction -> IO (SCardStatus)
disconnect h a = let disconnect_ = {#call SCardDisconnect as ^ #}
                 in  liftM fromCLong $ disconnect_ h (fromIntegral $ fromEnum a)

-- |
beginTransaction :: SCardHandle -> IO (SCardStatus)
beginTransaction h = liftM fromCLong $ {#call SCardBeginTransaction as ^#} h

-- |
endTransaction :: SCardHandle -> SCardAction -> IO (SCardStatus)
endTransaction h a = let end_ = {#call SCardDisconnect as ^ #}
                     in  liftM fromCLong $ end_ h (fromIntegral $ fromEnum a)

-- |
cancel :: SCardHandle -> IO (SCardStatus)
cancel h = liftM fromCLong $ {#call SCardCancel as ^ #} h

{--
control :: SCardHandle -> [Word8] -> Int -> Int -> IO (Either SCardStatus String)
control h s op len = let control_ = {#call SCardControl as ^ #}
                     in useAsCStringLen (pack s) $ \(sb,l) -> allocaArray len $ \r ->
                          alloca $ \l' ->
                            do rt   <- liftM fromCLong $ control_ h (fromIntegral op) (castPtr sb) (fromIntegral l) r len l'
                               len' <- peek l'
                               res  <- peekArray len'
                               if (rt == Ok) then return $Right res
                                             else return $Left rt
--}



-- | 'transmit' sends an APDU to the smartcard / RFID tag connected to by 'connect'
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
