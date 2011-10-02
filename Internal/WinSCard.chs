{-# LANGUAGE ForeignFunctionInterface #-}
module Internal.WinSCard ( establishContext
                         , releaseContext
                         , validateContext
                         , listReaders
                         , connect )
where

import Foreign
import Foreign.C
import Foreign.C.String
import Internal.PCSCLite
import Data.List.Utils
import Data.Bits
import Control.Monad

#include <winscard.h>

establishContext_ :: CULong -> Ptr () -> Ptr () -> Ptr CLong -> IO CLong
establishContext_ = {#call SCardEstablishContext as ^#}


-- | The 'establishContext' function needs to be invoked first when talking to the pcscd.
establishContext :: SCardScope -> IO (Either SCardStatus SCardContext)
establishContext s = alloca $ \ctx_ptr ->
                        do rt  <- liftM fromCLong $ establishContext_ (fromIntegral(fromEnum s)) nullPtr nullPtr ctx_ptr
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
