{-# LANGUAGE ForeignFunctionInterface #-}
module Internal.WinSCard ( establishContext
                         , releaseContext
                         , validateContext
                         , listReaders )
where

import Foreign
import Foreign.C
import Foreign.C.String
import Internal.PCSCLite
import Data.List.Utils

#include <winscard.h>

establishContext_ :: CULong -> Ptr () -> Ptr () -> Ptr CLong -> IO CLong
establishContext_ = {#call SCardEstablishContext as ^#}


-- | the 'establishContext' function needs to be invoked first when talking to the pcscd.
establishContext :: SCardScope -> IO (Either SCardError SCardContext)
establishContext s = alloca $ \ctx_ptr -> do rt  <- establishContext_ (fromIntegral(fromEnum s)) nullPtr nullPtr ctx_ptr
                                             ctx <- peek ctx_ptr
                                             return $ret rt ctx where
                                                    ret = \rt' ctx' -> if rt' == 0 then Right ctx'
                                                                                   else Left $toSCardError rt'

releaseContext_ = {#fun SCardReleaseContext as ^ {`Int'} -> `Int'#}

-- | the 'releaseContext' function needs to be invoked last in the program, to cleanup.
releaseContext :: SCardContext -> IO (SCardError)
releaseContext c = do rt <- releaseContext_ $fromIntegral c
                      return $toSCardError $fromIntegral rt

validateContext_ = {#call SCardIsValidContext as ^#}

validateContext :: SCardContext -> IO (Bool)
validateContext c = do rt <- validateContext_ c                     
                       return $rt == 0

listReaders_ = {#call SCardListReaders as ^#}

getReaderSize_ :: SCardContext -> IO (Int)
getReaderSize_ c = alloca $ \s -> do listReaders_ c nullPtr nullPtr s
                                     s' <- peek s 
                                     return $fromIntegral s'

listReaders :: SCardContext -> IO (Either SCardError [String])
listReaders c = do n <- getReaderSize_ c
                   allocaArray n $ \rs ->
                        do alloca $ \size -> do poke size $fromIntegral n
                                                rt <- listReaders_ c nullPtr rs size 
                                                n'  <- peek size
                                                rs' <- peekArray (fromIntegral n') rs
                                                if (rt == 0) then return $ Right $ filter (/="") $ Data.List.Utils.split "\0" $ Prelude.map castCCharToChar rs'
                                                             else return $ Left  $ toSCardError rt
