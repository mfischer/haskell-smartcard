import System.Smartcard.Lowlevel.WinSCard ( establishContext
                                           , releaseContext
                                           , listReaders
                                           , listReaderGroups
                                           , transmit
                                           , status
                                           , getAttribute
                                           , connect)

import System.Smartcard.Lowlevel.PCSCLite ( SCardScope (UserScope)
                                          , SCardStatus (..)
                                          , SCardShare (..)
                                          , SCardContext (..)
                                          , SCardProtocol (..))

import System.Smartcard.Lowlevel.Reader   ( AttrTag (..)
                                          , mkRequest
                                          )

import Control.Monad

tryConnection c r'@(r:rs) = do printShow r' "Found readers: "
                               x <- connect c r Shared [T0, T1]
                               either print f x
                               where f (p, h) = do
                                     printShow p "Connected, Protocol is: "
                                     rt <- transmit h [0xff, 0x00, 0x40, 0x50, 0x04, 0x05, 0x05, 0x03, 0x01] 200 T0
                                     either print (`printShow` "Answer is: ") rt
                                     rt' <- status h 200 200
                                     either print (`printShow` "Queriying the status: ") rt'
                                     rt'' <- listReaderGroups c 200
                                     either print (`printShow` "Listing the reader groups: ") rt''
                                     rt''' <- getAttribute h (mkRequest VendorName) 200
                                     either print (`printShow` "Got attribute \"VendorName\"=") rt'''
                                     return ()


printShow :: Show a => a -> String -> IO ()
printShow s = putStrLn . (flip (++) . show) s

withContext :: SCardScope -> (SCardContext -> IO a) -> IO (Either String SCardStatus)
withContext s f = establishContext s >>= \r -> either (return . Left . show) (\c -> f c >> liftM Right (releaseContext c)) r

main =  withContext UserScope $ \c ->
          do rs <- listReaders c
             either print (tryConnection c) rs
             return ()
