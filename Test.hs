import Lowlevel.WinSCard ( establishContext
                         , releaseContext
                         , listReaders
                         , listReaderGroups
                         , transmit
                         , status
                         , connect)

import Lowlevel.PCSCLite ( SCardScope (UserScope)
                         , SCardStatus (..)
                         , SCardShare (..)
                         , SCardContext (..)
                         , SCardProtocol (..))

import Control.Monad

tryConnection c r'@(r:rs) = do printShow r' "Found readers: "
                               x <- connect c r Shared [T0, T1]
                               either print f x
                               where f (p, h) = do
                                     printShow p "Connected, Protocol is: "
                                     rt <- transmit h [0xff, 0x00, 0x40, 0x50, 0x04, 0x05, 0x05, 0x03, 0x01] 200 T0
                                     either print (`printShow` "Answer is: ") rt
                                     rt' <- status h 200 200
                                     printShow rt' "Querying the status: "
                                     rt'' <- listReaderGroups c 200
                                     printShow rt'' "Listing the reader groups: "
                                     return ()


printShow :: Show a => a -> String -> IO ()
printShow s = putStrLn . (flip (++) . show) s

withContext :: SCardScope -> (SCardContext -> IO a) -> IO (Either String SCardStatus)
withContext s f = establishContext s >>= \r -> either (return . Left . show) (\c -> f c >> liftM Right (releaseContext c)) r

main =  withContext UserScope $ \c ->
          do rs <- listReaders c
             either print (tryConnection c) rs
             return ()
