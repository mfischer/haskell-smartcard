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

tryConnection c (r:rs) = do putStrLn $"Found readers: " ++ show (r:rs)
                            x <- connect c r Shared [T0, T1]
                            case x of
                                 Left  err    -> putStrLn $show err
                                 Right (p, h) -> do { putStrLn $"Connected, Protocol is: " ++ show p
                                                   ; rt <- transmit h [0xff, 0x00, 0x40, 0x50, 0x04, 0x05, 0x05, 0x03, 0x01] 200 T0
                                                   ; case rt of
                                                          Left s  -> print s
                                                          Right a -> putStrLn $ "Answer is: " ++ show a
                                                   ; rt' <- status h 200 200
                                                   ; case rt' of
                                                          Left  s -> print s
                                                          Right a -> putStrLn $ "Queriying the status: " ++ show rt'
                                                   ; rt'' <- listReaderGroups c 200
                                                   ; putStrLn $ "Listing the reader groups: " ++ show rt''
                                                   ; return () }

withContext :: SCardScope -> (SCardContext -> IO a) -> IO (Either String SCardStatus)
withContext s f = establishContext s >>= \r -> either (return . Left . show) (\c -> f c >> liftM Right (releaseContext c)) r

main =  withContext UserScope $ \c ->
          do rs <- listReaders c
             either print (tryConnection c) rs
             return ()
