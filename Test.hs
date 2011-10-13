import Internal.WinSCard ( establishContext
                         , releaseContext
                         , listReaders
                         , transmit
                         , status
                         , connect)

import Internal.PCSCLite ( SCardScope (UserScope)
                         , SCardStatus (..)
                         , SCardShare (..)
                         , SCardProtocol (..))
import Numeric (showHex)

tryConnection c (r:rs) = do putStrLn $"Found readers: " ++ show (r:rs)
                            x <- connect c r Shared [T0, T1]
                            case x of
                                 Left  err    -> putStrLn $show err
                                 Right (p, h) -> do { putStrLn $"Connected, Protocol is: " ++ show p
                                                   ; rt <- transmit h [0xff, 0x00, 0x40, 0x50, 0x04, 0x05, 0x05, 0x03, 0x01] 200 T0
                                                   ; case rt of
                                                          Left s -> putStrLn $show s
                                                          Right a -> putStrLn $"Answer is: " ++ (show a)
                                                   ; rt' <- status h 200 200
                                                   ; putStrLn $ show rt'
                                                   ; return () }

main = do putStrLn "Trying to establish context ..."
          ret <- establishContext UserScope
          case ret of
               (Left s)     -> do putStrLn $show s
               (Right ctx') -> do { putStrLn "Trying to release the context ..."
                                 ; rs  <- listReaders ctx'
                                 ; case rs of
                                        Right rs' ->  tryConnection ctx' rs'
                                        Left  err ->  putStrLn $ show err
                                 ; res <- releaseContext ctx'
                                 ; return ()
                                 }
          return ()

