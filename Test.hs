import Internal.WinSCard ( establishContext
                         , releaseContext
                         , listReaders
                         , connect)

import Internal.PCSCLite ( SCardScope (UserScope)
                         , SCardStatus (..)
                         , SCardShare (..)
                         , SCardProtocol (..))


tryConnection c (r:rs) = do putStrLn $"Found readers: " ++ show (r:rs)
                            x <- connect c r Shared [T0, T1]
                            case x of
                                 Left  err    -> putStrLn $show err
                                 Right (p, h) -> putStrLn $"Connected, Protocol is: " ++ show p

main = do putStrLn "Trying to establish context ..."
          ret <- establishContext UserScope
          case ret of
               (Left s)     -> putStrLn $show s
               (Right ctx') -> do { putStrLn "Trying to release the context ..."
                                 ; rs  <- listReaders ctx'
                                 ; case rs of
                                        Right rs' ->  tryConnection ctx' rs'
                                        Left  err ->  putStrLn $ show err
                                 ; res <- releaseContext ctx'
                                 ; return ()
                                 }
          return ()

