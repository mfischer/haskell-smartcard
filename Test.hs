import Internal.WinSCard ( establishContext
                         , releaseContext
                         , listReaders)
import Internal.PCSCLite ( SCardScope (UserScope)
                         , SCardError (..))

main = do putStrLn "Trying to establish context ..."
          ret <- establishContext UserScope
          case ret of
               (Left s)     -> putStrLn $show s
               (Right ctx') -> do { putStrLn "Trying to release the context ..."
                                 ; rs  <- listReaders ctx'
                                 ; case rs of
                                        Right rs' ->  putStrLn $ "Found readers: " ++ show rs' 
                                        Left err  ->  putStrLn $ show err
                                 ; res <- releaseContext ctx'
                                 ; return ()
                                 }
          return ()

