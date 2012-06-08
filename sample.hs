import Control.Monad (liftM)
import Control.Monad.State
import qualified Data.ByteString as B
import Network.Protocol.ZigBee.ZNet25 as Z
import System.Environment
import System.Exit
import System.IO

main = do
    args <- getArgs
    when (length args /= 2) $ do
      progName <- getProgName
      hPutStrLn stderr $ "usage: " ++ progName ++ " <modem_path> <command_name>"
      exitFailure

    let (modemPath:cmdName:[]) = args

    -- Open modem in read/write mode
    withBinaryFile modemPath ReadWriteMode $ \h -> do
      -- The API interface is binary -- disable buffering
      hSetBuffering h NoBuffering

      -- Encode and send an AT command
      mapM_ (B.hPut h) $ Z.encode $ Z.ATCommand 1 (Z.commandName cmdName) B.empty

      -- Run the decoder until an AT command response is received
      waitResponse h Z.initDecode

  where
    waitResponse h ds = do
      bs <- B.hGetSome h 256
      let (rs, ds') = runState (Z.decode bs) ds
      done <- liftM or $ mapM printResponse rs
      if done
        then return ()
        else waitResponse h ds'

    printResponse (Right f@(ATCommandResponse _ _ _ _)) =
      putStrLn (show f) >> return True

    printResponse (Right f) =
      putStrLn (show f) >> return False

    printResponse (Left errStr) =
      hPutStrLn stderr errStr >> return False

