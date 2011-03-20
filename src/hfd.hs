
-- | Main module

module Main
(
main
)
where

import Data.Tuple.Utils (fst3)
import Data.Maybe
import Data.ByteString (hPut)
import System.IO (Handle, hClose, hSetBinaryMode, hFlush)
import System.Console.Haskeline (getInputLine)
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class(liftIO, MonadIO)
import Control.Exception (bracket)
import Network (withSocketsDo, PortNumber, PortID(PortNumber), HostName, sClose, accept, listenOn)

import App (App, runApp)
import IMsg (IMsg(..), nextIMessage)
import OMsg (OMsg(..), binOMsg)
import UCmd (UCmd(..), parseUCmd)

-- | Entry point
main :: IO ()
main = withSocketsDo $ bracket
  acceptPlayer
  (hClose . fst3)
  (start . fst3)
  where
  start h = do
    hSetBinaryMode h True
    runApp h app

-- | Listen on port, accept just one client and close socket
acceptPlayer :: IO (Handle, HostName, PortNumber)
acceptPlayer = bracket
  (listenOn (PortNumber 7935))
  sClose
  accept

-- | Main app
app :: Handle  -- ^ Output stream to communicate with player
    -> App IO ()
app h = do
  processUntillBreak
  exit <- processUserInput h
  when (not exit) (app h) 

-- | Process player's messages until 'IMsgBreakHitEx' received
--
-- XXX: It should fill file entries table
processUntillBreak :: MonadIO m => App m ()
processUntillBreak = do
  msg <- nextIMessage
  liftIO $ print msg
  case msg of
    IMsgBreakHitEx _ _ _ -> return ()
    _                    -> processUntillBreak

-- | Read user command and process it
processUserInput :: Handle -> App IO Bool
processUserInput h = do
  l <- lift $ getInputLine "hfb> "
  let cmd = l >>= parseUCmd
  if isNothing cmd
    then processUserInput h
    else processCmd h (fromJust cmd)

-- | Actualy process user command
processCmd :: Handle       -- ^ Output stream to player
           -> UCmd         -- ^ User command
           -> App IO Bool  -- ^ whether to exit
processCmd _ UCmdQuit     = return True
processCmd h UCmdContinue = doContinue h
processCmd h UCmdStep     = doStep h
processCmd h UCmdNext     = doNext h
-- processCmd h _            = processUserInput h

-- | Send message to player
sendMsg :: MonadIO m => Handle -> OMsg -> App m ()
sendMsg h msg =  liftIO $ hPut h (binOMsg msg)
              >> hFlush h

-- | Send @continue@ command to player
doContinue :: MonadIO m => Handle -> App m Bool
doContinue h =  sendMsg h OMsgContinue
             >> return False

-- | Send @step@ command to player
doStep :: MonadIO m => Handle -> App m Bool
doStep h =  sendMsg h OMsgStep
             >> return False

-- | Send @next@ command to player
doNext :: MonadIO m => Handle -> App m Bool
doNext h =  sendMsg h OMsgStep
             >> return False

