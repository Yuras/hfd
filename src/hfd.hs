
-- | Main module

module Main
(
main
)
where

import Data.Tuple.Utils (fst3)
import System.IO (Handle, hClose, hSetBinaryMode)
import System.Console.Haskeline (getInputLine)
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class(liftIO, MonadIO)
import Control.Exception (bracket)
import Network (withSocketsDo, PortNumber, PortID(PortNumber), HostName, sClose, accept, listenOn)

import App (App, runApp)
import IMsg (IMsg(..), nextIMessage)
import UCmd (parseUCmd)

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

-- | Read user command and process it
processUserInput :: Handle -> App IO Bool
processUserInput _ = do
  l <- lift $ getInputLine "hfb> "
  liftIO $ print l
  let cmd = l >>= parseUCmd
  liftIO $ print cmd
  return True

-- | Process player's messages until IMsgBreakHitEx received
--
-- XXX: It should fill file entries table
processUntillBreak :: MonadIO m => App m ()
processUntillBreak = do
  msg <- nextIMessage
  liftIO $ print msg
  case msg of
    IMsgBreakHitEx _ _ _ -> return ()
    _                    -> processUntillBreak

