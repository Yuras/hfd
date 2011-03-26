
-- | Main module

module Main
(
main
)
where

import Data.Tuple.Utils (fst3)
import Data.Maybe
import Data.ByteString (hPut)
import Data.ByteString.Char8 (unpack)
import System.IO (Handle, hClose, hSetBinaryMode, hFlush)
import System.Console.Haskeline (getInputLine)
import Control.Monad (when, liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (get)
import Control.Monad.IO.Class(liftIO, MonadIO)
import Control.Exception (bracket)
import Network (withSocketsDo, PortNumber, PortID(PortNumber), HostName,
                sClose, accept, listenOn)

import App (App, runApp, FileEntry(..), addFileEntry, AppState(..), setLastCmd)
import IMsg (IMsg(..), nextIMessage, AMF(..))
import OMsg (OMsg(..), binOMsg)
import UCmd (UCmd(..), parseUCmd, InfoCmd(..))

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
app :: App IO ()
app = do
  processUntillBreak
  -- sendMsg (OMsgSetDebuggerOptions "break_on_fault" "on")
  -- sendMsg (OMsgSetDebuggerOptions "disable_script_stuck" "on")
  -- sendMsg (OMsgSetDebuggerOptions "disable_script_stuck_dialog" "on")
  -- sendMsg (OMsgSetDebuggerOptions "enumerate_override" "on")
  sendMsg (OMsgSetDebuggerOptions "notify_on_failure" "on")
  -- sendMsg (OMsgSetDebuggerOptions "invoke_setters" "on")
  -- sendMsg (OMsgSetDebuggerOptions "swf_load_messages" "on")
  loop
  where
  loop = do
    exit <- processUserInput
    when (not exit) (processUntillBreak >> loop)

-- | Process player's messages until 'IMsgBreakHitEx' received
processUntillBreak :: App IO ()
processUntillBreak = do
  msg <- nextMsg
  case msg of
    IMsgBreakHitEx _ _ _       -> printSourceLine msg >> return ()
    IMsgSwdFileEntry _ _ _ _ _ -> processFileEntry msg >> processUntillBreak
    _                          -> processUntillBreak

-- | Print current source line
printSourceLine :: IMsg -> App IO ()
printSourceLine (IMsgBreakHitEx file line _) = do
  files <- lift . lift $ fmap asFiles get
  let mln = srcLine files
  if isJust mln
    then liftIO $ putStrLn $ fromJust mln
    else liftIO $ putStrLn "No source"
  where
  srcLine files = do
    FileEntry _ content <- lookup (fromIntegral file) files
    let lln = take 1 $ drop (fromIntegral line - 1) content
    if length lln == 0
      then Nothing
      else Just $ head lln
printSourceLine _ = error "printSourceLine: something is wrong..."

-- | Read file content and add new file entry
processFileEntry :: MonadIO m => IMsg -> App m ()
processFileEntry (IMsgSwdFileEntry idi _ nm _ _) = do
  content <- liftIO readFile'
  -- liftIO $ print content
  addFileEntry (fromIntegral idi, FileEntry name (lines content))
  where
  name = unpack nm
  path = map fixup name
  fixup ';' = '/'
  fixup ch = ch
  readFile' = catch (readFile path) (const $ return "")
processFileEntry _ = error "processFileEntry: something is wrong..."

-- | Read user command and process it
processUserInput :: App IO Bool
processUserInput = do
  l <- lift $ getInputLine "hfb> "
  let cmd = l >>= parseUCmd
  liftIO $ print cmd
  setLastCmd cmd
  if isNothing cmd
    then processUserInput
    else processCmd (fromJust cmd)

-- | Actualy process user command
processCmd :: UCmd         -- ^ User command
           -> App IO Bool  -- ^ whether to exit
processCmd UCmdEmpty      = do
  cmd <- lift . lift $ liftM asLastCmd get
  if isJust cmd
    then processCmd (fromJust cmd)
    else processUserInput
processCmd UCmdQuit               = return True
processCmd UCmdContinue           = doContinue
processCmd UCmdStep               = doStep
processCmd UCmdNext               = doNext
processCmd (UCmdInfo cmd)         = processInfoCmd cmd >> processUserInput
processCmd (UCmdPrint v)          = doPrint v >> processUserInput
processCmd (UCmdBreakpoint fl ln) = doSetBreakpoint fl ln >> processUserInput
processCmd UCmdTest               = doGetFrame >> processUserInput

-- | Set breakpoint
doSetBreakpoint :: MonadIO m => Int -> Int -> App m Bool
doSetBreakpoint fl ln = do
  sendMsg (OMsgSetBreakpoint (fromIntegral fl) (fromIntegral ln))
  msg <- nextMsg
  case msg of
    IMsgBreakpoints _ -> return ()
    _ -> liftIO $ putStrLn "doSetBreakpoint: Unexpected message from player"
  return True;

-- | Print variable
doPrint :: MonadIO m => String -> App m ()
doPrint v = do
  _ <- doGetFrame
  msg <- nextMsg
  case msg of
    IMsgFunctionFrame _ _ _ vs -> findValue vs
    _ -> liftIO $ putStrLn "doPrint: Unexpected message from player"
  where
  findValue vs = do
    let vs' = filter (\a -> amfName a == v) vs
    liftIO $ print vs'

-- | Process @info@ command
processInfoCmd :: MonadIO m => InfoCmd -> App m ()
processInfoCmd ICFiles = printFiles
  where
  printFiles = do
    files <- lift . lift $ liftM asFiles get
    liftIO $ mapM_ (printFile) files
  printFile (idi, FileEntry name _) =
    putStrLn $ "#" ++ show idi ++ ": " ++ name

-- | Get function frame
doGetFrame :: MonadIO m => App m Bool
doGetFrame = do
  sendMsg (OMsgGetFunctionFrame 0)
  return True;

-- | Send @continue@ command to player
doContinue :: MonadIO m => App m Bool
doContinue =  sendMsg OMsgContinue
           >> return False

-- | Send @step@ command to player
doStep :: MonadIO m => App m Bool
doStep =  sendMsg OMsgStep
       >> return False

-- | Send @next@ command to player
doNext :: MonadIO m => App m Bool
doNext =  sendMsg OMsgNext
       >> return False

-- | Send message to player
sendMsg :: MonadIO m => OMsg -> App m ()
sendMsg msg = do
  h <- lift . lift $ liftM asHandle get
  liftIO $ hPut h (binOMsg msg) >> hFlush h
  return ()

-- | Take next message
-- This function is just a wrapper around nextIMessage,
-- the only difference is that it responses to `IMsgProcessTag` message
nextMsg :: MonadIO m => App m IMsg
nextMsg = do
  msg <- nextIMessage
  liftIO $ print msg
  case msg of
    IMsgProcessTag -> sendMsg OMsgProcessTag >> nextMsg
    _              -> return msg

