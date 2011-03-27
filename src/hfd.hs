
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
import Control.Monad (unless, liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (get)
import Control.Monad.IO.Class(liftIO, MonadIO)
import Control.Exception (bracket)
import Network (withSocketsDo, PortNumber, PortID(PortNumber), HostName,
                sClose, accept, listenOn)

import App (App, runApp, FileEntry(..), addFileEntry, AppState(..), setLastCmd)
import IMsg (IMsg(..), nextIMessage, AMF(..), AMFValue(..), amfUndecoratedName)
import OMsg (OMsg(..), binOMsg)
import UCmd (UCmd(..), parseUCmd, InfoCmd(..))

-- | Entry point
main :: IO ()
main = withSocketsDo $ printHello >> bracket
  acceptPlayer
  (hClose . fst3)
  (start . fst3)
  where
  start h = do
    putStrLn "Enter \'help\' for list of commands"
    hSetBinaryMode h True
    runApp h app

-- | Print hello message
printHello :: IO ()
printHello = do
  putStrLn "HFB: Flash Debugger version 0.0.1"
  putStrLn "Copyright (c) 2011 Yuras Shumovich"
  putStrLn "mailto:shumovichy@gmail.com"

printHelp :: IO ()
printHelp = do
  printHello
  putStrLn "List of commands:"
  putStrLn "\thelp                          print this help"
  putStrLn "\tquit                          quit hfd"
  putStrLn "\tcontinue                      continue execution until breakpoint hit"
  putStrLn "\tstep                          continue execution until different source line reached"
  putStrLn "\tnext                          continue execution until next source line reached"
  putStrLn "\tinfo files                    show all source files"
  putStrLn "\tbreakpoint <fileID>:<line>    set breakpoint at the location, e.g. \'b #1:23\'"
  putStrLn "\t                              use \'info files\' to get fileID"
  putStrLn "\tprint <name>[.name]*          inspect variables"
  putStrLn "Shortcuts are allowed, e.g. \'c\', \'co\', \'cont\', etc will mean \'continue\'"

-- | Listen on port, accept just one client and close socket
acceptPlayer :: IO (Handle, HostName, PortNumber)
acceptPlayer = bracket
  (listenOn (PortNumber 7935))
  sClose
  (\s -> putStrLn "Waiting for player..." >> accept s)

-- | Main app
app :: App IO ()
app = do
  processUntillBreak
  doSetDebuggerOption "break_on_fault" "on"
  -- doSetDebuggerOption "disable_script_stuck" "on"
  -- doSetDebuggerOption "disable_script_stuck_dialog" "on"
  -- doSetDebuggerOption "enumerate_override" "on"
  doSetDebuggerOption "notify_on_failure" "on"
  -- doSetDebuggerOption "invoke_setters" "on"
  -- doSetDebuggerOption "swf_load_messages" "on"
  loop
  where
  loop = do
    exit <- processUserInput
    unless exit (processUntillBreak >> loop)

-- | Process player's messages until 'IMsgBreakHitEx' received
processUntillBreak :: App IO ()
processUntillBreak = do
  msg <- nextMsg
  case msg of
    IMsgBreakHitEx _ _ _       -> printSourceLine msg
    IMsgSwdFileEntry _ _ _ _ _ -> processFileEntry msg >> processUntillBreak
    IMsgException _ _ _        -> processException msg >> processUntillBreak
    _                          -> processUntillBreak

-- | Print information about exception
processException :: MonadIO m => IMsg -> App m ()
processException (IMsgException _ msg _) = do
  liftIO $ putStrLn " [exception]"
  liftIO $ putStrLn msg
processException _ = error "processException: something is wrong"

-- | Print current source line
printSourceLine :: IMsg -> App IO ()
printSourceLine (IMsgBreakHitEx file line _) = do
  files <- lift . lift $ fmap asFiles get
  let mln = srcLine files
  if isJust mln
    then liftIO $ putStrLn $ " " ++ show line ++ ": " ++ fromJust mln
    else liftIO $ putStrLn "No source"
  where
  srcLine files = do
    FileEntry _ content <- lookup (fromIntegral file) files
    let lln = take 1 $ drop (fromIntegral line - 1) content
    if null lln
      then Nothing
      else Just $ head lln
printSourceLine _ = error "printSourceLine: something is wrong..."

-- | Read file content and add new file entry
processFileEntry :: MonadIO m => IMsg -> App m ()
processFileEntry (IMsgSwdFileEntry idi _ nm _ _) = do
  content <- liftIO readFile'
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
processCmd UCmdFinish             = doFinish
processCmd (UCmdInfo cmd)         = processInfoCmd cmd >> processUserInput
processCmd (UCmdPrint v)          = doPrint v >> processUserInput
processCmd (UCmdBreakpoint fl ln) = doSetBreakpoint fl ln >> processUserInput
processCmd UCmdTest               = doGetFrame >> processUserInput
processCmd UCmdHelp               = liftIO printHelp >> processUserInput

-- | Set debuger option
doSetDebuggerOption :: MonadIO m => String -> String -> App m ()
doSetDebuggerOption op val = do
  sendMsg (OMsgSetDebuggerOptions op val)
  msg <- nextMsg
  case msg of
    IMsgDebuggerOption _ _ -> return ()
    _ -> liftIO $ putStrLn "doSetDebuggerOption: Unexpected message from player"

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
doPrint :: MonadIO m => [String] -> App m ()
doPrint v = do
  _ <- doGetFrame
  msg <- nextMsg
  case msg of
    IMsgFunctionFrame _ _ _ vs -> findValue vs
    _ -> liftIO $ putStrLn "doPrint: Unexpected message from player"
  where
  findValue vs = do
    let vs' = filter (\a -> amfUndecoratedName a == head v) vs
    liftIO $ print vs'
    unless (null vs') (doPrintProps (tail v) (amfValue $ head vs'))

-- | Print object properties as requested
doPrintProps :: MonadIO m => [String] -> AMFValue -> App m ()
doPrintProps [] _ = return ()
doPrintProps (name:ns) (AMFObject ptr _ _ _ _) = do
  sendMsg (OMsgGetField ptr "")
  msg <- nextMsg
  case msg of
    IMsgGetField _ vs ->
      let vs' = filter (\a -> amfUndecoratedName a == name) vs in
      case vs' of
        [] -> liftIO $ putStrLn "Not found"
        [v] -> if null ns
                 then liftIO $ print v
                 else doPrintProps ns (amfValue v)
        _ -> liftIO $ putStrLn "Multiple"
    _ -> return ()
doPrintProps _ _ = liftIO $ putStrLn "Not found"

-- | Process @info@ command
processInfoCmd :: MonadIO m => InfoCmd -> App m ()
processInfoCmd ICFiles = printFiles
  where
  printFiles = do
    files <- lift . lift $ liftM asFiles get
    liftIO $ mapM_ printFile files
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

-- | Send @finish@ command to player
doFinish :: MonadIO m => App m Bool
doFinish =  sendMsg OMsgFinish
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
-- and prints traces
nextMsg :: MonadIO m => App m IMsg
nextMsg = do
  msg <- nextIMessage
  -- liftIO $ print msg
  case msg of
    IMsgProcessTag -> sendMsg OMsgProcessTag >> nextMsg
    IMsgTrace str  -> liftIO (putStrLn $ " [trace] " ++ str) >> nextMsg
    _              -> return msg

