
-- | Main module

module Main
(
main
)
where

import Data.Tuple.Utils (fst3)
import Data.Maybe
import Data.Word (Word32, Word16)
import Data.ByteString.Char8 (unpack)
import System.IO (Handle, hClose, hSetBinaryMode)
import System.Console.Haskeline (getInputLine)
import Control.Monad (unless, liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (get)
import Control.Monad.IO.Class(liftIO, MonadIO)
import Control.Exception (bracket)
import Network (withSocketsDo, PortNumber, PortID(PortNumber), HostName,
                sClose, accept, listenOn)

import App (App, runApp, FileEntry(..), addFileEntry, getFileEntry,
            AppState(..), setLastCmd, setStack, Breakpoint(..), removeBreakpoint)
import IMsg (IMsg(..))
import UCmd (UCmd(..), parseUCmd, InfoCmd(..))
import Print (doPrint)
import Proto (setDebuggerOption, nextMsg, execContinue, execNext, execStep, execFinish,
              setBreakpoint, deleteBreakpoint)

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

-- | Print list of commands
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
  putStrLn "\tinfo breakpoints              show all breakpoints"
  putStrLn "\tbreakpoint <fileID>:<line>    set breakpoint at the location, e.g. \'b #1:23\'"
  putStrLn "\t                              use \'info files\' to get fileID"
  putStrLn "\tdelete <breakpointID>         delete breakpoint by ID"
  putStrLn "\t                              use \'info breakpoints\' to get breakpoint ID"
  putStrLn "\tdelete                        delete all breakpoints"
  putStrLn "\tprint <name>[.name]*          inspect variables"
  putStrLn "\tbacktrace (bt)                show call stack"
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
  setDebuggerOption "break_on_fault" "on"
  -- doSetDebuggerOption "disable_script_stuck" "on"
  -- doSetDebuggerOption "disable_script_stuck_dialog" "on"
  -- doSetDebuggerOption "enumerate_override" "on"
  setDebuggerOption "notify_on_failure" "on"
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
    IMsgBreakHitEx _ _ stack   -> processBreak stack   >> printSourceLine msg
    IMsgSwdFileEntry _ _ _ _ _ -> processFileEntry msg >> processUntillBreak
    IMsgException _ _ _        -> processException msg >> processUntillBreak
    _                          -> processUntillBreak

-- | Save current stack
processBreak :: Monad m => [(Word16, Word16, Word32, String)] -> App m ()
processBreak = setStack . map toStack
  where
  toStack (fl, ln, _, fn) = (fromIntegral fl, fromIntegral ln, fn)

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
    then liftIO (putStrLn "Unknown command") >> processUserInput
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
processCmd UCmdContinue           = execContinue >> return False
processCmd UCmdStep               = execStep >> return False
processCmd UCmdNext               = execNext >> return False
processCmd UCmdFinish             = execFinish >> return False
processCmd (UCmdInfo cmd)         = processInfoCmd cmd >> processUserInput
processCmd (UCmdPrint v)          = doPrint v >> processUserInput
processCmd (UCmdBreakpoint fl ln) = setBreakpoint fl ln >> processUserInput
processCmd UCmdStack              = printStack >> processUserInput
processCmd UCmdList               = listSource >> processUserInput
processCmd (UCmdDelete (Just iD)) = deleteBP iD >> processUserInput
processCmd (UCmdDelete Nothing)   = deleteAll >> processUserInput
processCmd UCmdTest               = processUserInput
processCmd UCmdHelp               = liftIO printHelp >> processUserInput

-- | Delete breakpoint by id
deleteBP :: MonadIO m => Int -> App m ()
deleteBP iD = do
  bs <- lift . lift $ liftM asBreaks get
  let bp = lookup iD bs
  if isJust bp
    then do
      let Breakpoint fl ln = fromJust bp
      deleteBreakpoint fl ln
      removeBreakpoint iD
    else liftIO $ putStrLn "Unknown breakpoint id. Type \"info breakpoints\" for list of all breakpoints"

-- | Delete all breakpoints
deleteAll :: MonadIO m => App m ()
deleteAll = do
  bs <- lift . lift $ liftM asBreaks get
  mapM_ (deleteBP . fst) bs

-- | Print source around current position
listSource :: MonadIO m => App m ()
listSource = do
  stack <- lift . lift $ liftM asStack get
  if null stack
    then liftIO $ putStrLn "No source"
    else print' $ head stack
  where
  print' (fl, ln, _) = do
    fs <- lift . lift $ liftM asFiles get
    let f = lookup fl fs
    if isNothing f
      then liftIO $ putStrLn "No source"
      else liftIO $ mapM_ printLine $ take 11 $ drop (ln - 6) (zip allLines $ feContent $ fromJust f)
  printLine (ln, cont) = putStrLn $ " " ++ show ln ++ ": " ++ cont
  allLines :: [Int]
  allLines = [1..]

-- | Print current stack
printStack :: MonadIO m => App m ()
printStack = do
  state <- lift . lift $ get
  let stack = asStack state
  let files = asFiles state
  liftIO $ mapM_ (print' files) stack
  where
  print' fs (fl, ln, fn) = putStrLn $ fn ++ "() at " ++ file fs fl ++ ":" ++ show ln
  file fs fl = let fe = lookup fl fs in
    if isJust fe
      then fePath (fromJust fe)
      else "(no source)"

-- | Process @info@ command
processInfoCmd :: MonadIO m => InfoCmd -> App m ()
processInfoCmd ICFiles = printFiles
  where
  printFiles = do
    files <- lift . lift $ liftM asFiles get
    liftIO $ mapM_ printFile files
  printFile (idi, FileEntry name _) =
    putStrLn $ "#" ++ show idi ++ ": " ++ name
processInfoCmd ICBreakpoints = do
  bs <- lift . lift $ liftM asBreaks get
  mapM_ printBP bs
  where
  printBP (iD, Breakpoint fl ln) = do
    fe <- liftM fromJust $ getFileEntry fl
    liftIO $ putStrLn $ " " ++ show iD ++ "\t: " ++ fePath fe ++ " at line " ++ show ln

