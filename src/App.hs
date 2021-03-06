
-- | This module defines App monad

module App
(
App,
runApp,
AppState(..),
FileEntry(..),
StackFrame,
Breakpoint(..),
setStack,
addFileEntry,
getFileEntry,
setLastCmd,
setBreakpoints,
removeBreakpoint,
newBreakId
)
where

import Data.ByteString (ByteString)
import Data.Iteratee (Iteratee, run)
import Data.Iteratee.IO (enumHandle)
import System.IO (Handle)
import System.Environment (getEnv)
import System.Console.Haskeline (InputT, runInputT, Settings(..), CompletionFunc, simpleCompletion)
import Control.Monad (liftM)
import Control.Monad.Trans.State (StateT, evalStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class(MonadIO)

import UCmd (UCmd(..), suggestBaseCmd)
import Inst ()  -- 'MonadCatchIO' instance for 'InputT'

-- | App monad
type App m = Iteratee ByteString (InputT (StateT AppState m))

-- | Run App monad
runApp :: Handle                -- ^ Input/Output stream to be used
                                -- to communicate with player
       -> App IO a              -- ^ Application
       -> IO a
runApp h app = do
  home <- getEnv "HOME"
  let history = home ++ "/.hfd_history"
  flip evalStateT (defaultState h) $
    runInputT (hlSettings history) (enumHandle 1 h app >>= run)

-- | Make haskeline settings
hlSettings :: MonadIO m => String -> Settings (StateT AppState m)
hlSettings history = Settings {historyFile = Just history, complete = completeFunc, autoAddHistory = True}

-- | Complete base commands
completeFunc :: MonadIO m => CompletionFunc (StateT AppState m)
completeFunc (s, _) = do
  let s' = reverse s
  let sgs = suggestBaseCmd s'
  return (s, map (simpleCompletion . drop (length s')) sgs)

-- | Application state
data AppState = AppState {
  asFiles :: [(Int, FileEntry)],  -- ^ map of file entries
  asStack :: [StackFrame],        -- ^ current stack if any
  asBreaks :: [(Int, Breakpoint)], -- ^ list of breakpoints
  asLastBreakId :: Int,           -- ^ last used breakpoint id
  asLastCmd :: Maybe UCmd,        -- ^ last command entered by user
  asHandle :: Handle              -- ^ handle to player
} deriving Show

-- | Represents single breakpoint
data Breakpoint = Breakpoint {
  bpFileId :: Int,
  bpLine :: Int
} deriving Show

-- | Set breakpoints
setBreakpoints :: Monad m => [(Int, Breakpoint)] -> App m ()
setBreakpoints bs = do
  state <- lift . lift $ get
  lift . lift $ put state {asBreaks = bs}

-- | Remove breakpoint by id
removeBreakpoint :: Monad m => Int -> App m ()
removeBreakpoint iD = do
  state <- lift . lift $ get
  let bs = filter ((/= iD) . fst) $ asBreaks state
  setBreakpoints bs

-- | Allocate id for new breakpoint
newBreakId :: Monad m => App m Int
newBreakId = do
  state <- lift . lift $ get
  let newId = asLastBreakId state + 1
  lift . lift $ put state {asLastBreakId = newId}
  return newId

-- | Default application state contains nothing
defaultState :: Handle -> AppState
defaultState = AppState [] [] [] 0 Nothing

-- | File entry represents one source file
data FileEntry = FileEntry {
  fePath :: String,      -- ^ Path to file as recieved from player,
                         -- e.g. @\/home\/user\/proj;com\/example;Main.as@
  feContent :: [String]  -- ^ File content
} deriving Show

-- | Represents stack frame
-- file id, line and function name
type StackFrame = (Int, Int, String)

-- | Returns `FileEntry` by id
getFileEntry :: Monad m => Int -> App m (Maybe FileEntry)
getFileEntry iD = do
  fs <- lift . lift $ liftM asFiles get
  return $ lookup iD fs

-- | Set current stack
setStack :: Monad m => [StackFrame] -> App m ()
setStack st = do
  state <- lift $ lift get
  lift . lift $ put state {asStack = st}

-- | Add new file entry to app state
--
-- XXX: check that ids are unique
addFileEntry :: Monad m => (Int, FileEntry) -> App m ()
addFileEntry fe = do
  state <- lift $ lift get
  let fes = asFiles state
  lift . lift $ put state {asFiles = fe : fes}

-- | Set last cmd entered by user
setLastCmd :: Monad m => Maybe UCmd -> App m ()
setLastCmd (Just UCmdEmpty) = return ()
setLastCmd cmd = lift . lift $ do
  state <- get
  put $ state {asLastCmd = cmd}

