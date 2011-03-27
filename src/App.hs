
-- | This module defines App monad

module App
(
App,
runApp,
AppState(..),
FileEntry(..),
addFileEntry,
setLastCmd
)
where

import Data.ByteString (ByteString)
import Data.Iteratee (Iteratee, run)
import Data.Iteratee.IO (enumHandle)
import System.IO (Handle)
import System.Environment (getEnv)
import System.Console.Haskeline (InputT, runInputT, defaultSettings, Settings(..))
import Control.Monad.Trans.State (StateT, evalStateT, get, put)
import Control.Monad.Trans.Class (lift)

import UCmd (UCmd(..))
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
  print home
  let history = home ++ "/.hfd_history"
  flip evalStateT (defaultState h) $
    runInputT defaultSettings {historyFile = Just history} (enumHandle 1 h app >>= run)

-- | Application state
data AppState = AppState {
  asFiles :: [(Int, FileEntry)],  -- ^ map of file entries
  asLastCmd :: Maybe UCmd,        -- ^ last command entered by user
  asHandle :: Handle              -- ^ handle to player
} deriving Show

-- | Default application state contains nothing
defaultState :: Handle -> AppState
defaultState = AppState [] Nothing

-- | File entry represents one source file
data FileEntry = FileEntry {
  fePath :: String,      -- ^ Path to file as recieved from player,
                         -- e.g. @\/home\/user\/proj;com\/example;Main.as@
  feContent :: [String]  -- ^ File content
} deriving Show

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

