
-- | This module defines App monad

module App
(
App,
runApp
)
where

import Data.ByteString (ByteString)
import Data.Iteratee (Iteratee, run)
import Data.Iteratee.IO (enumHandle)
import System.IO (Handle)
import System.Console.Haskeline (InputT, runInputT, defaultSettings)
import Control.Monad.Trans.State (StateT, evalStateT)

import Inst ()  -- 'MonadCatchIO' instance for 'InputT'

-- | App monad
type App m = Iteratee ByteString (InputT (StateT Int m))

-- | Run App monad
runApp :: Handle                -- ^ Input/Output stream to be used to communicate with player
       -> (Handle -> App IO a)  -- ^ Application; handle -- output stream to player
       -> IO a
runApp h app = flip evalStateT 0 $ runInputT defaultSettings (enumHandle 1 h (app h) >>= run)

