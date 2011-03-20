
-- | This module defines 'MonadCatchIO' instance for 'InputT'

module Inst
(
)
where

import qualified System.Console.Haskeline as HL
import System.Console.Haskeline (InputT, MonadException)
import Control.Monad.CatchIO (MonadCatchIO(..))

instance (MonadCatchIO m, MonadException m) => MonadCatchIO (InputT m) where
  catch = HL.catch
  block = HL.block
  unblock = HL.unblock

