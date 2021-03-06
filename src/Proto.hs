
module Proto
(
setBreakpoint,
deleteBreakpoint,
-- deleteBreakpoints,
setDebuggerOption,
execFinish,
execContinue,
execStep,
execNext,
getFrame,
getField,
getProp,
nextMsg
)
where

import Data.Maybe (isJust)
import Data.List (find)
import Data.Word (Word32)
import Data.ByteString (hPut)
import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (get)
import Control.Monad.IO.Class(liftIO, MonadIO)
import System.IO (hFlush)

import App (App, AppState(..), Breakpoint(..), setBreakpoints, newBreakId)
import OMsg (OMsg(..), binOMsg)
import IMsg (IMsg(..), AMF(..), nextIMessage)

-- | Set breakpoint
setBreakpoint :: MonadIO m
  => Int  -- ^ File id
  -> Int  -- ^ Line number
  -> App m ()
setBreakpoint fl ln = do
  bs <- lift . lift $ liftM asBreaks get
  let exists = isJust $ find (\(_, b) -> (bpFileId b == fl) && (bpLine b == ln)) bs
  if exists
    then liftIO $ putStrLn "Breakpoint exists. Multiple breakpoints at the same line are not supported."
    else do
      sendMsg (OMsgSetBreakpoint (fromIntegral fl) (fromIntegral ln))
      msg <- nextMsg
      case msg of
        IMsgBreakpoints bs' -> do
          let exists' = isJust $ find (== (fromIntegral fl, fromIntegral ln)) bs'
          if exists'
            then do
              newId <- newBreakId
              setBreakpoints $ (newId, Breakpoint fl ln) : bs
            else
              liftIO $ putStrLn "Can't resolve breakpoint (the line is not executable?)"
        _ -> liftIO $ putStrLn "doSetBreakpoint: Unexpected message from player"


-- | Delete breakpoint
deleteBreakpoint :: MonadIO m => Int -> Int -> App m ()
deleteBreakpoint fl ln =
  sendMsg (OMsgClearBreakpoint (fromIntegral fl) (fromIntegral ln))

-- | Delete all breakpoints
-- Hmm... Doesn't work... Use `deleteBreakpoint` for now
{-deleteBreakpoints :: MonadIO m => App m ()
deleteBreakpoints =
  sendMsg OMsgClearBreakpoints-}

-- | Set debuger option
setDebuggerOption :: MonadIO m
  => String  -- ^ Option name
  -> String  -- ^ Option value
  -> App m ()
setDebuggerOption op val = do
  sendMsg (OMsgSetDebuggerOptions op val)
  msg <- nextMsg
  case msg of
    IMsgDebuggerOption _ _ -> return ()
    _ -> liftIO $ putStrLn "doSetDebuggerOption: Unexpected message from player"


-- | Send @finish@ command to player
execFinish :: MonadIO m => App m ()
execFinish =  sendMsg OMsgFinish

-- | Send @continue@ command to player
execContinue :: MonadIO m => App m ()
execContinue =  sendMsg OMsgContinue

-- | Send @step@ command to player
execStep :: MonadIO m => App m ()
execStep =  sendMsg OMsgStep

-- | Send @next@ command to player
execNext :: MonadIO m => App m ()
execNext =  sendMsg OMsgNext

-- | Get function frame
getFrame :: MonadIO m => App m [AMF]
getFrame = do
  sendMsg (OMsgGetFunctionFrame 0)
  msg <- nextMsg
  case msg of
    IMsgFunctionFrame _ _ _ vs -> return vs
    _ -> liftIO (putStrLn "doPrint: Unexpected message from player") >>
         return []

-- | Get field value
getField :: MonadIO m
  => Word32  -- ^ Object address
  -> String  -- ^ Field name
  -> App m [AMF]
getField ptr name = do
  sendMsg (OMsgGetField ptr name)
  msg <- nextMsg
  case msg of
    IMsgGetField _ vs -> return vs
    _ -> liftIO (putStrLn "Unexpected message from player") >>
         return []

-- | Get property by calling getter
getProp :: MonadIO m => Word32 -> String -> App m (Maybe AMF)
getProp ptr name = do
  sendMsg (OMsgGetField ptr name)
  msg <- nextMsg
  case msg of
    IMsgGetField v _ -> return $ Just v
    _ -> liftIO (putStrLn "Unexpected message from player") >>
          return Nothing

-- | Take next message
-- This function is just a wrapper around nextIMessage,
-- the only difference is that it responses to `IMsgProcessTag` message
-- and prints traces
nextMsg :: MonadIO m => App m IMsg
nextMsg = do
  msg <- nextIMessage
  -- liftIO $ print msg
  case msg of
    IMsgCreateAnonymObject _ -> nextMsg
    IMsgSetLocalVars _ -> nextMsg
    IMsgDeleteField _ _ -> nextMsg
    IMsgSetField _ _ _ -> nextMsg
    IMsgSetField2 _ _ _ -> nextMsg
    IMsgProcessTag -> sendMsg OMsgProcessTag >> nextMsg
    IMsgTrace str  -> liftIO (putStrLn $ " [trace] " ++ str) >> nextMsg
    _              -> return msg

-- | Send message to player
sendMsg :: MonadIO m => OMsg -> App m ()
sendMsg msg = do
  -- liftIO $ print msg
  h <- lift . lift $ liftM asHandle get
  liftIO $ hPut h (binOMsg msg) >> hFlush h
  return ()

