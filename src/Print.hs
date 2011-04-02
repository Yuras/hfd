
module Print
(
doPrint
)
where

import Control.Monad (unless)
import Control.Monad.IO.Class(liftIO, MonadIO)

import App (App)
import Proto (getFrame, getField)
import IMsg (AMF(..), AMFValue(..), amfUndecoratedName)

-- | Print variable
doPrint :: MonadIO m => [String] -> App m ()
doPrint v = do
  vs <- getFrame
  let vs' = filter (\a -> amfUndecoratedName a == head v) vs
  unless (null vs') (doPrintProps (tail v) (amfValue $ head vs'))
  where

-- | Print object properties as requested
doPrintProps :: MonadIO m => [String] -> AMFValue -> App m ()
doPrintProps [] v = liftIO $ print v
doPrintProps (name:ns) (AMFObject ptr _ _ _ _) = do
  vs <- getField ptr ""
  if name == ""
    then printAll vs
    else find' vs
  where
  printAll vs = liftIO $ mapM_ print vs
  find' vs =
    let vs' = filter (\a -> amfUndecoratedName a == name) vs in
    case vs' of
      [] -> liftIO $ putStrLn "Not found"
      [v] -> if null ns
               then liftIO $ print v
               else doPrintProps ns (amfValue v)
      _ -> liftIO $ putStrLn "Multiple"
doPrintProps _ _ = liftIO $ putStrLn "Not found"

