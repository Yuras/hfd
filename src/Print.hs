
module Print
(
doPrint
)
where

import Data.Bits (testBit)
import Data.Maybe (isJust, fromJust)
import Control.Monad (unless, liftM)
import Control.Monad.IO.Class(liftIO, MonadIO)

import App (App)
import Proto (getFrame, getField, getProp)
import IMsg (AMF(..), AMFValue(..), amfUndecoratedName)

-- | Print variable
doPrint :: MonadIO m => [String] -> App m ()
doPrint v = do
  vs <- getFrame
  let vs' = filter (\a -> amfUndecoratedName a == head v) vs
  unless (null vs') (doPrintProps (tail v) (head vs'))

-- | Print object properties as requested
doPrintProps :: MonadIO m => [String] -> AMF -> App m ()
doPrintProps [] v = callGetter v >>= liftIO . putStrLn . prettyAMF
doPrintProps (name:ns) (AMF _ _ _ (AMFObject ptr _ _ _ _)) = do
  vs <- liftM (filter notTrails) $ getField ptr ""
  if name == ""
    then printAll vs
    else find' vs
  where
  notTrails (AMF _ _ _ AMFTrails) = False
  notTrails _ = True
  printAll vs = mapM callGetter vs >>= liftIO . mapM_ (putStrLn . prettyAMF)
  find' vs =
    let vs' = filter (\a -> amfUndecoratedName a == name) vs in
    case vs' of
      [] -> liftIO $ putStrLn "Not found"
      [v] -> if null ns
               then callGetter v >>= (liftIO . putStrLn . prettyAMF)
               else doPrintProps ns v
      _ -> liftIO $ putStrLn "Multiple"
doPrintProps _ _ = liftIO $ putStrLn "Not found"

hasGetter :: AMF -> Bool
hasGetter amf = amfFlags amf `testBit` 19

callGetter :: MonadIO m => AMF -> App m AMF
callGetter amf = if hasGetter amf
                   then call amf
                   else return amf
  where
  call a@(AMF ptr _ _ _) = do
    res <- getProp ptr (amfUndecoratedName a)
    if isJust res
      then return $ fromJust res
      else return a

-- | Show AMF for user
prettyAMF :: AMF -> String
prettyAMF amf@(AMF _ _ _ v) = amfUndecoratedName amf ++ " = " ++ prettyAMVValue v

-- | Show AMF value for user
prettyAMVValue :: AMFValue -> String
prettyAMVValue (AMFDouble v) = show v
prettyAMVValue (AMFBool v) = show v
prettyAMVValue (AMFString v) = show v
prettyAMVValue (AMFObject _ _ _ _ n) = n
prettyAMVValue AMFNull = "null"
prettyAMVValue AMFUndefined = "undefined"
prettyAMVValue v = show v

