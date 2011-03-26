
-- | This module defines messages from player to debugger

module IMsg
(
IMsg(..),
AMF(..),
nextIMessage
)
where

import Data.Word (Word8, Word16, Word32)
import Data.ByteString (ByteString, pack)
import qualified Data.ByteString.Char8 as BSChar
import qualified Data.Iteratee as I
import Data.Iteratee (Iteratee, Endian(LSB), endianRead4, endianRead2)
import Control.Monad (replicateM, when)


-- * Interface

-- | Messages sent by player
data IMsg
  -- | 00 or 00
  = IMsgMenuState Word32 Word32
  -- | 03 or 03
  | IMsgCreateAnonymObject Word32
  -- | 05 or 05
  | IMsgTrace ByteString
  -- | 0A or 10
  | IMsgSetField Word32 ByteString [Word8]
  -- | 0B or 11
  | IMsgDeleteField Word32 ByteString
  -- | 0C or 12
  | IMsgMovieAttr ByteString ByteString
  -- | 0E or 14
  | IMsgSwdFileEntry Word32 Word32 ByteString ByteString Word32
  -- | 0F or 15
  | IMsgAskBreakpoints
  -- | 10 or 16
  | IMsgBreakHit Word16 Word16 Word32 ByteString
  -- | 11 or 17
  | IMsgBreak
  -- | 12 or 18
  | IMsgSetLocalVars Word32
  -- | 13 or 19
  | IMsgBreakpoints [(Word16, Word16)]
  -- | 14 or 20
  | IMsgNumSwdFileEntry Word32 Word32
  -- | 19 or 25
  | IMsgProcessTag
  -- | 1A or 26
  | IMsgVersion Word32 Word8
  -- | 1B or 27
  | IMsgBreakHitEx Word16 Word16 [(Word16, Word16, Word32, ByteString)]
  -- | 1C or 28
  | IMsgSetField2 Word32 ByteString [Word8]
  -- | 1F or 31
  | IMsgFunctionFrame Word32 Word32 AMF [AMF]
  -- | 24 or 36
  | IMsgException Word32 ByteString [Word8]
  -- | All other
  | IMsgUnknown Word32 [Word8]
  deriving Show

-- | Represents Action Message Format entry
data AMF = AMF {
  amfParent :: Word32,
  amfName :: String,
  amfFlags :: Word32,
  amfValue :: AMFValue
} deriving Show

-- | Represents AMF value
data AMFValue = AMFDouble Double
              | AMFBool Bool
              | AMFString ByteString
              | AMFObject Word32 Word32 Word16 Word16 ByteString
              | AMFNull
              deriving Show

-- | Read next message from player
nextIMessage :: Monad m => Iteratee ByteString m IMsg
nextIMessage = do
  len <- endianRead4 e_
  idi <- endianRead4 e_
  msg <- case idi of
           00 -> iterMenuState len
           03 -> iterCreateAnonymObject len
           05 -> iterTrace len
           10 -> iterSetField len
           11 -> iterDeleteField len
           12 -> iterMovieAttr len
           14 -> iterSwdFileEntry len
           15 -> iterAskBreakpoints len
           16 -> iterBreakHit len
           17 -> iterBreak len
           18 -> iterSetLocalVars len
           19 -> iterBreakpoints len
           20 -> iterNumSwdFileEntry len
           25 -> iterProcessTag len
           26 -> iterVersion len
           27 -> iterBreakHitEx len
           28 -> iterSetField2 len
           31 -> iterFunctionFrame len
           36 -> iterException len
           _  -> iterUnknown idi len
  return msg


-- * Internals
-- ** Iteratees to parse messages

iterFunctionFrame :: Monad m => Word32 -> Iteratee ByteString m IMsg
iterFunctionFrame len = do
  depth <- endianRead4 e_
  when (depth /= 0) (fail "iterFunctionFrame: depth != 0, not implemented")
  addr <- endianRead4 e_
  (amf, ln) <- takeAMF
  children <- takeChildren (fromIntegral len - 4 - 4 - ln) []
  return $ IMsgFunctionFrame depth addr amf children
  where
  takeChildren 0 res = return $ reverse res
  takeChildren l res = do
    when (l < 0) (fail "iterFunctionFrame: wrong size")
    (amf, vl) <- takeAMF
    takeChildren (fromIntegral l - vl) (amf : res)

iterException :: Monad m => Word32 -> Iteratee ByteString m IMsg
iterException len = do
  arg1 <- endianRead4 e_
  (ex, ln) <- takeStr
  arg3 <- replicateM (fromIntegral len - 4 - ln) I.head
  return $ IMsgException arg1 ex arg3

iterTrace :: Monad m => Word32 -> Iteratee ByteString m IMsg
iterTrace len = do
  (msg, ln) <- takeStr
  when (len /= fromIntegral ln) (fail "iterTrace: wrong length")
  return $ IMsgTrace msg

iterProcessTag :: Monad m => Word32 -> Iteratee ByteString m IMsg
iterProcessTag len = do
  when (len /= 0) (fail "iterProcessTag: wrong length")
  return IMsgProcessTag

iterDeleteField :: Monad m => Word32 -> Iteratee ByteString m IMsg
iterDeleteField len = do
  addr <- endianRead4 e_
  (name, ln) <- takeStr
  when (len /= fromIntegral ln + 4) (fail "iterDeleteField: wrong length")
  return $ IMsgDeleteField addr name

iterSetField :: Monad m => Word32 -> Iteratee ByteString m IMsg
iterSetField len = do
  addr <- endianRead4 e_
  (name, ln) <- takeStr
  amf <- replicateM (fromIntegral len - 4 - ln) I.head
  return $ IMsgSetField addr name amf

iterSetField2 :: Monad m => Word32 -> Iteratee ByteString m IMsg
iterSetField2 len = do
  addr <- endianRead4 e_
  (name, ln) <- takeStr
  amf <- replicateM (fromIntegral len - 4 - ln) I.head
  return $ IMsgSetField2 addr name amf

iterMenuState :: Monad m => Word32 -> Iteratee ByteString m IMsg
iterMenuState len = do
  when (len /= 8) (fail "iterMenuState: wrong length")
  arg1 <- endianRead4 e_
  arg2 <- endianRead4 e_
  return $ IMsgMenuState arg1 arg2

iterCreateAnonymObject :: Monad m => Word32 -> Iteratee ByteString m IMsg
iterCreateAnonymObject len = do
  when (len /= 4) (fail "iterCreateAnonymObject: wrong length")
  addr <- endianRead4 e_
  return $ IMsgCreateAnonymObject addr

iterSetLocalVars :: Monad m => Word32 -> Iteratee ByteString m IMsg
iterSetLocalVars len = do
  when (len /= 4) (fail "iterSetLocalVars: wrong length")
  addr <- endianRead4 e_
  return $ IMsgSetLocalVars addr

iterBreak :: Monad m => Word32 -> Iteratee ByteString m IMsg
iterBreak len = do
  when (len /= 0) (fail "iterBreak: wrong length")
  return IMsgBreak

-- XXX: Check length
iterBreakHitEx :: Monad m => Word32 -> Iteratee ByteString m IMsg
iterBreakHitEx _ = do
  fileId <- endianRead2 e_
  line <- endianRead2 e_
  depth <- endianRead4 e_
  stack <- replicateM (fromIntegral depth) iterFrame
  return $ IMsgBreakHitEx fileId line stack
  where
  iterFrame = do
    fileId <- endianRead2 e_
    line <- endianRead2 e_
    addr <- endianRead4 e_
    (entry, _) <- takeStr
    return (fileId, line, addr, entry)

iterBreakHit :: Monad m => Word32 -> Iteratee ByteString m IMsg
iterBreakHit len = do
  fileId <- endianRead2 e_
  line <- endianRead2 e_
  addr <- endianRead4 e_
  (function, ln) <- takeStr
  when (len /= fromIntegral ln + 8) (fail "iterBreakHit: wrong length")
  return $ IMsgBreakHit fileId line addr function

iterAskBreakpoints :: Monad m => Word32 -> Iteratee ByteString m IMsg
iterAskBreakpoints len = do
  when (len /= 0) (fail "iterAskBreakpoints: wrong length")
  return IMsgAskBreakpoints

-- XXX: Check length
iterBreakpoints :: Monad m => Word32 -> Iteratee ByteString m IMsg
iterBreakpoints _ = do
  count <- endianRead4 e_
  l <- replicateM (fromIntegral count) iter'
  return $ IMsgBreakpoints l
  where
  iter' = do
    fileId <- endianRead2 e_
    line <- endianRead2 e_
    return (fileId, line)

iterSwdFileEntry :: Monad m => Word32 -> Iteratee ByteString m IMsg
iterSwdFileEntry len = do
  fileId <- endianRead4 e_
  unIndex <- endianRead4 e_
  (name, ln1) <- takeStr
  (source, ln2) <- takeStr
  swfIndex <- endianRead4 e_
  when (len /= fromIntegral (ln1 + ln2) + 12)
       (fail "iterSwdFileEntry: wrong length")
  return $ IMsgSwdFileEntry fileId unIndex name source swfIndex

iterUnknown :: Monad m => Word32 -> Word32 -> Iteratee ByteString m IMsg
iterUnknown idi len = do
  dat <- replicateM (fromIntegral len) I.head
  return $ IMsgUnknown idi dat

iterVersion :: Monad m => Word32 -> Iteratee ByteString m IMsg
iterVersion len = do
  when (len /= 5) (fail "iterVersion: wrong length")
  major <- endianRead4 e_
  minor <- I.head
  return $ IMsgVersion major minor

iterMovieAttr :: Monad m => Word32 -> Iteratee ByteString m IMsg
iterMovieAttr len = do
  (name, ln1) <- takeStr
  (value, ln2) <- takeStr
  when (len /= fromIntegral (ln1 + ln2)) (fail "iterMovieAttr: wrong length")
  return $ IMsgMovieAttr name value

iterNumSwdFileEntry :: Monad m => Word32 -> Iteratee ByteString m IMsg
iterNumSwdFileEntry len = do
  when (len /= 8) (fail "iterNumSwdFileEntry: wrong length")
  num <- endianRead4 e_
  index <- endianRead4 e_
  return $ IMsgNumSwdFileEntry num index


-- ** Utilities

e_ :: Endian
e_ = LSB

-- | Read zero terminated string
-- returns string and number of bytes read
takeStr :: Monad m => Iteratee ByteString m (ByteString, Int)
takeStr = takeStr' [] 0
  where
  takeStr' :: Monad m =>
    [Word8] -> Int -> Iteratee ByteString m (ByteString, Int)
  takeStr' cs len = do
    c <- I.head
    if c == 0
      then return . (flip (,) (len + 1)) . pack . reverse $ cs
      else takeStr' (c:cs) (len + 1)

-- | Read AMF
takeAMF :: Monad m => Iteratee ByteString m (AMF, Int)
takeAMF = do
  parent <- endianRead4 e_
  (name, nl) <- takeStr
  vtype <- endianRead2 e_
  flags <- endianRead4 e_
  (value, vl) <- takeAMFValue vtype
  return $ (AMF parent (bs2s name) flags value, 4 + nl + 2 + 4 + vl)

-- | Read AMF value
takeAMFValue :: Monad m => Word16 -> Iteratee ByteString m (AMFValue, Int)
takeAMFValue 0 = do
  (str, ln) <- takeStr
  return (AMFDouble . read . bs2s $ str, ln)
takeAMFValue 2 = do
  (str, ln) <- takeStr
  return (AMFString str, ln)
takeAMFValue 3 = do
  oid <- endianRead4 e_
  tp <- endianRead4 e_
  isF <- endianRead2 e_
  r <- endianRead2 e_
  (typeName, tl) <- takeStr
  return (AMFObject oid tp isF r typeName, 4 + 4 + 2 + 2 + tl)
takeAMFValue 5 = return (AMFNull, 0)
takeAMFValue tp = fail $ "takeAMFValue: not implemented: " ++ show tp

-- | ByteString to String
bs2s :: ByteString -> String
bs2s = BSChar.unpack

