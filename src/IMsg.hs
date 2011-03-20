
-- | This module defines messages from player to debugger

module IMsg
(
IMsg(..),
nextIMessage
)
where

import Data.Word (Word8, Word16, Word32)
import Data.ByteString (ByteString, pack)
import qualified Data.Iteratee as I
import Data.Iteratee (Iteratee, Endian(LSB), endianRead4, endianRead2)
import Control.Monad (replicateM, when)

-- | Messages send by player
data IMsg
  = IMsgMenuState Word32 Word32                                          -- ^ 00 or 00
  | IMsgCreateAnonymObject Word32                                        -- ^ 03 or 03
  | IMsgTrace ByteString                                                 -- ^ 05 or 05
  | IMsgSetField Word32 ByteString [Word8]                               -- ^ 0A or 10
  | IMsgDeleteField Word32 ByteString                                    -- ^ 0B or 11
  | IMsgMovieAttr ByteString ByteString                                  -- ^ 0C or 12
  | IMsgSwdFileEntry Word32 Word32 ByteString ByteString Word32          -- ^ 0E or 14
  | IMsgAskBreakpoints                                                   -- ^ 0F or 15
  | IMsgBreakHit Word16 Word16 Word32 ByteString                         -- ^ 10 or 16
  | IMsgBreak                                                            -- ^ 11 or 17
  | IMsgSetLocalVars Word32                                              -- ^ 12 or 18
  | IMsgBreakpoints [(Word16, Word16)]                                   -- ^ 13 or 19
  | IMsgNumSwdFileEntry Word32 Word32                                    -- ^ 14 or 20
  | IMsgProcessTag                                                       -- ^ 19 or 25
  | IMsgVersion Word32 Word8                                             -- ^ 1A or 26
  | IMsgBreakHitEx Word16 Word16 [(Word16, Word16, Word32, ByteString)]  -- ^ 1B or 27
  | IMsgSetField2 Word32 ByteString [Word8]                              -- ^ 1C or 28
  | IMsgException Word32 ByteString [Word8]                              -- ^ 24 or 36
  | IMsgUnknown Word32 [Word8]

instance Show IMsg where
  show (IMsgVersion major minor) = "IMsgVersion(" ++ show major ++ ", " ++ show minor ++ ")"
  show (IMsgMovieAttr name value) = "IMsgMovieAttr(" ++ show name ++ ", " ++ show value ++ ")"
  show (IMsgNumSwdFileEntry num index) = "IMsgNumSwdFileEntry(" ++ show num ++ ", " ++ show index ++ ")"
  show (IMsgSwdFileEntry fileId unIndex name source swfIndex) =
    "IMsgSwdFileEntry(" ++ show fileId ++ ", " ++ show unIndex ++ ", " ++ show name ++ ", " ++ show source ++ ", " ++ show swfIndex ++ ")"
  show (IMsgBreakpoints l) = "IMsgBreakpoints(" ++ show l ++ ")"
  show IMsgAskBreakpoints = "IMsgAskBreakpoints"
  show (IMsgBreakHit fileId line addr function) = "IMsgBreakHit(" ++ show fileId ++ ", " ++ show line ++ ", " ++ show addr ++ ", " ++ show function ++ ")"
  show (IMsgBreakHitEx fileId line stack) = "IMsgBreakHitEx(" ++ show fileId ++ ", " ++ show line ++ ", " ++ show stack ++ ")"
  show IMsgBreak = "IMsgBreak"
  show (IMsgSetLocalVars addr) = "IMsgSetLocalVars(" ++ show addr ++ ")"
  show (IMsgCreateAnonymObject addr) = "IMsgCreateAnonymObject(" ++ show addr ++ ")"
  show (IMsgMenuState arg1 arg2) = "IMsgMenuState(" ++ show arg1 ++ ", " ++ show arg2 ++ ")"
  show (IMsgSetField2 addr name amf) = "IMsgSetField2(" ++ show addr ++ ", " ++ show name ++ ", " ++ show amf ++ ")"
  show (IMsgSetField addr name amf) = "IMsgSetField(" ++ show addr ++ ", " ++ show name ++ ", " ++ show amf ++ ")"
  show (IMsgDeleteField addr name) = "IMsgDeleteField(" ++ show addr ++ ", " ++ show name ++ ")"
  show IMsgProcessTag = "IMsgProcessTag"
  show (IMsgTrace msg) = "IMsgTrace(" ++ show msg ++ ")"
  show (IMsgException arg1 ex arg3) = "IMsgException(" ++ show arg1 ++ ", " ++ show ex ++ ", " ++ show arg3 ++ ")"
  show (IMsgUnknown idi dat) = "IMsgUnknown(" ++ show idi ++ ", " ++ show dat ++ ")"

-- | Read next message from player
nextIMessage :: Monad m => Iteratee ByteString m IMsg
nextIMessage = do
  len <- endianRead4 e_
  idi <- endianRead4 e_
  msg <- case idi of
           26 -> iterVersion len
           12 -> iterMovieAttr len
           20 -> iterNumSwdFileEntry len
           14 -> iterSwdFileEntry len
           19 -> iterBreakpoints len
           15 -> iterAskBreakpoints len
           16 -> iterBreakHit len
           27 -> iterBreakHitEx len
           17 -> iterBreak len
           18 -> iterSetLocalVars len
           03 -> iterCreateAnonymObject len
           00 -> iterMenuState len
           28 -> iterSetField2 len
           10 -> iterSetField len
           11 -> iterDeleteField len
           25 -> iterProcessTag len
           05 -> iterTrace len
           36 -> iterException len
           _  -> iterUnknown idi len
  return msg

e_ :: Endian
e_ = LSB

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
  when (len /= fromIntegral (ln1 + ln2) + 12) (fail "iterSwdFileEntry: wrong length")
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

-- | Read zero terminated string
-- returns string and number of bytes read
takeStr :: Monad m => Iteratee ByteString m (ByteString, Int)
takeStr = takeStr' [] 0
  where
  takeStr' :: Monad m => [Word8] -> Int -> Iteratee ByteString m (ByteString, Int)
  takeStr' cs len = do
    c <- I.head
    if c == 0
      then return . (flip (,) (len + 1)) . pack . reverse $ cs
      else takeStr' (c:cs) (len + 1)

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

