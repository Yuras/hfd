
-- | This module defines output messages, sent to player

module OMsg
(
OMsg(..),
binOMsg
)
where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString, empty, pack, append)
import qualified Data.ByteString.Char8 as BSChar
import Data.Word (Word32, Word16)
import Data.Bits (shiftR)

-- | Messages that debugger can send to player
data OMsg
  -- | 0F or 15
  = OMsgContinue
  -- | 11 or 17
  | OMsgSetBreakpoint Word16 Word16
  -- | 12 or 18
  | OMsgClearBreakpoint Word16 Word16
  -- | 13 or 19
  | OMsgClearBreakpoints
  -- | 14 or 20
  | OMsgNext
  -- | 15 or 21
  | OMsgStep
  -- | 16 or 22
  | OMsgFinish
  -- | 17 or 23
  | OMsgProcessTag
  -- | 19 or 25
  | OMsgGetField Word32 String
  -- | 1A or 26
  | OMsgGetFunctionFrame Word32
  -- | 1C or 28
  | OMsgSetDebuggerOptions String String
  deriving Show

-- | Convert `OMsg` to `ByteString`
binOMsg :: OMsg -> ByteString
binOMsg OMsgContinue                 = mkBin 15 empty
binOMsg (OMsgSetBreakpoint fl ln)    = mkBin 17 (mkBinWord32 0 `append` mkBinWord16 fl `append` mkBinWord16 ln)
binOMsg (OMsgClearBreakpoint fl ln)  = mkBin 18 (mkBinWord32 0 `append` mkBinWord16 fl `append` mkBinWord16 ln)
binOMsg OMsgClearBreakpoints         = mkBin 19 empty
binOMsg OMsgNext                     = mkBin 20 empty
binOMsg OMsgStep                     = mkBin 21 empty
binOMsg OMsgFinish                   = mkBin 22 empty
binOMsg OMsgProcessTag               = mkBin 23 empty
binOMsg (OMsgGetField addr name)     = mkBin 25 (mkBinWord32 addr `append` s2bs name `append` pack [0] `append` mkBinWord32 15)
binOMsg (OMsgGetFunctionFrame depth) = mkBin 26 (mkBinWord32 depth)
binOMsg (OMsgSetDebuggerOptions n v) = mkBin 28 (s2bs n `append` pack [0] `append` s2bs v `append` pack [0])

-- | Make binary message
--
-- Format: length (4b) + message id (4b) + message
--
-- All data is little endian
mkBin :: Word32 -> ByteString -> ByteString
mkBin idi msg = BS.concat [l, i, msg]
  where
  i = mkBinWord32 idi
  l = mkBinWord32 $ fromIntegral $ BS.length msg

-- | Convert `Word32` to `ByteString` using little endian encoding
mkBinWord32 :: Word32 -> ByteString
mkBinWord32 i = pack w
  where
  w =
    [ fromIntegral i
    , fromIntegral (i `shiftR` 8)
    , fromIntegral (i `shiftR` 16)
    , fromIntegral (i `shiftR` 24)
    ]

-- | Convert `Word16` to `ByteString` using little endian encoding
mkBinWord16 :: Word16 -> ByteString
mkBinWord16 i = pack w
  where
  w =
    [ fromIntegral i
    , fromIntegral (i `shiftR` 8)
    ]

-- | Convert `String` to `ByteString`
s2bs :: String -> ByteString
s2bs = BSChar.pack

