
-- | This module defines output messages, sent to player

module OMsg
(
OMsg(..),
binOMsg
)
where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString, empty, pack, append)
import Data.Word (Word32, Word16)
import Data.Bits (shiftR)

-- | Messages that debugger can send to player
data OMsg
  = OMsgContinue                     -- ^ 0F or 15
  | OMsgSetBreakpoint Word16 Word16  -- ^ 11 or 17
  | OMsgNext                         -- ^ 14 or 20
  | OMsgStep                         -- ^ 15 or 21
  | OMsgProcessTag                   -- ^ 17 or 23
  | OMsgGetFunctionFrame Word32      -- ^ 1A or 26

-- | Convert `OMsg` to `ByteString`
binOMsg :: OMsg -> ByteString
binOMsg (OMsgSetBreakpoint fl ln)    = mkBin 17 (mkBinWord32 0 `append` mkBinWord16 fl `append` mkBinWord16 ln)
binOMsg OMsgContinue                 = mkBin 15 empty
binOMsg OMsgNext                     = mkBin 20 empty
binOMsg OMsgStep                     = mkBin 21 empty
binOMsg OMsgProcessTag               = mkBin 23 empty
binOMsg (OMsgGetFunctionFrame depth) = mkBin 26 (mkBinWord32 depth)

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

