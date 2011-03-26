
-- | This module defines output messages, sent to player

module OMsg
(
OMsg(..),
binOMsg
)
where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString, empty, pack)
import Data.Word (Word32)
import Data.Bits (shiftR)

-- | Messages that debugger can send to player
data OMsg
  = OMsgContinue                 -- ^ 0F or 15
  | OMsgNext                     -- ^ 14 or 20
  | OMsgStep                     -- ^ 15 or 21
  | OMsgGetFunctionFrame Word32  -- ^ 1A or 26

-- | Convert `OMsg` to `ByteString`
binOMsg :: OMsg -> ByteString
binOMsg OMsgContinue                 = mkBin 15 empty
binOMsg OMsgNext                     = mkBin 20 empty
binOMsg OMsgStep                     = mkBin 21 empty
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

