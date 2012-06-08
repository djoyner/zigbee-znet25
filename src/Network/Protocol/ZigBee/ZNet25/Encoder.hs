{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module:      Network.Protocool.ZigBee.ZNet25.Encoder
-- Copyright:   (c) 2012 David Joyner
-- License:     BSD3
-- Maintainer:  David Joyner <david@joynerhome.net>
-- Stability:   experimental
-- Portability: portable
--
-- XBee ZNet 2.5 (ZigBee) frame encoder/decoder functions

module Network.Protocol.ZigBee.ZNet25.Encoder (
  -- * Frame encoder
    encode

  -- * Stateful frame decoder
  , DecoderState
  , initDecode
  , decode
) where

import Network.Protocol.ZigBee.ZNet25.Constants
import Network.Protocol.ZigBee.ZNet25.Frame
import Control.Monad
import qualified Control.Monad.State as S
import Data.Bits (xor)
import qualified Data.ByteString as B
import Data.Either.Utils (forceEither)
import qualified Data.Serialize as DS
import Data.Word

-- | Serialize a 'Frame', escape control characters, and wrap the result with
-- framing bytes.  Return an array of 'B.ByteString' suitable for transmission
-- to the XBee modem.
--
-- Note that this function returns an array of 'B.ByteString'.  Encoding
-- takes place in a piece-wise manner and for efficiency's sake the individual
-- bits are not concatenated to form a single 'B.ByteString'.  Typically this is
-- a non-issue however if you need a single 'B.ByteString' representation of the
-- 'Frame' you can always obtain it by calling 'B.concat'.
--
-- Here's an example that illustrates 'encode' usage as well as the
-- on-the-wire frame format:
--
-- > import qualified Data.ByteString as B
-- > import Network.Protocol.ZigBee.ZNet25
-- > import Text.Printf
-- >
-- > main = hexdump $ B.concat $ encode (ATCommand 0 (commandName "ND") B.empty)
-- >
-- > hexdump = mapM_ (putStr . printf "%02x ") . B.unpack
--
-- This prints:
--
-- > 7e 00 04 08 00 4e 44 65
--
-- The leading @7e@ byte is the frame delimiter.  This is followed by the 16-bit
-- frame length (4 bytes in this case), that many bytes of data (the
-- serialized 'ATCommand' frame), and the final checksum byte.
encode :: Frame -> [B.ByteString]
encode f = [ B.singleton ctrlFrameDelim, len, f_esc, cksum ]
  where
    f_enc = DS.encode f
    f_esc = escapeBuffer f_enc
    len   = (escapeBuffer . DS.runPut . DS.putWord16be .
             fromIntegral . B.length) f_enc
    cksum = escapeBuffer $ B.singleton $ 0xff - (B.foldl (+) 0 f_enc)

data FrameState = Hunting
                | GetLength
                | GetData deriving Show

-- | 'decode' runs in the 'S.State' monad.  @DecoderState@ tracks the
-- decoder's in/out-of frame state, current frame length, and other state
-- variables.
data DecoderState = DS
  { dsFrameState :: FrameState
  , dsFrameLength :: Int
  , dsEscapedChar :: Bool
  , dsBuffer :: B.ByteString } deriving Show

-- | Initial state needed to run 'decode' in the 'S.State' monad.
initDecode :: DecoderState
initDecode = DS Hunting 0 False B.empty

-- | Decode a 'B.ByteString' in the 'S.State' monad, reversing the 'encode'
-- process.  Once a frame delimiter byte is found, the inner frame payload is
-- unescaped, the checksum is verified, and finally a 'Frame' is deserialized.
--
-- Note that this function may produce zero or more errors or 'Frame's depending
-- on the 'DecoderState' and input byte string.  Errors will be reported for
-- checksum errors and 'Frame' deserialization failures.
--
-- Here's a slightly more complex example that 'encode's two separate frames,
-- runs each array of 'B.ByteString's through @decode@ and prints the result
-- after running the 'S.State' monad:
--
-- > import Control.Monad.State
-- > import qualified Data.ByteString as B
-- > import Network.Protocol.ZigBee.ZNet25
-- >
-- > main = putStrLn $ show $ evalState (mapM decode bs) initDecode
-- >   where
-- >     bs = concat $ map encode [atndCommand, txRequest]
-- >     atndCommand = ATCommand 1 (commandName "ND") B.empty
-- >     txRequest = ZigBeeTransmitRequest 2 addr nwaddr 0 0 $ B.singleton 0x55
-- >     addr = address $ B.pack [ 0xde, 0xad, 0xbe, 0xef, 0xba, 0xda, 0xba, 0xda ]
-- >     nwaddr = networkAddress $ B.pack [ 0x55, 0xaa ]
--
-- This prints:
--
-- > [[],[],[],[Right (ATCommand 1 "ND" "")],[],[],[],[Right (ZigBeeTransmitRequest 2 de:ad:be:ef:ba:da:ba:da 55:aa 0 0 "U")]]
--
-- Note a few things:
--
--  * Each call to 'encode' apparently produced four separate 'B.ByteString's.
--    This is a by-product of the 'encode' implementation as described above.
--
--  * @decode@ was only able to produce a result once the final 'B.ByteString'
--    of each 'Frame' was processed.  In this case the result was
--    'Right' 'Frame'.  If an error had occurred, we'd see 'Left' 'String'
--    instead.
decode :: S.MonadState DecoderState m => B.ByteString -> m [Either String Frame]
decode bs0 = do
    ds <- S.get
    let t = S.runState (unescapeBuffer bs0) $ dsEscapedChar ds
    go ds { dsEscapedChar = snd t } $ fst t

  where
    -- Drop bytes until a frame delimiter is found
    go ds@(DS Hunting _ _ _) bs
      | B.null bs                   = S.put ds >> return []
      | B.head bs == ctrlFrameDelim = go ds_gl $ B.tail bs
      | otherwise                   = go ds $ B.tail bs
      where
        ds_gl = ds { dsFrameState = GetLength, dsBuffer = B.empty }

    -- Once we have at least two bytes of unescaped data,
    -- deserialize the length (add one byte for trailing checksum)
    go ds@(DS GetLength _ _ buf) bs
      | B.length buf' >= 2 = go ds_gd $ B.drop 2 buf'
      | otherwise          = S.put ds_gl >> return []
      where
        buf'  = B.append buf bs
        len'  = (fromIntegral . forceEither . DS.runGet DS.getWord16be) buf' + 1
        ds_gd = ds { dsFrameState = GetData
                   , dsFrameLength = len'
                   , dsBuffer = B.empty }
        ds_gl = ds { dsBuffer = buf' }

    -- Once we've accumulated the whole frame (including the checksum byte)
    -- then we can decode and output the result
    go ds@(DS GetData len _ buf) bs
      | B.length buf' >= len = liftM (result:) $ go ds_h $ B.drop len buf'
      | otherwise            = S.put ds_gd >> return []
      where
        result
          | cksum_ok  = case DS.decode $ B.take (len - 1) buf' of
                          Left err -> Left $ "Decode error: " ++ err
                          Right f  -> Right (f :: Frame)
          | otherwise = Left "Checksum error"
        buf'          = B.append buf bs
        cksum_ok      = B.foldl (+) 0 (B.take len buf') == 0xff
        ds_h          = initDecode
        ds_gd         = ds { dsBuffer = buf' }

escapeBuffer :: B.ByteString -> B.ByteString
escapeBuffer = B.concat . fmap B.pack . map escapeChar . B.unpack

unescapeBuffer :: S.MonadState Bool m => B.ByteString -> m B.ByteString
unescapeBuffer = liftM (B.pack . concat) . mapM unescapeChar . B.unpack

escapeChar :: Word8 -> [Word8]
escapeChar c
  | isControlChar c = [ctrlEscape, c `xor` 0x20]
  | otherwise       = [c]

unescapeChar :: S.MonadState Bool m => Word8 -> m [Word8]
unescapeChar c = S.get >>= unescape
  where
    unescape True       = S.put False >> return [c `xor` 0x20]
    unescape False
      | c == ctrlEscape = S.put True >> return []
      | otherwise       = return [c]

isControlChar :: Word8 -> Bool
isControlChar c
  | c == ctrlFrameDelim = True
  | c == ctrlEscape     = True
  | c == ctrlXon        = True
  | c == ctrlXoff       = True
  | otherwise           = False

