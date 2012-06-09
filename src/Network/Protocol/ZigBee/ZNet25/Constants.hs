{-# OPTIONS -fno-warn-missing-signatures #-}

module Network.Protocol.ZigBee.ZNet25.Constants where

import Data.Word (Word8)

-- | HDLC-like control characters
ctrlFrameDelim = 0x7e :: Word8
ctrlEscape = 0x7d :: Word8
ctrlXon = 0x11 :: Word8
ctrlXoff = 0x13 :: Word8

-- | ZNet 2.5 API id constants
apiIdModemStatus = 0x8a :: Word8
apiIdATCommand = 0x08 :: Word8
apiIdATCommandQueueParameterValue = 0x09 :: Word8
apiIdATCommandResponse = 0x88 :: Word8
apiIdRemoteCommandRequest = 0x17 :: Word8
apiIdRemoteCommandResponse = 0x97 :: Word8
apiIdZigBeeTransmitRequest = 0x10 :: Word8
apiIdExplicitAddressingZigBeeCommandFrame = 0x11 :: Word8
apiIdZigBeeTransmitStatus = 0x8b :: Word8
apiIdZigBeeReceivePacket = 0x90 :: Word8
apiIdZigBeeExplicitRxIndicator = 0x91 :: Word8
apiIdZigBeeIODataSampleIndicator = 0x92 :: Word8
apiIdXBeeSensorReadIndicator = 0x94 :: Word8
apiIdNodeIdentificationIndicator = 0x95 :: Word8

