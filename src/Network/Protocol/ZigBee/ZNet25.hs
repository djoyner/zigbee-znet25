-- |
-- Module:      Network.Protocool.ZigBee.ZNet25
-- Copyright:   (c) 2012 David Joyner
-- License:     BSD3
-- Maintainer:  David Joyner <david@joynerhome.net>
-- Stability:   experimental
-- Portability: portable
--
-- A protocol library enabling wireless communications via
-- XBee/XBee-PRO (ZibBee) ZNet 2.5 RF modules.
--
-- The library performs binary encoding/decoding, on-the-wire framing,
-- and error checking of control and data "API" frames.  In other words
-- the library expects to communicate with an XBee modem that has AP
-- parameter = 2, indicating API frames with HDLC-like escape characters.
-- This mode provides for 8-bit transparent operation and is described in
-- Section 6 of <ftp://ftp1.digi.com/support/documentation/90000866_C.pdf>.
--
-- The library code is 100% pure; nothing here depends on the
-- IO monad and no particular modem device is assumed.  The code
-- depends on the @cereal@ package for serialization but is otherwise
-- pretty standard.  The test suite is based on @QuickCheck@ properties.

module Network.Protocol.ZigBee.ZNet25 (
    module Network.Protocol.ZigBee.ZNet25.Encoder
  , module Network.Protocol.ZigBee.ZNet25.Frame
) where

import Network.Protocol.ZigBee.ZNet25.Encoder
import Network.Protocol.ZigBee.ZNet25.Frame

