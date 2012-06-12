# Overview

This is a Haskell protocol library enabling wireless communications via
XBee/XBee-PRO (ZigBee) ZNet 2.5 RF modules.

The library performs binary encoding/decoding, on-the-wire framing,
and error checking of control and data "API" frames.  In other words 
the library expects to communicate with an XBee modem that has AP
parameter = 2, indicating API frames with HDLC-like escape characters.  This
mode provides for 8-bit transparent operation and is described in
Section 6 of the
[XBee ZNet 2.5 Product Manual] (ftp://ftp1.digi.com/support/documentation/90000866_C.pdf).

The library code is 100% pure; nothing here depends on the
IO monad and no particular modem device is assumed.  The code
depends on the [cereal](http://hackage.haskell.org/package/cereal)
package for serialization but is otherwise
pretty standard.  The test suite is based on
[QuickCheck](http://hackage.haskell.org/package/QuickCheck) properties.

# Getting Started

Install with cabal:

* `cabal install zigbee-znet25`

Or clone the master [git repository](https://github.com/djoyner/zigbee-znet25):

* `git clone git://github.com/djoyner/zigbee-znet25.git`

A [sample program](https://github.com/djoyner/zigbee-znet25/blob/master/sample.hs)
is included.  I've also used the library to communicate with an
[Arduino-based temperature and power-usage probe](https://github.com/djoyner/hap-probe-collector).

# Author and Maintainer

David Joyner, <david@joynerhome.net>

