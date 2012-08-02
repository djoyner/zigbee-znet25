-- |
-- Module:      Network.Protocool.ZigBee.ZNet25.Frame
-- Copyright:   (c) 2012 David Joyner
-- License:     BSD3
-- Maintainer:  David Joyner <david@joynerhome.net>
-- Stability:   experimental
-- Portability: portable
--
-- XBee ZNet 2.5 (ZigBee) frame types

module Network.Protocol.ZigBee.ZNet25.Frame (
  -- * The Frame type
    Frame(..)

  -- * AT-style command names
  , CommandName
  , commandName
  , unCommandName

  -- * Node addressing
  , Address
  , address
  , unAddress
  , NetworkAddress
  , networkAddress
  , unNetworkAddress

  -- * Type aliases
  , AnalogChannelMask
  , BroadcastRadius
  , ClusterId
  , CommandOptions
  , CommandStatus
  , DeviceType
  , DeliveryStatus
  , DestinationEndpoint
  , DigitalChannelMask
  , DiscoveryStatus
  , FrameId
  , ManufacturerId
  , ModemStatusByte
  , ParentNetworkAddress
  , ProfileId
  , ReceiveOptions
  , RemoteAddress
  , RemoteNetworkAddress
  , SampleCount
  , SourceAction
  , SourceEndpoint
  , TransmitOptions
  , TransmitRetryCount
  , XBeeSensorMask
) where

import Control.Applicative
import Control.Monad (liftM)
import qualified Data.ByteString as B
import Data.Char (chr, ord)
import Data.List (intercalate)
import Data.Serialize
import Data.Word
import Network.Protocol.ZigBee.ZNet25.Constants
import Text.Printf

-- | API frame types.  See Section 6 of the XBee ZNet 2.5 Product Manual
-- (<ftp://ftp1.digi.com/support/documentation/90000866_C.pdf>)
-- for frame type-specific documentation.
data Frame = ApiIdNotImplemented Word8
           | ModemStatus ModemStatusByte
           | ATCommand FrameId CommandName B.ByteString
           | ATCommandQueueParameterValue FrameId CommandName B.ByteString
           | ATCommandResponse FrameId CommandName CommandStatus B.ByteString
           | RemoteCommandRequest FrameId Address NetworkAddress CommandOptions
               CommandName B.ByteString
           | RemoteCommandResponse FrameId Address NetworkAddress CommandName
               CommandStatus B.ByteString
           | ZigBeeTransmitRequest FrameId Address NetworkAddress
               BroadcastRadius TransmitOptions B.ByteString
           | ExplicitAddressingZigBeeCommandFrame FrameId Address NetworkAddress
               SourceEndpoint DestinationEndpoint ClusterId ProfileId
               BroadcastRadius TransmitOptions B.ByteString
           | ZigBeeTransmitStatus FrameId NetworkAddress TransmitRetryCount
               DeliveryStatus DiscoveryStatus
           | ZigBeeReceivePacket Address NetworkAddress ReceiveOptions
               B.ByteString
           | ZigBeeExplicitRxIndicator Address NetworkAddress SourceEndpoint
               DestinationEndpoint ClusterId ProfileId ReceiveOptions
               B.ByteString
           | ZigBeeIODataSampleIndicator Address NetworkAddress ReceiveOptions
               SampleCount DigitalChannelMask AnalogChannelMask B.ByteString
           | XBeeSensorReadIndicator Address NetworkAddress ReceiveOptions
               XBeeSensorMask B.ByteString
           | NodeIdentificationIndicator Address NetworkAddress ReceiveOptions
               RemoteNetworkAddress RemoteAddress String ParentNetworkAddress
               DeviceType SourceAction ProfileId ManufacturerId
  deriving (Eq, Show)

instance Serialize Frame where
  put (ApiIdNotImplemented _) = error "Unsupported put (ApiIdNotImplemented)"

  put (ModemStatus modemStatus) =
    put apiIdModemStatus >>
    put modemStatus

  put (ATCommand frameId cmdName val) =
    put apiIdATCommand >>
    put frameId >>
    put cmdName >>
    putByteString val

  put (ATCommandQueueParameterValue frameId cmdName val) =
    put apiIdATCommandQueueParameterValue >>
    put frameId >>
    put cmdName >>
    putByteString val

  put (ATCommandResponse frameId cmdName cmdStatus val) =
    put apiIdATCommandResponse >>
    put frameId >>
    put cmdName >>
    put cmdStatus >>
    putByteString val

  put (RemoteCommandRequest frameId addr nwaddr cmdOptions cmdName val) =
    put apiIdRemoteCommandRequest >>
    put frameId >>
    put addr >>
    put nwaddr >>
    put cmdOptions >>
    put cmdName >>
    putByteString val

  put (RemoteCommandResponse frameId addr nwaddr cmdName cmdStatus val) =
    put apiIdRemoteCommandResponse >>
    put frameId >>
    put addr >>
    put nwaddr >>
    put cmdName >>
    put cmdStatus >>
    putByteString val

  put (ZigBeeTransmitRequest frameId addr nwaddr bcastRadius txOptions val) =
    put apiIdZigBeeTransmitRequest >>
    put frameId >>
    put addr >>
    put nwaddr >>
    put bcastRadius >>
    put txOptions >>
    putByteString val

  put (ExplicitAddressingZigBeeCommandFrame frameId addr nwaddr srcEP destEP
       clusterId profileId bcastRadius txOptions val) =
    put apiIdExplicitAddressingZigBeeCommandFrame >>
    put frameId >>
    put addr >>
    put nwaddr >>
    put srcEP >>
    put destEP >>
    putWord8 0 >>  -- reserved byte is always set to 0
    put clusterId >>
    put profileId >>
    put bcastRadius >>
    put txOptions >>
    putByteString val

  put (ZigBeeTransmitStatus frameId nwaddr txRetryCt delStatus discStatus) =
    put apiIdZigBeeTransmitStatus >>
    put frameId >>
    put nwaddr >>
    put txRetryCt >>
    put delStatus >>
    put discStatus

  put (ZigBeeReceivePacket addr nwaddr rxOptions val) =
    put apiIdZigBeeReceivePacket >>
    put addr >>
    put nwaddr >>
    put rxOptions >>
    putByteString val

  put (ZigBeeExplicitRxIndicator addr nwaddr srcEP destEP clusterId profileId
       rxOptions val) =
    put apiIdZigBeeExplicitRxIndicator >>
    put addr >>
    put nwaddr >>
    put srcEP >>
    put destEP >>
    put clusterId >>
    put profileId >>
    put rxOptions >>
    putByteString val

  put (ZigBeeIODataSampleIndicator addr nwaddr rxOptions sampleCt digChanMask
       anaChanMask val) =
    put apiIdZigBeeIODataSampleIndicator >>
    put addr >>
    put nwaddr >>
    put rxOptions >>
    put sampleCt >>
    putWord16be digChanMask >>
    put anaChanMask >>
    putByteString val

  put (XBeeSensorReadIndicator addr nwaddr rxOptions xbeeSensorMask val) =
    put apiIdXBeeSensorReadIndicator >>
    put addr >>
    put nwaddr >>
    put rxOptions >>
    put xbeeSensorMask >>
    putByteString val

  put (NodeIdentificationIndicator addr nwaddr rxOptions remNWAddr remAddr
       nodeIdStr parentNWAddr devType srcAction profileId mfgId) =
    put apiIdNodeIdentificationIndicator >>
    put addr >>
    put nwaddr >>
    put rxOptions >>
    put remNWAddr >>
    put remAddr >>
    putNullTerminatedString nodeIdStr >>
    put parentNWAddr >>
    put devType >>
    put srcAction >>
    put profileId >>
    put mfgId

  get = getWord8 >>= go
    where
      go apiId
        | apiId == apiIdModemStatus = getModemStatus
        | apiId == apiIdATCommand = getATCommand
        | apiId == apiIdATCommandQueueParameterValue =
            getATCommandQueueParameterValue
        | apiId == apiIdATCommandResponse = getATCommandResponse
        | apiId == apiIdRemoteCommandRequest = getRemoteCommandRequest
        | apiId == apiIdRemoteCommandResponse = getRemoteCommandResponse
        | apiId == apiIdZigBeeTransmitRequest = getZigBeeTransmitRequest
        | apiId == apiIdExplicitAddressingZigBeeCommandFrame =
            getExplicitAddressingZigBeeCommandFrame
        | apiId == apiIdZigBeeTransmitStatus = getZigBeeTransmitStatus
        | apiId == apiIdZigBeeReceivePacket = getZigBeeReceivePacket
        | apiId == apiIdZigBeeExplicitRxIndicator = getZigBeeExplicitRxIndicator
        | apiId == apiIdZigBeeIODataSampleIndicator =
            getZigBeeIODataSampleIndicator
        | apiId == apiIdXBeeSensorReadIndicator = getXBeeSensorReadIndicator
        | apiId == apiIdNodeIdentificationIndicator =
            getNodeIdentificationIndicator
        | otherwise = return $ ApiIdNotImplemented apiId

getModemStatus :: Get Frame
getModemStatus =
  ModemStatus <$>
    get

getATCommand :: Get Frame
getATCommand =
  ATCommand <$>
    get <*>
    get <*>
    getRemainingByteString

getATCommandQueueParameterValue :: Get Frame
getATCommandQueueParameterValue =
  ATCommandQueueParameterValue <$>
    get <*>
    get <*>
    getRemainingByteString

getATCommandResponse :: Get Frame
getATCommandResponse =
  ATCommandResponse <$>
    get <*>
    get <*>
    get <*>
    getRemainingByteString

getRemoteCommandRequest :: Get Frame
getRemoteCommandRequest =
  RemoteCommandRequest <$>
    get <*>
    get <*>
    get <*>
    get <*>
    get <*>
    getRemainingByteString

getRemoteCommandResponse :: Get Frame
getRemoteCommandResponse =
  RemoteCommandResponse <$>
    get <*>
    get <*>
    get <*>
    get <*>
    get <*>
    getRemainingByteString

getZigBeeTransmitRequest :: Get Frame
getZigBeeTransmitRequest =
  ZigBeeTransmitRequest <$>
    get <*>
    get <*>
    get <*>
    get <*>
    get <*>
    getRemainingByteString

getExplicitAddressingZigBeeCommandFrame :: Get Frame
getExplicitAddressingZigBeeCommandFrame =
  ExplicitAddressingZigBeeCommandFrame <$>
    get <*>
    get <*>
    get <*>
    get <*>
    get <*>
    (getWord8 >> get) <*>  -- skip reserved byte
    get <*>
    get <*>
    get <*>
    getRemainingByteString

getZigBeeTransmitStatus :: Get Frame
getZigBeeTransmitStatus =
  ZigBeeTransmitStatus <$>
    get <*>
    get <*>
    get <*>
    get <*>
    get

getZigBeeReceivePacket :: Get Frame
getZigBeeReceivePacket =
  ZigBeeReceivePacket <$>
    get <*>
    get <*>
    get <*>
    getRemainingByteString

getZigBeeExplicitRxIndicator :: Get Frame
getZigBeeExplicitRxIndicator =
  ZigBeeExplicitRxIndicator <$>
    get <*>
    get <*>
    get <*>
    get <*>
    get <*>
    get <*>
    get <*>
    getRemainingByteString

getZigBeeIODataSampleIndicator :: Get Frame
getZigBeeIODataSampleIndicator =
  ZigBeeIODataSampleIndicator <$>
    get <*>
    get <*>
    get <*>
    get <*>
    getWord16be <*>
    get <*>
    getRemainingByteString

getXBeeSensorReadIndicator :: Get Frame
getXBeeSensorReadIndicator =
  XBeeSensorReadIndicator <$>
    get <*>
    get <*>
    get <*>
    get <*>
    getRemainingByteString

getNodeIdentificationIndicator :: Get Frame
getNodeIdentificationIndicator =
  NodeIdentificationIndicator <$>
    get <*>
    get <*>
    get <*>
    get <*>
    get <*>
    getNullTerminatedString <*>
    get <*>
    get <*>
    get <*>
    get <*>
    get

-- | @AT@ command names.  These are limited to two bytes, e.g. @ND@ for
-- neightbor discovery.
data CommandName = CommandName String deriving (Eq, Ord)

instance Show CommandName where
  show (CommandName s) = show s

-- | Construct a @CommandName@.  Beware that this function will 'error' if
-- the name is not exactly two bytes long.
commandName :: String -> CommandName
commandName s
  | length s == 2 = CommandName s
  | otherwise     = error "CommandName must be two characters in length"

-- | Deconstruct a @CommandName@.
unCommandName :: CommandName -> String
unCommandName (CommandName s) = s

instance Serialize CommandName where
  put (CommandName s) = put (s !! 0) >> put (s !! 1)
  get = do
    c1 <- get
    c2 <- get
    return $ CommandName [c1, c2]

-- | All XBee ZNet 2.5 modules are identified by a unique (and static)
-- 64-bit address.
data Address = Address B.ByteString
  deriving (Eq, Ord)

instance Show Address where
  show (Address bs) = showHexAddress bs

instance Serialize Address where
  put (Address bs) = putRawByteString bs
  get              = Address <$> getByteString 8

-- | Construct an @Address@.  Beware that this function will 'error' if
-- the address is not exactly eight bytes long.
address :: B.ByteString -> Address
address bs
  | B.length bs == 8 = Address bs
  | otherwise        = error "Address must be eight bytes in length"

-- | Deconstruct an @Address@.
unAddress :: Address -> B.ByteString
unAddress (Address bs) = bs

-- | When XBee ZNet 2.5 modules join the network they are assigned a 16-bit
-- address.  Note that unlike 'Address' which is unique and static for a
-- given node, a node's @NetworkAddress@ is dynamic and may change over
-- time.
data NetworkAddress = NetworkAddress B.ByteString
  deriving (Eq, Ord)

instance Show NetworkAddress where
  show (NetworkAddress bs) = showHexAddress bs

instance Serialize NetworkAddress where
  put (NetworkAddress bs) = putRawByteString bs
  get                     = NetworkAddress <$> getByteString 2

-- | Construct a @NetworkAddress@.  Beware that this function will 'error' if
-- the address is not exactly two bytes long.
networkAddress :: B.ByteString -> NetworkAddress
networkAddress bs
  | B.length bs == 2 = NetworkAddress bs
  | otherwise        = error "NetworkAddress must be two bytes in length"

-- | Deconstruct a @NetworkAddress@.
unNetworkAddress :: NetworkAddress -> B.ByteString
unNetworkAddress (NetworkAddress bs) = bs

-- Various type aliases used in Frame constructors.
type AnalogChannelMask = Word8
type BroadcastRadius = Word8
type ClusterId = Word8
type CommandOptions = Word8
type CommandStatus = Word8
type DeviceType = Word8
type DeliveryStatus = Word8
type DestinationEndpoint = Word8
type DigitalChannelMask = Word16
type DiscoveryStatus = Word8
type FrameId = Word8
type ManufacturerId = Word8
type ModemStatusByte = Word8
type ParentNetworkAddress = NetworkAddress
type ProfileId = Word8
type ReceiveOptions = Word8
type RemoteAddress = Address
type RemoteNetworkAddress = NetworkAddress
type SampleCount = Word8
type SourceAction = Word8
type SourceEndpoint = Word8
type TransmitOptions = Word8
type TransmitRetryCount = Word8
type XBeeSensorMask = Word8

putRawByteString :: B.ByteString -> PutM ()
putRawByteString bs = mapM_ put $ B.unpack bs

getRemainingByteString :: Get B.ByteString
getRemainingByteString = do
  len <- remaining
  getByteString $ fromIntegral len

putNullTerminatedString :: String -> PutM ()
putNullTerminatedString s = mapM_ put s >> (put $ chr 0)

getNullTerminatedString :: Get String
getNullTerminatedString = get >>= go
  where
    go c | ord c /= 0 = liftM (c:) getNullTerminatedString
         | otherwise  = return []

showHexString :: String -> B.ByteString -> String
showHexString sep bs = intercalate sep $ map (printf "%02x") $ B.unpack bs

showHexAddress :: B.ByteString -> String
showHexAddress = showHexString ":"

