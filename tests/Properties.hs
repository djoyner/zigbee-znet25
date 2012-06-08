{-# OPTIONS -fno-warn-orphans -fno-warn-unused-binds #-}

import Control.Applicative
import Control.Monad.State
import qualified Data.ByteString as B
import Data.Bits (xor)
import Data.Char (chr)
import Network.Protocol.ZigBee.ZNet25
import System.Exit
import Test.QuickCheck hiding (Result)
import Text.Printf

-- QuickCheck properties
prop_DecodeRandomBufferWithoutException bs =
  seq (evalState (decode bs) initDecode) True

prop_DecodeRandomFrameWithoutException bs =
  seq (evalState (decode $ B.cons 0x7e bs) initDecode) True

prop_EncoderDecoderIdentity f =
    check $ evalState (liftM concat $ mapM decode $ encode f) initDecode
  where
    check ((Right f0):[]) = f == f0
    check _               = False

prop_DecoderDetectsChecksumErrors f =
    seq (check $ evalState (decode f_enc) initDecode, f_munged) True
  where
    check ((Left err):[]) = err == "Checksum error"
    check _               = False
    f_enc                 = B.concat $ encode f
    f_munged              = B.snoc (B.take (B.length f_enc - 1) f_enc) $
                            B.last f_enc `xor` 0x55

-- QuickCheck test runner
test :: Testable a => a -> IO Bool
test t =
  do
    r <- quickCheckResult t
    return $ case r of
      Success _ _ _ -> True
      _             -> False

main :: IO ()
main =
  do
    success <- liftM and $ mapM (\(s, a) -> printf "%-50s: " s >> a) tests
    if success then exitSuccess else exitFailure
  where tests =
          [
              ("decode random buffer w/o exception",
                test prop_DecodeRandomBufferWithoutException)

            , ("decode random frame w/o exception",
               test prop_DecodeRandomFrameWithoutException)

            , ("decoder detects checksum errors",
               test prop_DecoderDetectsChecksumErrors)

            , ("encoder/decoder identity",
               test prop_EncoderDecoderIdentity)
          ]

-- Arbitrary instances
instance Arbitrary Frame where
  arbitrary = oneof
    [
      liftArb1 ModemStatus
    , liftArb3 ATCommand
    , liftArb3 ATCommandQueueParameterValue
    , liftArb4 ATCommandResponse
    , liftArb6 RemoteCommandRequest
    , liftArb6 RemoteCommandResponse
    , liftArb6 ZigBeeTransmitRequest
    , liftArb10 ExplicitAddressingZigBeeCommandFrame
    , liftArb5 ZigBeeTransmitStatus
    , liftArb4 ZigBeeReceivePacket
    , liftArb8 ZigBeeExplicitRxIndicator
    , liftArb7 ZigBeeIODataSampleIndicator
    , liftArb5 XBeeSensorReadIndicator
    , NodeIdentificationIndicator <$>
        arbitrary <*>
        arbitrary <*>
        arbitrary <*>
        arbitrary <*>
        arbitrary <*>
        arbitraryStringBarNull <*>
        arbitrary <*>
        arbitrary <*>
        arbitrary <*>
        arbitrary <*>
        arbitrary
    ]

instance Arbitrary CommandName where
  arbitrary = do
    c1 <- arbitrary
    c2 <- arbitrary
    return $ commandName [c1,c2]

instance Arbitrary Address where
  arbitrary = address <$> (fmap B.pack $ replicateM 8 arbitrary)

instance Arbitrary NetworkAddress where
  arbitrary = networkAddress <$> (fmap B.pack $ replicateM 2 arbitrary)

instance Arbitrary B.ByteString where
  arbitrary = fmap B.pack arbitrary

arbitraryStringBarNull = listOf $ choose (chr 1, chr 255)

liftArb1  f = f <$> arbitrary
liftArb2  f = f <$> arbitrary <*> arbitrary
liftArb3  f = f <$> arbitrary <*> arbitrary <*> arbitrary
liftArb4  f = f <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
liftArb5  f = f <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
liftArb6  f = f <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
liftArb7  f = f <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
liftArb8  f = f <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
liftArb9  f = f <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
liftArb10 f = f <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

