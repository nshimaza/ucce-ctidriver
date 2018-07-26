{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.CceCti.FloatPartSpec where

import           Prelude hiding (length, fromList)

import           Data.Binary
import           Data.ByteString.Lazy
import           Data.Either

import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)

import           Network.CceCti hiding (empty)

spec :: Spec
spec = do
    describe "RouterCallKeyDay Word32" $ do
        it "RouterCallKeyDay 1234 encodes to [0,72,0,4, 0x00,0x00,0x04,0xd2]" $
            encode (RouterCallKeyDay 1234) `shouldBe` pack [0,72,0,4,0,0,0x04,0xd2]

        it "decodes [0,72,0,4, 0x01,0x02,0x03,0x04] to RouterCallKeyDay 16909060" $
            decode (pack [0,72,0,4, 1,2,3,4]) `shouldBe` RouterCallKeyDay 16909060

        prop "Length of encoded RouterCallKeyDay should be 8" $ \(n :: Word32) ->
            (length . encode . RouterCallKeyDay) n == 8

        prop "(decode . encode) should be id" $ \(n :: Word32) ->
            (decode . encode . RouterCallKeyDay) n == RouterCallKeyDay n

    describe "NumServiceMembers Word16" $ do
        it "NumServiceMembers 4321 encodes to [0,186,0,2, 0x10,0xe1]" $
            encode (NumServiceMembers 4321) `shouldBe` pack [0,186,0,2,0x10,0xe1]

        it "decodes [0,186,0,2, 0x02,0x01] to NumServiceMembers 513" $
            decode (pack [0,186,0,2, 2,1]) `shouldBe` NumServiceMembers 513

        prop "Length of encoded SkillGroupPriority should be 6" $ \(n :: Word16) ->
            (length . encode . NumServiceMembers) n == 6

        prop "(decode . encode) should be id" $ \(n :: Word16) ->
            (decode . encode . NumServiceMembers) n == NumServiceMembers n

    describe "DeviceConfigKey Unspec8" $ do
        it "encodes tag and inside Unspec8" $
            (encode . DeviceConfigKey . Unspec8 . pack) [9,8,7,6,5] `shouldBe` pack [0,180,0,5,9,8,7,6,5]

        it "decodes DeviceConfigKey from tag 180" $
            decode (pack [0,180,0,5,5,4,3,2,1]) `shouldBe` (DeviceConfigKey . Unspec8 . pack) [5,4,3,2,1]

        prop "Length of encoded DeviceConfigKey is shorter or equal to 12" $ \(ws :: [Word8]) ->
            (length . encode . DeviceConfigKey . Unspec8 . pack) ws <= 12

    describe "SkillGroupState (SafeEnum AgentState)" $ do
        it "encodes enumeration index into binary" $
            (encode . SkillGroupState . SafeEnum . Right) AgentStateNotReady `shouldBe` pack [0,65, 0,2, 0,2]

        it "decodes enumeration index to Enum value" $
            decode (pack [0,65, 0,2, 0,4]) `shouldBe` (SkillGroupState . SafeEnum . Right) AgentStateTalking

        it "decodes unrecognized index to Left" $ do
            let (SkillGroupState (SafeEnum either)) = decode (pack [0,65, 0,2, 1,0])
            isLeft either `shouldBe` True

    describe "AgentFlags (BitSet (Holder AgentFlagsMask) AgentFlagsMask)" $ do
        it "encodes tag and mask into Word16 bitwise set" $ do
            let elem = AgentFlags (fromList [PrimarySupervisorMask, SupervisorMask])
            encode elem `shouldBe` pack [0,87, 0,2, 0x00,0x05]

        it "decodes Word16 bitwise set to BitMask contained by FloatElem" $
            decode (pack [0,87, 0,2, 0,2]) `shouldBe` AgentFlags (fromList [TemporaryAgentMask])

    describe "NamedVariable" $ do
        it "encodes variable with name and value" $ do
            let name = "variable name comes here"
                val = "hello, world"
                namedVar = NamedVariable (NamedVar name val)
            encode namedVar `shouldBe` pack [ 0,82, 0,38
                                            , 0x76,0x61,0x72,0x69,0x61,0x62,0x6c,0x65,0x20,0x6e
                                            , 0x61,0x6d,0x65,0x20,0x63,0x6f,0x6d,0x65,0x73,0x20
                                            , 0x68,0x65,0x72,0x65,0
                                            , 0x68,0x65,0x6c,0x6c,0x6f,0x2c,0x20,0x77,0x6f,0x72
                                            , 0x6c,0x64,0 ]

        it "encodes empty name and value" $
            encode (NamedVariable (NamedVar empty empty)) `shouldBe` pack [0,82, 0,2, 0,0]

        it "decodes variable name and value from tag 82" $
            True `shouldBe` True

        it "decodes completely empty payload" $
            decode (pack [0,82, 0,0]) `shouldBe` NamedVariable (NamedVar empty empty)
