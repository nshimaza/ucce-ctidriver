{-# LANGUAGE ScopedTypeVariables #-}

module Network.CceCti.BitMaskSpec where

import           Data.Binary
import           Data.ByteString.Lazy hiding (all, empty, map, reverse)

import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.ADT

import           Network.CceCti


instance Arbitrary AgentStateMask where
    arbitrary = genericArbitrary
instance ToADTArbitrary AgentStateMask

instance Arbitrary CallVariableMask where
    arbitrary = genericArbitrary
instance ToADTArbitrary CallVariableMask


spec :: Spec
spec = do
    describe "AgentFlagsMask" $ do
        let fullList = [minBound..maxBound :: AgentFlagsMask]

        it "provides equality test" $
            PrimarySupervisorMask == PrimarySupervisorMask `shouldBe` True

        it "provides equality test for generated BitSet" $
            insert SupervisorMask empty == fromList [SupervisorMask] `shouldBe` True

        it "ByteString should be generated from any AgentFlagsMask constructor using fromList" $
            encode (fromList [PrimarySupervisorMask, SupervisorMask]) `shouldBe` pack [0,0x05]

        it "should decode appropriate AgentStateMask element from ByteString" $
            decode (pack [0,0x06]) `shouldBe` fromList [TemporaryAgentMask, SupervisorMask]

        it "fromList doesn't care of order of element in the given list" $
            fromList fullList `shouldBe` (fromList . reverse) fullList

        it "toList always returns list in order" $
            (toList . fromList . reverse) fullList `shouldBe` fullList

        prop "(encode . decode) should be id with 'clean' input" $ forAll (choose (0, 7)) $ \n -> do
            let decoded = (toList . decode) (pack [0,n]) `asTypeOf` fullList
            (encode . fromList . reverse) decoded `shouldBe` pack [0,n]

    describe "AgentStateMask" $ do
        it "ByteString should be generated from any AgentStateMask constructor using fromList" $ do
            encode (fromList [ AgentLoginMask        ]) `shouldBe` pack [0,0,0,0x01]
            encode (fromList [ AgentLogoutMask       ]) `shouldBe` pack [0,0,0,0x02]
            encode (fromList [ AgentNotReadyMask     ]) `shouldBe` pack [0,0,0,0x04]
            encode (fromList [ AgentAvailableMask    ]) `shouldBe` pack [0,0,0,0x08]
            encode (fromList [ AgentTalkingMask      ]) `shouldBe` pack [0,0,0,0x10]
            encode (fromList [ AgentWorkNotReadyMask ]) `shouldBe` pack [0,0,0,0x20]
            encode (fromList [ AgentWorkReadyMask    ]) `shouldBe` pack [0,0,0,0x40]
            encode (fromList [ AgentBusyOtherMask    ]) `shouldBe` pack [0,0,0,0x80]
            encode (fromList [ AgentReservedMask     ]) `shouldBe` pack [0,0,1,0x00]
            encode (fromList [ AgentHoldMask         ]) `shouldBe` pack [0,0,2,0x00]
            encode (fromList [ AgentLoginMask
                             , AgentLogoutMask
                             , AgentNotReadyMask
                             , AgentAvailableMask
                             , AgentTalkingMask
                             , AgentWorkNotReadyMask
                             , AgentWorkReadyMask
                             , AgentBusyOtherMask
                             , AgentReservedMask
                             , AgentHoldMask
                             ]) `shouldBe` pack [0,0,3,0xff]

        it "should decode appropriate AgentStateMask element from ByteString" $ do
            decode (pack [0,0,0,0x01])    `shouldBe` fromList [ AgentLoginMask     ]
            decode (pack [0,0,0,0x02])    `shouldBe` fromList [ AgentLogoutMask    ]
            decode (pack [0,0,0,0x80])    `shouldBe` fromList [ AgentBusyOtherMask ]
            decode (pack [0,0,0x01,0])    `shouldBe` fromList [ AgentReservedMask  ]
            decode (pack [0,0,0x03,0x81]) `shouldBe` fromList [ AgentLoginMask
                                                               , AgentBusyOtherMask
                                                               , AgentReservedMask
                                                               , AgentHoldMask ]

        prop "Inserted AgentStateMask should become a member of resulted BitSet" $ \(m :: AgentStateMask) ->
            insert m empty `shouldSatisfy` member m

        prop "Inserting existing AgentStateMask should result the same BitSet" $ \(m :: AgentStateMask) ->
            insert m (fromList [m]) `shouldBe` fromList [m]

    describe "CallControlMask" $ do
        it "ByteString should be generated from any CallControlMask constructor using fromList" $
            encode (fromList [ ControlQueryAgentStateMask
                             , ControlSendDtmfSignalMask  ]) `shouldBe` pack [0x00, 0x04, 0x00, 0x01]

        it "should decode appropriate CallControlMask element from ByteString" $
            decode (pack [0x00, 0x00, 0x40, 0x08]) `shouldBe` fromList [ ControlAnswerCallMask
                                                                        , ControlTransferCallMask ]

    describe "CallEventMessageMask" $ do
        it "ByteString should be generated from any CallEventMessageMask constructor using fromList" $
            encode (fromList [ CallDeliveredMask
                             , CallTranslationRouteMask
                             , CallAgentGreetingMask ]) `shouldBe` pack [0x08,0x00,0x10,0x01]

        it "should decode appropriate CallEventMessageMask element from ByteString" $
            decode (pack [0x00,0x00,0x60,0x00]) `shouldBe` fromList [ BeginCallMask
                                                                     , EndCallMask ]

    describe "CallVariableMask" $ do
        it "should be encoded into two byte binary data" $
            encode (fromList [ CallVariable1Mask, CallVariable10Mask ]) `shouldBe` pack [2, 1]

        it "should decode appropriate CallVariableMask element from ByteString" $
            decode (pack [1,2]) `shouldBe` fromList [ CallVariable2Mask, CallVariable9Mask ]

        prop "Inserted CallVariableMask should become a member of resulted BitSet" $ \(m :: CallVariableMask) ->
            insert m empty `shouldSatisfy` member m

        prop "Inserting existing CallVariableMask should result the same BitSet" $ \(m :: CallVariableMask) ->
            insert m (fromList [m]) `shouldBe` fromList [m]

    describe "ClassOfDevice" $ do
        it "ByteString should be generated from any ClassOfDevice constructor using fromList" $
            encode (fromList [DevCOtherMask, DevCVoiceMask]) `shouldBe` pack [0,0x90]

        it "should decode appropriate ClassOfDevice element from ByteString" $
            decode (pack [0,0x60]) `shouldBe` fromList [DevCImageMask, DevCDataMask]

    describe "CtiServiceMask" $ do
        let fullList = [minBound..maxBound :: CtiServiceMask]

        it "ByteString should be generated from any CtiServiceMask constructor using fromList" $ do
            encode (fromList [ CtiServiceClientEvents ]) `shouldBe` pack [0,0,0,0x01]
            encode (fromList [ CtiServiceAllEvents    ]) `shouldBe` pack [0,0,0,0x10]
            encode (fromList [ CtiServiceDebug        ]) `shouldBe` pack [0x80,0,0,0]

        it "should decode appropriate CtiServiceMask element from ByteString" $ do
            decode (pack [0,0,0x04,0]) `shouldBe` fromList [ CtiServiceAgentReporting ]
            decode (pack [0,0x40,0,0]) `shouldBe` fromList [ CtiServiceAcdLineOnly    ]

        it "inserted mask should become a member" $
            fullList `shouldSatisfy` all (\m -> member m (insert m empty))

        it "fromList doesn't care of order of element in the given list" $
            fromList fullList `shouldBe` (fromList . reverse) fullList

        it "toList always returns list in order" $
            (toList . fromList . reverse) fullList `shouldBe` fullList

        prop "(encode . decode) should be id with 'clean' input" $ forAll (choose (0, 0xffffffff :: Word32)) $ \n -> do
            let b1 = fromIntegral (n `div` 0x1000000)
                b2 = fromIntegral ((n `div` 0x10000) `mod` 0x100)
                b3 = fromIntegral ((n `div` 0x100) `mod` 0x100)
                b4 = fromIntegral (n `mod` 0x100)
                bs = pack [b1,b2,b3,b4]
                decoded = (toList . decode) bs `asTypeOf` fullList
            (encode . fromList . reverse) decoded `shouldBe` bs
