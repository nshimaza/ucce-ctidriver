{-# LANGUAGE ScopedTypeVariables #-}

module Network.CceCtiSpec where

import           Data.Binary
import qualified Data.ByteString.Lazy as BL

import           Network.CceCti

import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.ADT


{-
Making enumeration types testable with QuickCheck using Arbitrary
-}
instance Arbitrary AgentStateMask where
    arbitrary = genericArbitrary
instance ToADTArbitrary AgentStateMask

instance Arbitrary CallVariableMask where
    arbitrary = genericArbitrary
instance ToADTArbitrary CallVariableMask



spec :: Spec
spec = do
    describe "AgentStateMask" $ do
        it "ByteString should be generated from any AgentStateMask constructor using fromList" $ do
            encode (fromList [ AgentLoginMask        ]) `shouldBe` BL.pack [0,0,0,0x01]
            encode (fromList [ AgentLogoutMask       ]) `shouldBe` BL.pack [0,0,0,0x02]
            encode (fromList [ AgentNotReadyMask     ]) `shouldBe` BL.pack [0,0,0,0x04]
            encode (fromList [ AgentAvailableMask    ]) `shouldBe` BL.pack [0,0,0,0x08]
            encode (fromList [ AgentTalkingMask      ]) `shouldBe` BL.pack [0,0,0,0x10]
            encode (fromList [ AgentWorkNotReadyMask ]) `shouldBe` BL.pack [0,0,0,0x20]
            encode (fromList [ AgentWorkReadyMask    ]) `shouldBe` BL.pack [0,0,0,0x40]
            encode (fromList [ AgentBusyOtherMask    ]) `shouldBe` BL.pack [0,0,0,0x80]
            encode (fromList [ AgentReservedMask     ]) `shouldBe` BL.pack [0,0,1,0x00]
            encode (fromList [ AgentHoldMask         ]) `shouldBe` BL.pack [0,0,2,0x00]
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
                             ]) `shouldBe` BL.pack [0,0,3,0xff]

        it "should decode appropriate AgentStateMask element from ByteString" $ do
            decode (BL.pack [0,0,0,0x01])    `shouldBe` fromList [ AgentLoginMask     ]
            decode (BL.pack [0,0,0,0x02])    `shouldBe` fromList [ AgentLogoutMask    ]
            decode (BL.pack [0,0,0,0x80])    `shouldBe` fromList [ AgentBusyOtherMask ]
            decode (BL.pack [0,0,0x01,0])    `shouldBe` fromList [ AgentReservedMask  ]
            decode (BL.pack [0,0,0x03,0x81]) `shouldBe` fromList [ AgentLoginMask
                                                                 , AgentBusyOtherMask
                                                                 , AgentReservedMask
                                                                 , AgentHoldMask ]

        prop "Inserted AgentStateMask should become a member of resulted BitSet" $ \(m :: AgentStateMask) ->
            member m (insert m empty) `shouldBe` True

        prop "Inserting existing AgentStateMask should result the same BitSet" $ \(m :: AgentStateMask) ->
            insert m (fromList [m]) `shouldBe` fromList [m]


    describe "CallEventMessageMask" $ do
        it "ByteString should be generated from any CallEventMessageMask constructor using fromList" $
            encode (fromList [ CallDeliveredMask
                             , CallTranslationRouteMask
                             , CallAgentGreetingMask ]) `shouldBe` BL.pack [0x08,0x00,0x10,0x01]

        it "should decode appropriate CallEventMessageMask element from ByteString" $
            decode (BL.pack [0x00,0x00,0x60,0x00]) `shouldBe` fromList [ BeginCallMask
                                                                       , EndCallMask ]

    describe "CtiServiceMask" $ do
        it "ByteString should be generated from any CtiServiceMask constructor using fromList" $ do
            encode (fromList [ CtiServiceClientEvents ]) `shouldBe` BL.pack [0,0,0,0x01]
            encode (fromList [ CtiServiceAllEvents    ]) `shouldBe` BL.pack [0,0,0,0x10]
            encode (fromList [ CtiServiceDebug        ]) `shouldBe` BL.pack [0x80,0,0,0]

        it "should decode appropriate CtiServiceMask element from ByteString" $ do
            decode (BL.pack [0,0,0x04,0]) `shouldBe` fromList [ CtiServiceAgentReporting ]
            decode (BL.pack [0,0x40,0,0]) `shouldBe` fromList [ CtiServiceAcdLineOnly    ]


    describe "CallVariableMask" $ do
        it "should be encoded into two byte binary data" $
            encode (fromList [ CallVariable1Mask, CallVariable10Mask ]) `shouldBe` BL.pack [2, 1]

        it "should decode appropriate CallVariableMask element from ByteString" $
            decode (BL.pack [1,2]) `shouldBe` fromList [ CallVariable2Mask, CallVariable9Mask ]

        prop "Inserted CallVariableMask should become a member of resulted BitSet" $ \(m :: CallVariableMask) ->
            member m (insert m empty) `shouldBe` True

        prop "Inserting existing CallVariableMask should result the same BitSet" $ \(m :: CallVariableMask) ->
            insert m (fromList [m]) `shouldBe` fromList [m]
