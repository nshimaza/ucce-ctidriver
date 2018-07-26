{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.CceCti.MessageSpec where

import           Data.Binary
import           Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as C

import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)

import           Network.CceCti

spec :: Spec
spec = do
    describe "FailureConfR" $ do
        let body = FailureConfR (InvokeID 0x01020304) (SafeEnum (Right ECtiInvalidMonitorStatus))
            raw = pack [1,2,3,4, 0,0,0,97]

        it "encodes invoke ID and status code" $
            encode body `shouldBe` raw

        it "decodes invokeID and status code" $
            decode raw `shouldBe` body

    describe "Fixed part of FailureConf" $ do
        let fixed = FailureConf $ FailureConfR (InvokeID 0x01020304) (SafeEnum (Right ECtiInvalidMonitorStatus))
            raw = pack [0,0,0,1, 1,2,3,4, 0,0,0,97]

        it "encodes message type, invoke ID and status code" $
            encode fixed `shouldBe` raw

        it "decodes message type, invokeID and status code" $
            decode raw `shouldBe` fixed

    describe "FailureConf" $ do
        let fixed = FailureConf $ FailureConfR (InvokeID 0x01020304) (SafeEnum (Right ECtiInvalidMonitorStatus))
            msg = Message fixed (FloatPart [])
            raw = pack [0,0,0,1, 1,2,3,4, 0,0,0,97]

        it "encodes message type, invoke ID and status code" $
            encode msg `shouldBe` raw

        it "decodes message type, invokeID and status code" $
            decode raw `shouldBe` msg

    describe "FailureEvent" $ do
        let fixed = FailureEvent $ FailureEventR (SafeEnum (Right ECtiUnspecifiedFailure))
            msg = Message fixed (FloatPart [])
            raw = pack [0,0,0,2, 0,0,0,17]

        it "encodes message type and status code" $
            encode msg `shouldBe` raw

        it "decodes message type and status code" $
            decode raw `shouldBe` msg

    describe "OpenReq" $ do
        let fixed = OpenReq OpenReqR {
                  openReqInvokeID = InvokeID 0x04030201
                , openReqVersionNumber = protocolVersion
                , openReqIdleTimeout = CtiOpt Nothing
                , openReqPeripheralID = PeripheralID (CtiOpt (Just 0x02030405))
                , openReqserviceRequested = fromList [CtiServiceAllEvents]
                , openReqcallMsgMask = fromList [BeginCallMask, EndCallMask]
                , openReqagentStateMask = fromList [AgentAvailableMask, AgentHoldMask]
                , openReqconfigMsgMask = fromList []
                , openReqReserved1 = 0
                , openReqReserved2 = 0
                , openReqReserved3 = 0
                }
            float = FloatPart
                [ ClientID "ClientID"
                , ClientPassword (fromWord8List [])
                , ClientSignature "ClientSignature"
                , AgentExtension "3001"
                , AgentID "1001"
                , AgentInstrument "3001"
                , AppPathID 0x03040506
                ]
            msg = Message fixed float
            raw = pack
                [ 0,0,0,3, 4,3,2,1, 0,0,0,20, 0xff,0xff,0xff,0xff, 2,3,4,5, 0,0,0,0x10
                , 0,0,0x60,0, 0,0,0x02,0x08, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0
                , 0,1,0,9,  0x43, 0x6c,0x69,0x65,0x6e,0x74,0x49,0x44,0
                , 0,2,0,0
                , 0,3,0,16, 0x43,0x6c,0x69,0x65,0x6e,0x74,0x53,0x69,0x67,0x6e,0x61,0x74,0x75,0x72,0x65,0
                , 0,4,0,5,  0x33,0x30,0x30,0x31,0
                , 0,5,0,5,  0x31,0x30,0x30,0x31,0
                , 0,6,0,5,  0x33,0x30,0x30,0x31,0
                , 0,97,0,4, 3,4,5,6
                ]

        it "encodes message" $
            encode msg `shouldBe` raw

        it "decodes message" $
            decode raw `shouldBe` msg

    describe "OpenConf" $ do
        let fixed = OpenConf OpenConfR {
                  openConfInvokeID = InvokeID 0x01020304
                , openConfServiceGranted = fromList [CtiServiceAllEvents]
                , openConfMonitorID = MonitorID 0x02030405
                , openConfPGStatus = fromList [PgsOpcDown, PgsCCDown, PgsLimitedFunction]
                , openConfIcmCentralControllerTime = Timestamp 0x04050607
                , openConfPeripheralOnline = SafeEnum (Right CtiBoolTrue)
                , openConfPeripheralType = SafeEnum (Right (CtiOpt (Just PeripheralTypeEnterpriseAgent)))
                , openConfAgentState = SafeEnum (Right AgentStateAvailable)
                , openConfDepartmentID = DepartmentID 0x05060708
                }
            float = FloatPart
                [ AgentExtension "3001"
                , AgentID "1001"
                , AgentInstrument "3001"
                , NumPeripherals 0x0506
                , MultiLineAgnetControl (SafeEnum (Right MultilineAgentControlSingleLineOnly))
                ]
            msg = Message fixed float
            raw = pack
                [ 0,0,0,4, 1,2,3,4, 0,0,0,0x10, 2,3,4,5, 0,0,0,0x13, 4,5,6,7, 0,1, 0,17, 0,3, 5,6,7,8
                , 0,4,0,5,   0x33,0x30,0x30,0x31,0
                , 0,5,0,5,   0x31,0x30,0x30,0x31,0
                , 0,6,0,5,   0x33,0x30,0x30,0x31,0
                , 0,228,0,2, 5,6
                , 0,224,0,2, 0,0
                ]

        it "encodes message" $
            encode msg `shouldBe` raw

        it "decodes message" $
            decode raw `shouldBe` msg

    describe "HeartBeatReq" $ do
        let fixed = HeartBeatReq $ HeartBeatReqR (InvokeID 0x02030405)
            msg = Message fixed (FloatPart [])
            raw = pack [0,0,0,5, 2,3,4,5]

        it "encodes message" $
            encode msg `shouldBe` raw

        it "decodes message" $
            decode raw `shouldBe` msg

    describe "HeartBeatConf" $ do
        let fixed = HeartBeatConf $ HeartBeatConfR (InvokeID 0x03040607)
            msg = Message fixed (FloatPart [])
            raw = pack [0,0,0,6, 3,4,6,7]

        it "encodes message" $
            encode msg `shouldBe` raw

        it "decodes message" $
            decode raw `shouldBe` msg

    describe "CloseReq" $ do
        let fixed = CloseReq CloseReqR {
              closeReqInvokeID = InvokeID 0x04050607
            , closeReqStatus = SafeEnum (Right ECtiInvalidRequestType)
        }
            msg = Message fixed (FloatPart [])
            raw = pack [0,0,0,7, 4,5,6,7, 0,0,0,98]

        it "encodes message" $
            encode msg `shouldBe` raw

        it "decodes message" $
            decode raw `shouldBe` msg

    describe "CloseConf" $ do
        let fixed = CloseConf $ CloseConfR (InvokeID 0x03040506)
            msg = Message fixed (FloatPart [])
            raw = pack [0,0,0,8, 3,4,5,6]

        it "encodes message" $
            encode msg `shouldBe` raw

        it "decodes message" $
            decode raw `shouldBe` msg


    describe "BeginCallEvent" $ do
        let fixed = BeginCallEvent BeginCallEventR {
                  beginCallEventMonitorID = MonitorID 0x01020304
                , beginCallEventPeripheralID = PeripheralID (CtiOpt (Just 0x02030405))
                , beginCallEventPeripheralType = SafeEnum (Right (CtiOpt (Just PeripheralTypeEnterpriseAgent)))
                , beginCallEventNumCtiClients = 0x0304
                , beginCallEventNumNamedVariables = 0x0405
                , beginCallEventNumNamedArrays = 0x0506
                , beginCallEventCallType = SafeEnum (Right (CtiOpt (Just CallTypePreRouteAcdIn)))
                , beginCallEventConnectionDeviceIDType = SafeEnum (Right (CtiOpt (Just ConnectionDevIDStatic)))
                , beginCallEventConnectionCallID = ConnectionCallID (CtiOpt (Just 0x06070809))
                , beginCallEventCalledPartyDisposition = SafeEnum (Right DispositionDisconnectDropHandledPrimaryRoute)
                }
            float = FloatPart
                [ ConnectionDevID "ConnectionDeviceID"
                , Ani "09012345678"
                , UserToUserInfo (fromWord8List [0,1,2,3,4,5,6,7,8,9])
                , Dnis "0398765432"
                , DialedNumber "0120123123"
                , CallerEnteredDigit "321"
                , RouterCallKeyDay 0x06070809
                , RouterCallkeyCallID 0x0708090a
                , RouterCallKeySequenceNum 0x08090a0b
                , CallVar1 "CV1"
                , CallVar2 "CV2"
                , CallVar3 "CV3"
                , CallVar4 "CV4"
                , CallVar5 "CV5"
                , CallVar6 "CV6"
                , CallVar7 "CV7"
                , CallVar8 "CV8"
                , CallVar9 "CV9"
                , CallVar10 "CV10"
                , CallWrapupData "Wrapup"
                , NamedVariable (NamedVar "ECCVar" "ECCVal")
                , NamedArray (NamedArr 1 "ECCArr" "ECCArrVal")
                , CtiClientSignature "CtiClientSignature"
                , CtiClientTimestamp (Timestamp 0x090a0b0c)
                , CallReferenceID (fromWord8List [9,8,7,6,5,4,3,2,1,0])
                ]
            msg = Message fixed float
            raw = pack
                [ 0,0,0,23, 1,2,3,4, 2,3,4,5, 0,17, 3,4, 4,5, 5,6, 0,2, 0,0, 6,7,8,9, 0,13
                , 0,25,0,19,  0x43,0x6f,0x6e,0x6e,0x65,0x63,0x74,0x69,0x6f,0x6e,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,8,0,12,   0x30, 0x39, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38,0
                , 0,9,0,10,   0,1,2,3,4,5,6,7,8,9
                , 0,10,0,11,  0x30, 0x33, 0x39, 0x38, 0x37, 0x36, 0x35, 0x34, 0x33, 0x32,0
                , 0,11,0,11,  0x30, 0x31, 0x32, 0x30, 0x31, 0x32, 0x33, 0x31, 0x32, 0x33,0
                , 0,12,0,4,   0x33, 0x32, 0x31,0
                , 0,72,0,4,   6,7,8,9
                , 0,73,0,4,   7,8,9,10
                , 0,110,0,4,  8,9,10,11
                , 0,13,0,4,   0x43,0x56,0x31,0
                , 0,14,0,4,   0x43,0x56,0x32,0
                , 0,15,0,4,   0x43,0x56,0x33,0
                , 0,16,0,4,   0x43,0x56,0x34,0
                , 0,17,0,4,   0x43,0x56,0x35,0
                , 0,18,0,4,   0x43,0x56,0x36,0
                , 0,19,0,4,   0x43,0x56,0x37,0
                , 0,20,0,4,   0x43,0x56,0x38,0
                , 0,21,0,4,   0x43,0x56,0x39,0
                , 0,22,0,5,   0x43,0x56,0x31,0x30,0
                , 0,46,0,7,   0x57,0x72,0x61,0x70,0x75,0x70,0
                , 0,82,0,14,  0x45,0x43,0x43,0x56,0x61,0x72,0, 0x45,0x43,0x43,0x56,0x61,0x6c,0
                , 0,83,0,18,  1, 0x45,0x43,0x43,0x41,0x72,0x72,0, 0x45,0x43,0x43,0x41,0x72,0x72,0x56,0x61,0x6c,0
                , 0,23,0,19,  0x43,0x74,0x69,0x43,0x6c,0x69,0x65,0x6e,0x74,0x53,0x69,0x67,0x6e,0x61,0x74,0x75,0x72,0x65,0
                , 0,24,0,4,   9,10,11,12
                , 0,223,0,10, 9,8,7,6,5,4,3,2,1,0
                ]

        it "encodes message" $
            encode msg `shouldBe` raw

        it "decodes message" $
            decode raw `shouldBe` msg

    describe "EndCallEvent" $ do
        let fixed = EndCallEvent EndCallEventR
                { endCallEventMonitorID = MonitorID 0x01020304
                , endCallEventPeripheralID = PeripheralID (CtiOpt (Just 0x02030405))
                , endCallEventPeripheralType = SafeEnum (Right (CtiOpt (Just PeripheralTypeEnterpriseAgent)))
                , endCallEventConnectionDeviceIDType = SafeEnum (Right (CtiOpt (Just ConnectionDevIDStatic)))
                , endCallEventConnectionCallID = ConnectionCallID (CtiOpt (Just 0x06070809))
                }
            float = FloatPart [ConnectionDevID "ConnectionDeviceID"]
            msg = Message fixed float
            raw = pack
                [ 0,0,0,24, 1,2,3,4, 2,3,4,5, 0,17, 0,0, 6,7,8,9
                , 0,25,0,19, 0x43,0x6f,0x6e,0x6e,0x65,0x63,0x74,0x69,0x6f,0x6e,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                ]

        it "encodes message" $
            encode msg `shouldBe` raw

        it "decodes message" $
            decode raw `shouldBe` msg
