{-# LANGUAGE OverloadedStrings #-}

module Network.CceCti.MessageSampleSpec where

import           Control.Monad        (forM_)
import           Data.Binary
import           Data.ByteString.Lazy (pack)
import           Data.Word            (Word8)

import           Test.Hspec

import           Network.CceCti

spec :: Spec
spec =
    describe "MessageSampls" $ do
        it "encodes all sample messages" $
            forM_ messageSamples $ \(MessageSample name raw msg) ->
                (name, encode msg) `shouldBe` (name, pack raw)

        it "decodes all sample messages" $
            forM_ messageSamples $ \(MessageSample name raw msg) ->
             (name, decode (pack raw)) `shouldBe` (name, msg)


data MessageSample = MessageSample String [Word8] Message

messageSamples :: [MessageSample]
messageSamples = [
      let name = "FAILURE_CONF"
          fixed = FailureConf $ FailureConfR (InvokeID 0x01020304) (SafeEnum (Right ECtiInvalidMonitorStatus))
          raw = [ 0,0,0,1, 1,2,3,4, 0,0,0,97 ]
      in MessageSample name raw (Message fixed (FloatPart []))

    , let name = "FAILURE_EVENT"
          fixed = FailureEvent $ FailureEventR (SafeEnum (Right ECtiUnspecifiedFailure))
          raw = [ 0,0,0,2, 0,0,0,17 ]
      in MessageSample name raw (Message fixed (FloatPart []))

    , let name = "OPEN_REQ"
          fixed = OpenReq OpenReqR
                { openReqInvokeID = InvokeID 0x04030201
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
          raw = [ 0,0,0,3, 4,3,2,1, 0,0,0,20, 0xff,0xff,0xff,0xff, 2,3,4,5, 0,0,0,0x10
                , 0,0,0x60,0, 0,0,0x02,0x08, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0
                , 0,1,0,9,  0x43, 0x6c,0x69,0x65,0x6e,0x74,0x49,0x44,0
                , 0,2,0,0
                , 0,3,0,16, 0x43,0x6c,0x69,0x65,0x6e,0x74,0x53,0x69,0x67,0x6e,0x61,0x74,0x75,0x72,0x65,0
                , 0,4,0,5,  0x33,0x30,0x30,0x31,0
                , 0,5,0,5,  0x31,0x30,0x30,0x31,0
                , 0,6,0,5,  0x33,0x30,0x30,0x31,0
                , 0,97,0,4, 3,4,5,6
                ]
      in MessageSample name raw (Message fixed float)

    , let name = "OPEN_CONF"
          fixed = OpenConf OpenConfR
                { openConfInvokeID = InvokeID 0x01020304
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
          raw = [ 0,0,0,4, 1,2,3,4, 0,0,0,0x10, 2,3,4,5, 0,0,0,0x13, 4,5,6,7, 0,1, 0,17, 0,3, 5,6,7,8
                , 0,4,0,5,   0x33,0x30,0x30,0x31,0
                , 0,5,0,5,   0x31,0x30,0x30,0x31,0
                , 0,6,0,5,   0x33,0x30,0x30,0x31,0
                , 0,228,0,2, 5,6
                , 0,224,0,2, 0,0
                ]
      in MessageSample name raw (Message fixed float)

    , let name = "HEARTBEAT_REQ"
          fixed = HeartBeatReq $ HeartBeatReqR (InvokeID 0x02030405)
          raw = [ 0,0,0,5, 2,3,4,5 ]
      in MessageSample name raw (Message fixed (FloatPart []))

    , let name = "HEARTBEAT_CONF"
          fixed = HeartBeatConf $ HeartBeatConfR (InvokeID 0x03040607)
          raw = [ 0,0,0,6, 3,4,6,7 ]
      in MessageSample name raw (Message fixed (FloatPart []))

    , let name = "CLOSE_REQ"
          fixed = CloseReq CloseReqR
                { closeReqInvokeID = InvokeID 0x04050607
                , closeReqStatus = SafeEnum (Right ECtiInvalidRequestType)
                }
          raw = [ 0,0,0,7, 4,5,6,7, 0,0,0,98 ]
      in MessageSample name raw (Message fixed (FloatPart []))

    , let name = "CLOSE_CONF"
          fixed = CloseConf $ CloseConfR (InvokeID 0x03040506)
          raw = [ 0,0,0,8, 3,4,5,6 ]
      in MessageSample name raw (Message fixed (FloatPart []))

    , let name = "CALL_DELIVERED_EVENT"
          fixed = CallDeliveredEvent CallDeliveredEventR
                { callDeliveredEventMonitorID = MonitorID 0x01020304
                , callDeliveredEventPeripheralID = PeripheralID (CtiOpt (Just 0x02030405))
                , callDeliveredEventPeripheralType = SafeEnum (Right (CtiOpt (Just PeripheralTypeEnterpriseAgent)))
                , callDeliveredEventConnectionDeviceIDType = SafeEnum (Right (CtiOpt (Just ConnectionDevIDStatic)))
                , callDeliveredEventConnectionCallID = ConnectionCallID (CtiOpt (Just 0x06070809))
                , callDeliveredEventLineHandle = LineHandle (CtiOpt (Just 0x0304))
                , callDeliveredEventLineType = SafeEnum (Right (CtiOpt (Just LineTypeSupervisor)))
                , callDeliveredEventServiceNumber = ServiceNumber (CtiOpt (Just 0x04050607))
                , callDeliveredEventServiceID = ServiceID (CtiOpt (Just 0x05060708))
                , callDeliveredEventSkillGroupNumber = SkillGroupNumber (CtiOpt (Just 0x04050607))
                , callDeliveredEventSkillGroupID = SkillGroupID (CtiOpt (Just 0x05060708))
                , callDeliveredEventSkillGroupPriority = SkillGroupPriority (CtiOpt0 (Just 0x0607))
                , callDeliveredEventAlertingDeviceType = SafeEnum (Right (CtiOpt (Just DevIDAgentDevice)))
                , callDeliveredEventCallingDeviceType = SafeEnum (Right (CtiOpt (Just DevIDExternal)))
                , callDeliveredEventCalledDeviceType = SafeEnum (Right (CtiOpt (Just DevIDRoutePoint)))
                , callDeliveredEventLastRedirectDeviceType = SafeEnum (Right (CtiOpt (Just DevIDCtiPort)))
                , callDeliveredEventLocalConnectionState = SafeEnum (Right (CtiOpt (Just LocalConnectionStateNull)))
                , callDeliveredEventEventCause = SafeEnum (Right (CtiOpt Nothing))
                , callDeliveredEventNumNamedVariables = 0x0405
                , callDeliveredEventNumNamedArrays = 0x0506
                }
          float = FloatPart
                [ ConnectionDevID "ConnectionDeviceID"
                , AlertingDevID "AlertingDeviceID"
                , CallingDevID "CallingDeviceID"
                , CalledDevID "CalledDeviceID"
                , LastRedirectDevID "LastRedirectDeviceID"
                , TrunkNumber 0x01020304
                , TrunkGroupNumber 0x02030405
                , SecondaryConnCallID 0x03040506
                , Ani "09012345678"
                , AniII ""
                , UserToUserInfo (Unspec131 (pack [0,1,2,3,4,5,6,7,8,9]))
                , Dnis "0398765432"
                , DialedNumber "0120123123"
                , CallerEnteredDigit "321"
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
                ]
          raw = [ 0,0,0,9, 1,2,3,4, 2,3,4,5, 0,17, 0,0, 6,7,8,9, 3,4, 0,4
                , 4,5,6,7, 5,6,7,8, 4,5,6,7, 5,6,7,8, 6,7, 0,76, 0,75, 0,74, 0,73, 0,0, 0xff,0xff, 4,5, 5,6
                , 0,25,0,19,  0x43,0x6f,0x6e,0x6e,0x65,0x63,0x74,0x69,0x6f,0x6e,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,26,0,17,  0x41,0x6c,0x65,0x72,0x74,0x69,0x6e,0x67,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,27,0,16,  0x43,0x61,0x6c,0x6c,0x69,0x6e,0x67,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,28,0,15,  0x43,0x61,0x6c,0x6c,0x65,0x64,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,29,0,21,  0x4c,0x61,0x73,0x74,0x52,0x65,0x64,0x69,0x72,0x65,0x63,0x74,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,121,0,4,  1,2,3,4
                , 0,122,0,4,  2,3,4,5
                , 0,202,0,4,  3,4,5,6
                , 0,8,0,12,   0x30, 0x39, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38,0
                , 0,215,0,1,  0
                , 0,9,0,10,   0,1,2,3,4,5,6,7,8,9
                , 0,10,0,11,  0x30, 0x33, 0x39, 0x38, 0x37, 0x36, 0x35, 0x34, 0x33, 0x32,0
                , 0,11,0,11,  0x30, 0x31, 0x32, 0x30, 0x31, 0x32, 0x33, 0x31, 0x32, 0x33,0
                , 0,12,0,4,   0x33, 0x32, 0x31,0
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
                , 0,82,0,14,  0x45,0x43,0x43,0x56,0x61,0x72,0,0x45,0x43,0x43,0x56,0x61,0x6c,0
                , 0,83,0,18,  1,0x45,0x43,0x43,0x41,0x72,0x72,0, 0x45,0x43,0x43,0x41,0x72,0x72,0x56,0x61,0x6c,0
                ]
      in MessageSample name raw (Message fixed float)

    , let name = "CALL_ESTABLISHED_EVENT"
          fixed = CallEstablishedEvent CallEstablishedEventR
                { callEstablishedEventMonitorID = MonitorID 0x01020304
                , callEstablishedEventPeripheralID = PeripheralID (CtiOpt (Just 0x02030405))
                , callEstablishedEventPeripheralType = SafeEnum (Right (CtiOpt (Just PeripheralTypeEnterpriseAgent)))
                , callEstablishedEventConnectionDeviceIDType = SafeEnum (Right (CtiOpt (Just ConnectionDevIDDynamic)))
                , callEstablishedEventConnectionCallID = ConnectionCallID (CtiOpt (Just 0x03040506))
                , callEstablishedEventLineHandle = LineHandle (CtiOpt (Just 0x0405))
                , callEstablishedEventLineType = SafeEnum (Right (CtiOpt (Just LineTypeSupervisor)))
                , callEstablishedEventServiceNumber = ServiceNumber (CtiOpt (Just 0x05060708))
                , callEstablishedEventServiceID = ServiceID (CtiOpt (Just 0x06070809))
                , callEstablishedEventSkillGroupNumber = SkillGroupNumber (CtiOpt (Just 0x0708090a))
                , callEstablishedEventSkillGroupID = SkillGroupID (CtiOpt (Just 0x08090a0b))
                , callEstablishedEventSkillGroupPriority = SkillGroupPriority (CtiOpt0 (Just 0x090a))
                , callEstablishedEventAnsweringDeviceType = SafeEnum (Right (CtiOpt (Just DevIDAgentDevice)))
                , callEstablishedEventCallingDeviceType = SafeEnum (Right (CtiOpt (Just DevIDExternal)))
                , callEstablishedEventCalledDeviceType = SafeEnum (Right (CtiOpt (Just DevIDRoutePoint)))
                , callEstablishedEventLastRedirectDeviceType = SafeEnum (Right (CtiOpt (Just DevIDCtiPort)))
                , callEstablishedEventLocalConnectionState = SafeEnum (Right (CtiOpt (Just LocalConnectionStateConnect)))
                , callEstablishedEventEventCause = SafeEnum (Right (CtiOpt (Just EventCauseNewCall)))
                }
          float = FloatPart
                [ ConnectionDevID "ConnectionDeviceID"
                , AnsweringDevID "AnsweringDeviceID"
                , CallingDevID "CallingDeviceID"
                , CalledDevID "CalledDeviceID"
                , LastRedirectDevID "LastRedirectDeviceID"
                , TrunkNumber 0x01020304
                , TrunkGroupNumber 0x02030405
                ]
          raw = [ 0,0,0,10, 1,2,3,4, 2,3,4,5, 0,17, 0,1, 3,4,5,6, 4,5, 0,4, 5,6,7,8, 6,7,8,9
                , 7,8,9,10, 8,9,10,11, 9,10, 0,76, 0,75, 0,74, 0,73, 0,3, 0,22
                , 0,25,0,19,  0x43,0x6f,0x6e,0x6e,0x65,0x63,0x74,0x69,0x6f,0x6e,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,30,0,18,  0x41,0x6e,0x73,0x77,0x65,0x72,0x69,0x6e,0x67,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,27,0,16,  0x43,0x61,0x6c,0x6c,0x69,0x6e,0x67,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,28,0,15,  0x43,0x61,0x6c,0x6c,0x65,0x64,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,29,0,21,  0x4c,0x61,0x73,0x74,0x52,0x65,0x64,0x69,0x72,0x65,0x63,0x74,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,121,0,4,  1,2,3,4
                , 0,122,0,4,  2,3,4,5
                ]
      in MessageSample name raw (Message fixed float)

    , let name = "CALL_HELD_EVENT"
          fixed = CallHeldEvent CallHeldEventR
                { callHeldEventMonitorID = MonitorID 0x01020304
                , callHeldEventPeripheralID = PeripheralID (CtiOpt (Just 0x02030405))
                , callHeldEventPeripheralType = SafeEnum (Right (CtiOpt (Just PeripheralTypeEnterpriseAgent)))
                , callHeldEventConnectionDeviceIDType = SafeEnum (Right (CtiOpt (Just ConnectionDevIDDynamic)))
                , callHeldEventConnectionCallID = ConnectionCallID (CtiOpt (Just 0x03040506))
                , callHeldEventHoldingDeviceType = SafeEnum (Right (CtiOpt (Just DevIDExternal)))
                , callHeldEventLocalConnectionState = SafeEnum (Right (CtiOpt (Just LocalConnectionStateInitiate)))
                , callHeldEventEventCause = SafeEnum (Right (CtiOpt (Just EventCauseNewCall)))
                }
          float = FloatPart
                [ ConnectionDevID "ConnectionDeviceID"
                , HoldingDevID "HoldingDeviceID"
                ]
          raw = [ 0,0,0,11, 1,2,3,4, 2,3,4,5, 0,17, 0,1, 3,4,5,6, 0,75, 0,1, 0,22
                , 0,25,0,19,  0x43,0x6f,0x6e,0x6e,0x65,0x63,0x74,0x69,0x6f,0x6e,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,31,0,16,  0x48,0x6f,0x6c,0x64,0x69,0x6e,0x67,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                ]
      in MessageSample name raw (Message fixed float)

    , let name = "CALL_RETRIEVED_EVENT"
          fixed = CallRetrievedEvent CallRetrievedEventR
                { callRetrievedEventMonitorID = MonitorID 0x01020304
                , callRetrievedEventPeripheralID = PeripheralID (CtiOpt (Just 0x02030405))
                , callRetrievedEventPeripheralType = SafeEnum (Right (CtiOpt (Just PeripheralTypeEnterpriseAgent)))
                , callRetrievedEventConnectionDeviceIDType = SafeEnum (Right (CtiOpt (Just ConnectionDevIDDynamic)))
                , callRetrievedEventConnectionCallID = ConnectionCallID (CtiOpt (Just 0x03040506))
                , callRetrievedEventRetrievingDeviceType = SafeEnum (Right (CtiOpt (Just DevIDExternal)))
                , callRetrievedEventLocalConnectionState = SafeEnum (Right (CtiOpt (Just LocalConnectionStateInitiate)))
                , callRetrievedEventEventCause = SafeEnum (Right (CtiOpt (Just EventCauseNewCall)))
                }
          float = FloatPart
                [ ConnectionDevID "ConnectionDeviceID"
                , RetrievingDevID "RetrievingDeviceID"
                ]
          raw = [ 0,0,0,12, 1,2,3,4, 2,3,4,5, 0,17, 0,1, 3,4,5,6, 0,75, 0,1, 0,22
                , 0,25,0,19,  0x43,0x6f,0x6e,0x6e,0x65,0x63,0x74,0x69,0x6f,0x6e,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,32,0,19,  0x52,0x65,0x74,0x72,0x69,0x65,0x76,0x69,0x6e,0x67,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                ]
      in MessageSample name raw (Message fixed float)

    , let name = "CALL_CLEARD_EVENT"
          fixed = CallClearedEvent CallClearedEventR
                { callClearedEventMonitorID = MonitorID 0x01020304
                , callClearedEventPeripheralID = PeripheralID (CtiOpt (Just 0x02030405))
                , callClearedEventPeripheralType = SafeEnum (Right (CtiOpt (Just PeripheralTypeEnterpriseAgent)))
                , callClearedEventConnectionDeviceIDType = SafeEnum (Right (CtiOpt (Just ConnectionDevIDStatic)))
                , callClearedEventConnectionCallID = ConnectionCallID (CtiOpt (Just 0x06070809))
                , callClearedEventLocalConnectionState = SafeEnum (Right (CtiOpt (Just LocalConnectionStateNull)))
                , callClearedEventEventCause = SafeEnum (Right (CtiOpt (Just EventCauseCallCancelled)))
                }
          float = FloatPart
                [ ConnectionDevID "ConnectionDeviceID"
                ]
          raw = [ 0,0,0,13, 1,2,3,4, 2,3,4,5, 0,17, 0,0, 6,7,8,9, 0,0, 0,5
                , 0,25,0,19,  0x43,0x6f,0x6e,0x6e,0x65,0x63,0x74,0x69,0x6f,0x6e,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                ]
      in MessageSample name raw (Message fixed float)

    , let name = "CALL_CONNECTION_CLEARD_EVENT"
          fixed = CallConnectionClearedEvent CallConnectionClearedEventR
                { callConnectionClearedEventMonitorID = MonitorID 0x01020304
                , callConnectionClearedEventPeripheralID = PeripheralID (CtiOpt (Just 0x02030405))
                , callConnectionClearedEventPeripheralType = SafeEnum (Right (CtiOpt (Just PeripheralTypeEnterpriseAgent)))
                , callConnectionClearedEventConnectionDeviceIDType = SafeEnum (Right (CtiOpt (Just ConnectionDevIDStatic)))
                , callConnectionClearedEventConnectionCallID = ConnectionCallID (CtiOpt (Just 0x06070809))
                , callConnectionClearedEventReleasingDeviceType = SafeEnum (Right (CtiOpt (Just DevIDExternal)))
                , callConnectionClearedEventLocalConnectionState = SafeEnum (Right (CtiOpt (Just LocalConnectionStateNull)))
                , callConnectionClearedEventEventCause = SafeEnum (Right (CtiOpt (Just EventCauseCallCancelled)))
                }
          float = FloatPart
                [ ConnectionDevID "ConnectionDeviceID"
                , ReleasingDevID "ReleasingDeviceID"
                ]
          raw = [ 0,0,0,14, 1,2,3,4, 2,3,4,5, 0,17, 0,0, 6,7,8,9, 0,75, 0,0, 0,5
                , 0,25,0,19,  0x43,0x6f,0x6e,0x6e,0x65,0x63,0x74,0x69,0x6f,0x6e,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,33,0,18,  0x52,0x65,0x6c,0x65,0x61,0x73,0x69,0x6e,0x67,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                ]
      in MessageSample name raw (Message fixed float)

    , let name = "CALL_ORIGINATED_EVENT"
          fixed = CallOriginatedEvent CallOriginatedEventR
                { callOriginatedEventMonitorID = MonitorID 0x01020304
                , callOriginatedEventPeripheralID = PeripheralID (CtiOpt (Just 0x02030405))
                , callOriginatedEventPeripheralType = SafeEnum (Right (CtiOpt (Just PeripheralTypeEnterpriseAgent)))
                , callOriginatedEventConnectionDeviceIDType = SafeEnum (Right (CtiOpt (Just ConnectionDevIDDynamic)))
                , callOriginatedEventConnectionCallID = ConnectionCallID (CtiOpt (Just 0x03040506))
                , callOriginatedEventLineHandle = LineHandle (CtiOpt (Just 0x0405))
                , callOriginatedEventLineType = SafeEnum (Right (CtiOpt (Just LineTypeSupervisor)))
                , callOriginatedEventServiceNumber = ServiceNumber (CtiOpt (Just 0x05060708))
                , callOriginatedEventServiceID = ServiceID (CtiOpt (Just 0x06070809))
                , callOriginatedEventSkillGroupNumber = SkillGroupNumber (CtiOpt (Just 0x0708090a))
                , callOriginatedEventSkillGroupID = SkillGroupID (CtiOpt (Just 0x08090a0b))
                , callOriginatedEventSkillGroupPriority = SkillGroupPriority (CtiOpt0 (Just 0x090a))
                , callOriginatedEventCallingDeviceType = SafeEnum (Right (CtiOpt (Just DevIDExternal)))
                , callOriginatedEventCalledDeviceType = SafeEnum (Right (CtiOpt (Just DevIDRoutePoint)))
                , callOriginatedEventLocalConnectionState = SafeEnum (Right (CtiOpt (Just LocalConnectionStateConnect)))
                , callOriginatedEventEventCause = SafeEnum (Right (CtiOpt (Just EventCauseNewCall)))
                }
          float = FloatPart
                [ ConnectionDevID "ConnectionDeviceID"
                , CallingDevID "CallingDeviceID"
                , CalledDevID "CalledDeviceID"
                ]
          raw = [ 0,0,0,15, 1,2,3,4, 2,3,4,5, 0,17, 0,1, 3,4,5,6, 4,5, 0,4, 5,6,7,8, 6,7,8,9
                , 7,8,9,10, 8,9,10,11, 9,10, 0,75, 0,74, 0,3, 0,22
                , 0,25,0,19,  0x43,0x6f,0x6e,0x6e,0x65,0x63,0x74,0x69,0x6f,0x6e,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,27,0,16,  0x43,0x61,0x6c,0x6c,0x69,0x6e,0x67,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,28,0,15,  0x43,0x61,0x6c,0x6c,0x65,0x64,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                ]
      in MessageSample name raw (Message fixed float)

    , let name = "CALL_FAILED_EVENT"
          fixed = CallFailedEvent CallFailedEventR
                { callFailedEventMonitorID = MonitorID 0x01020304
                , callFailedEventPeripheralID = PeripheralID (CtiOpt (Just 0x02030405))
                , callFailedEventPeripheralType = SafeEnum (Right (CtiOpt (Just PeripheralTypeEnterpriseAgent)))
                , callFailedEventConnectionDeviceIDType = SafeEnum (Right (CtiOpt (Just ConnectionDevIDDynamic)))
                , callFailedEventConnectionCallID = ConnectionCallID (CtiOpt (Just 0x03040506))
                , callFailedEventFailingDeviceType = SafeEnum (Right (CtiOpt (Just DevIDExternal)))
                , callFailedEventCalledDeviceType = SafeEnum (Right (CtiOpt (Just DevIDRoutePoint)))
                , callFailedEventLocalConnectionState = SafeEnum (Right (CtiOpt (Just LocalConnectionStateInitiate)))
                , callFailedEventEventCause = SafeEnum (Right (CtiOpt (Just EventCauseNewCall)))
                }
          float = FloatPart
                [ ConnectionDevID "ConnectionDeviceID"
                , FailingDevID "FailingDeviceID"
                , CalledDevID "CalledDeviceID"
                ]
          raw = [ 0,0,0,16, 1,2,3,4, 2,3,4,5, 0,17, 0,1, 3,4,5,6, 0,75, 0,74, 0,1, 0,22
                , 0,25,0,19,  0x43,0x6f,0x6e,0x6e,0x65,0x63,0x74,0x69,0x6f,0x6e,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,34,0,16,  0x46,0x61,0x69,0x6c,0x69,0x6e,0x67,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,28,0,15,  0x43,0x61,0x6c,0x6c,0x65,0x64,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                ]
      in MessageSample name raw (Message fixed float)

    , let name = "CALL_CONFERENCED_EVENT"
          fixed = CallConferencedEvent CallConferencedEventR
                { callConferencedEventMonitorID = MonitorID 0x01020304
                , callConferencedEventPeripheralID = PeripheralID (CtiOpt (Just 0x02030405))
                , callConferencedEventPeripheralType = SafeEnum (Right (CtiOpt (Just PeripheralTypeEnterpriseAgent)))
                , callConferencedEventPrimaryDeviceIDType = SafeEnum (Right (CtiOpt (Just ConnectionDevIDDynamic)))
                , callConferencedEventPrimaryCallID = ConnectionCallID (CtiOpt (Just 0x06070809))
                , callConferencedEventLineHandle = LineHandle (CtiOpt (Just 0x0304))
                , callConferencedEventLineType = SafeEnum (Right (CtiOpt (Just LineTypeSupervisor)))
                , callConferencedEventSkillGroupNumber = SkillGroupNumber (CtiOpt (Just 0x04050607))
                , callConferencedEventSkillGroupID = SkillGroupID (CtiOpt (Just 0x05060708))
                , callConferencedEventSkillGroupPriority = SkillGroupPriority (CtiOpt0 (Just 0x0607))
                , callConferencedEventNumParties = 0x0708
                , callConferencedEventSecondaryDeviceIDType = SafeEnum (Right (CtiOpt (Just ConnectionDevIDStatic)))
                , callConferencedEventSecondaryCallID = ConnectionCallID (CtiOpt (Just 0x0708090a))
                , callConferencedEventControllerDeviceType = SafeEnum (Right (CtiOpt (Just DevIDAgentDevice)))
                , callConferencedEventAddedPartyDeviceType = SafeEnum (Right (CtiOpt (Just DevIDTrunk)))
                , callConferencedEventLocalConnectionState = SafeEnum (Right (CtiOpt (Just LocalConnectionStateConnect)))
                , callConferencedEventEventCause = SafeEnum (Right (CtiOpt (Just EventCausePark)))
                }
          float = FloatPart
                [ PrimaryDevID "PrimaryDeviceID"
                , SecondaryDevID "SecondaryDeviceID"
                , ControllerDevID "ControllerDeviceID"
                , AddedPartyDevID "AddedPartyDeviceID"
                , ConnectedPartyCallID (CallID 0x08090a0b)
                , ConnectedPartyDevIDType (SafeEnum (Right (CtiOpt (Just DevIDExternal))))
                , ConnectedPartyDevID "ConnectedPartyDeviceID"
                ]
          raw = [ 0,0,0,17, 1,2,3,4, 2,3,4,5, 0,17, 0,1, 6,7,8,9, 3,4, 0,4, 4,5,6,7, 5,6,7,8, 6,7
                , 7,8, 0,0, 7,8,9,10, 0,76, 0,70, 0,3, 0,25
                , 0,35,0,16,  0x50,0x72,0x69,0x6d,0x61,0x72,0x79,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,36,0,18,  0x53,0x65,0x63,0x6f,0x6e,0x64,0x61,0x72,0x79,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,37,0,19,  0x43,0x6f,0x6e,0x74,0x72,0x6f,0x6c,0x6c,0x65,0x72,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,38,0,19,  0x41,0x64,0x64,0x65,0x64,0x50,0x61,0x72,0x74,0x79,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,39,0,4,   8,9,10,11
                , 0,40,0,2,   0,75
                , 0,41,0,23,  0x43,0x6f,0x6e,0x6e,0x65,0x63,0x74,0x65,0x64,0x50,0x61,0x72,0x74,0x79,0x44,0x65,0x76,0x69,0x63,0x65
                ,             0x49,0x44,0
                ]
      in MessageSample name raw (Message fixed float)

    , let name = "CALL_TRANSFERRED_EVENT"
          fixed = CallTransferredEvent CallTransferredEventR
                { callTransferredEventMonitorID = MonitorID 0x01020304
                , callTransferredEventPeripheralID = PeripheralID (CtiOpt (Just 0x02030405))
                , callTransferredEventPeripheralType = SafeEnum (Right (CtiOpt (Just PeripheralTypeEnterpriseAgent)))
                , callTransferredEventPrimaryDeviceIDType = SafeEnum (Right (CtiOpt (Just ConnectionDevIDDynamic)))
                , callTransferredEventPrimaryCallID = ConnectionCallID (CtiOpt (Just 0x06070809))
                , callTransferredEventLineHandle = LineHandle (CtiOpt (Just 0x0304))
                , callTransferredEventLineType = SafeEnum (Right (CtiOpt (Just LineTypeSupervisor)))
                , callTransferredEventSkillGroupNumber = SkillGroupNumber (CtiOpt (Just 0x04050607))
                , callTransferredEventSkillGroupID = SkillGroupID (CtiOpt (Just 0x05060708))
                , callTransferredEventSkillGroupPriority = SkillGroupPriority (CtiOpt0 (Just 0x0607))
                , callTransferredEventNumParties = 0x0708
                , callTransferredEventSecondaryDeviceIDType = SafeEnum (Right (CtiOpt (Just ConnectionDevIDStatic)))
                , callTransferredEventSecondaryCallID = ConnectionCallID (CtiOpt (Just 0x0708090a))
                , callTransferredEventTransferringDeviceType = SafeEnum (Right (CtiOpt (Just DevIDAgentDevice)))
                , callTransferredEventTransferredDeviceType = SafeEnum (Right (CtiOpt (Just DevIDTrunk)))
                , callTransferredEventLocalConnectionState = SafeEnum (Right (CtiOpt (Just LocalConnectionStateConnect)))
                , callTransferredEventEventCause = SafeEnum (Right (CtiOpt (Just EventCausePark)))
                }
          float = FloatPart
                [ PrimaryDevID "PrimaryDeviceID"
                , SecondaryDevID "SecondaryDeviceID"
                , TransferringDevID "TransferringDeviceID"
                , TransferredDevID "TransferredDeviceID"
                , ConnectedPartyCallID (CallID 0x08090a0b)
                , ConnectedPartyDevIDType (SafeEnum (Right (CtiOpt (Just DevIDExternal))))
                , ConnectedPartyDevID "ConnectedPartyDeviceID"
                ]
          raw = [ 0,0,0,18, 1,2,3,4, 2,3,4,5, 0,17, 0,1, 6,7,8,9, 3,4, 0,4, 4,5,6,7, 5,6,7,8, 6,7
                , 7,8, 0,0, 7,8,9,10, 0,76, 0,70, 0,3, 0,25
                , 0,35,0,16,  0x50,0x72,0x69,0x6d,0x61,0x72,0x79,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,36,0,18,  0x53,0x65,0x63,0x6f,0x6e,0x64,0x61,0x72,0x79,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,42,0,21,  0x54,0x72,0x61,0x6e,0x73,0x66,0x65,0x72,0x72,0x69,0x6e,0x67,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,43,0,20,  0x54,0x72,0x61,0x6e,0x73,0x66,0x65,0x72,0x72,0x65,0x64,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,39,0,4,   8,9,10,11
                , 0,40,0,2,   0,75
                , 0,41,0,23,  0x43,0x6f,0x6e,0x6e,0x65,0x63,0x74,0x65,0x64,0x50,0x61,0x72,0x74,0x79,0x44,0x65,0x76,0x69,0x63,0x65,
                              0x49,0x44,0
                ]
      in MessageSample name raw (Message fixed float)

    , let name = "CALL_DIVERTED_EVENT"
          fixed = CallDivertedEvent CallDivertedEventR
                { callDivertedEventMonitorID = MonitorID 0x01020304
                , callDivertedEventPeripheralID = PeripheralID (CtiOpt (Just 0x02030405))
                , callDivertedEventPeripheralType = SafeEnum (Right (CtiOpt (Just PeripheralTypeEnterpriseAgent)))
                , callDivertedEventConnectionDeviceIDType = SafeEnum (Right (CtiOpt (Just ConnectionDevIDStatic)))
                , callDivertedEventConnectionCallID = ConnectionCallID (CtiOpt (Just 0x06070809))
                , callDivertedEventServiceNumber = ServiceNumber (CtiOpt (Just 0x04050607))
                , callDivertedEventServiceID = ServiceID (CtiOpt (Just 0x05060708))
                , callDivertedEventDivertingDeviceType = SafeEnum (Right (CtiOpt (Just DevIDQueue)))
                , callDivertedEventCalledDeviceType = SafeEnum (Right (CtiOpt (Just DevIDQueue)))
                , callDivertedEventLocalConnectionState = SafeEnum (Right (CtiOpt (Just LocalConnectionStateNull)))
                , callDivertedEventEventCause = SafeEnum (Right (CtiOpt Nothing))
                }
          float = FloatPart
                [ ConnectionDevID "ConnectionDeviceID"
                , DivertingDevID "DivertingDeviceID"
                , CalledDevID "CalledDeviceID"
                ]
          raw = [ 0,0,0,19, 1,2,3,4, 2,3,4,5, 0,17, 0,0, 6,7,8,9
                , 4,5,6,7, 5,6,7,8, 0,77, 0,77, 0,0, 0xff,0xff
                , 0,25,0,19,  0x43,0x6f,0x6e,0x6e,0x65,0x63,0x74,0x69,0x6f,0x6e,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,44,0,18,  0x44,0x69,0x76,0x65,0x72,0x74,0x69,0x6e,0x67,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,28,0,15,  0x43,0x61,0x6c,0x6c,0x65,0x64,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                ]
      in MessageSample name raw (Message fixed float)

    , let name = "CALL_SERVICE_INITIATED_EVENT"
          fixed = CallServiceInitiatedEvent CallServiceInitiatedEventR
                { callServiceInitiatedEventMonitorID = MonitorID 0x01020304
                , callServiceInitiatedEventPeripheralID = PeripheralID (CtiOpt (Just 0x02030405))
                , callServiceInitiatedEventPeripheralType = SafeEnum (Right (CtiOpt (Just PeripheralTypeEnterpriseAgent)))
                , callServiceInitiatedEventConnectionDeviceIDType = SafeEnum (Right (CtiOpt (Just ConnectionDevIDDynamic)))
                , callServiceInitiatedEventConnectionCallID = ConnectionCallID (CtiOpt (Just 0x03040506))
                , callServiceInitiatedEventLineHandle = LineHandle (CtiOpt (Just 0x0405))
                , callServiceInitiatedEventLineType = SafeEnum (Right (CtiOpt (Just LineTypeSupervisor)))
                , callServiceInitiatedEventServiceNumber = ServiceNumber (CtiOpt (Just 0x05060708))
                , callServiceInitiatedEventServiceID = ServiceID (CtiOpt (Just 0x06070809))
                , callServiceInitiatedEventSkillGroupNumber = SkillGroupNumber (CtiOpt (Just 0x0708090a))
                , callServiceInitiatedEventSkillGroupID = SkillGroupID (CtiOpt (Just 0x08090a0b))
                , callServiceInitiatedEventSkillGroupPriority = SkillGroupPriority (CtiOpt0 (Just 0x090a))
                , callServiceInitiatedEventCallingDeviceType = SafeEnum (Right (CtiOpt (Just DevIDExternal)))
                , callServiceInitiatedEventLocalConnectionState = SafeEnum (Right (CtiOpt (Just LocalConnectionStateConnect)))
                , callServiceInitiatedEventEventCause = SafeEnum (Right (CtiOpt (Just EventCauseNewCall)))
                }
          float = FloatPart
                [ ConnectionDevID "ConnectionDeviceID"
                , CallingDevID "CallingDeviceID"
                ]
          raw = [ 0,0,0,20, 1,2,3,4, 2,3,4,5, 0,17, 0,1, 3,4,5,6, 4,5, 0,4, 5,6,7,8, 6,7,8,9
                , 7,8,9,10, 8,9,10,11, 9,10, 0,75, 0,3, 0,22
                , 0,25,0,19,  0x43,0x6f,0x6e,0x6e,0x65,0x63,0x74,0x69,0x6f,0x6e,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,27,0,16,  0x43,0x61,0x6c,0x6c,0x69,0x6e,0x67,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                ]
      in MessageSample name raw (Message fixed float)

    , let name = "CALL_QUEUED_EVENT"
          fixed = CallQueuedEvent CallQueuedEventR
                { callQueuedEventMonitorID = MonitorID 0x01020304
                , callQueuedEventPeripheralID = PeripheralID (CtiOpt (Just 0x02030405))
                , callQueuedEventPeripheralType = SafeEnum (Right (CtiOpt (Just PeripheralTypeEnterpriseAgent)))
                , callQueuedEventConnectionDeviceIDType = SafeEnum (Right (CtiOpt (Just ConnectionDevIDStatic)))
                , callQueuedEventConnectionCallID = ConnectionCallID (CtiOpt (Just 0x06070809))
                , callQueuedEventServiceNumber = ServiceNumber (CtiOpt (Just 0x04050607))
                , callQueuedEventServiceID = ServiceID (CtiOpt (Just 0x05060708))
                , callQueuedEventQueueDeviceType = SafeEnum (Right (CtiOpt (Just DevIDQueue)))
                , callQueuedEventCallingDeviceType = SafeEnum (Right (CtiOpt (Just DevIDExternal)))
                , callQueuedEventCalledDeviceType = SafeEnum (Right (CtiOpt (Just DevIDRoutePoint)))
                , callQueuedEventLastRedirectDeviceType = SafeEnum (Right (CtiOpt (Just DevIDCtiPort)))
                , callQueuedEventNumQueued = 0x0607
                , callQueuedEventNumSkillGroups = 0x0708
                , callQueuedLocalConnectionState = SafeEnum (Right (CtiOpt (Just LocalConnectionStateNull)))
                , callQueuedEventEventCause = SafeEnum (Right (CtiOpt Nothing))
                }
          float = FloatPart
                [ ConnectionDevID "ConnectionDeviceID"
                , QueueDevID "QueueDeviceID"
                , CallingDevID "CallingDeviceID"
                , CalledDevID "CalledDeviceID"
                , LastRedirectDevID "LastRedirectDeviceID"
                , SkillGroupNumberF (SkillGroupNumber (CtiOpt (Just 0x08090a0b)))
                , SkillGroupIDF (SkillGroupID (CtiOpt (Just 0x090a0b0c)))
                , SkillGroupPriorityF (SkillGroupPriority (CtiOpt0 (Just 0x0a0b)))
                ]
          raw = [ 0,0,0,21, 1,2,3,4, 2,3,4,5, 0,17, 0,0, 6,7,8,9
                , 4,5,6,7, 5,6,7,8, 0,77, 0,75, 0,74, 0,73, 6,7, 7,8, 0,0, 0xff,0xff
                , 0,25,0,19,  0x43,0x6f,0x6e,0x6e,0x65,0x63,0x74,0x69,0x6f,0x6e,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,45,0,14,  0x51,0x75,0x65,0x75,0x65,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,27,0,16,  0x43,0x61,0x6c,0x6c,0x69,0x6e,0x67,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,28,0,15,  0x43,0x61,0x6c,0x6c,0x65,0x64,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,29,0,21,  0x4c,0x61,0x73,0x74,0x52,0x65,0x64,0x69,0x72,0x65,0x63,0x74,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,62,0,4,   8,9,10,11
                , 0,63,0,4,   9,10,11,12
                , 0,64,0,2,10,11
                ]
      in MessageSample name raw (Message fixed float)

    , let name = "CALL_TRANSLATION_ROUTE_EVENT"
          fixed = CallTranslationRouteEvent CallTranslationRouteEventR
                { callTranslationRoutenumNamedVariables = 0x0405
                , callTranslationRoutenumNamedArrays = 0x0506
                }
          float = FloatPart
                [ Ani "09012345678"
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
                , NamedVariable (NamedVar "ECCVar" "ECCVal")
                , NamedArray (NamedArr 1 "ECCArr" "ECCArrVal")
                ]
          raw = [ 0,0,0,22, 4,5, 5,6
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
                , 0,82,0,14,  0x45,0x43,0x43,0x56,0x61,0x72,0, 0x45,0x43,0x43,0x56,0x61,0x6c,0
                , 0,83,0,18,  1, 0x45,0x43,0x43,0x41,0x72,0x72,0, 0x45,0x43,0x43,0x41,0x72,0x72,0x56,0x61,0x6c,0
                ]
      in MessageSample name raw (Message fixed float)

    , let name = "BEGIN_CALL_EVENT"
          fixed = BeginCallEvent BeginCallEventR
                { beginCallEventMonitorID = MonitorID 0x01020304
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
          raw = [ 0,0,0,23, 1,2,3,4, 2,3,4,5, 0,17, 3,4, 4,5, 5,6, 0,2, 0,0, 6,7,8,9, 0,13
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
      in MessageSample name raw (Message fixed float)

    , let name = "END_CALL_EVENT"
          fixed = EndCallEvent EndCallEventR
                { endCallEventMonitorID = MonitorID 0x01020304
                , endCallEventPeripheralID = PeripheralID (CtiOpt (Just 0x02030405))
                , endCallEventPeripheralType = SafeEnum (Right (CtiOpt (Just PeripheralTypeEnterpriseAgent)))
                , endCallEventConnectionDeviceIDType = SafeEnum (Right (CtiOpt (Just ConnectionDevIDStatic)))
                , endCallEventConnectionCallID = ConnectionCallID (CtiOpt (Just 0x06070809))
                }
          float = FloatPart [ ConnectionDevID "ConnectionDeviceID" ]
          raw = [ 0,0,0,24, 1,2,3,4, 2,3,4,5, 0,17, 0,0, 6,7,8,9
                , 0,25,0,19, 0x43,0x6f,0x6e,0x6e,0x65,0x63,0x74,0x69,0x6f,0x6e,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                ]
      in MessageSample name raw (Message fixed float)

    , let name = "CALL_DATA_UPDATE_EVENT"
          fixed = CallDataUpdateEvent CallDataUpdateEventR
                { callDataUpdateEventMonitorID = MonitorID 0x01020304
                , callDataUpdateEventPeripheralID = PeripheralID (CtiOpt (Just 0x02030405))
                , callDataUpdateEventPeripheralType = SafeEnum (Right (CtiOpt (Just PeripheralTypeEnterpriseAgent)))
                , callDataUpdateEventNumCtiClients = 0x0304
                , callDataUpdateEventNumNamedVariables = 0x0405
                , callDataUpdateEventNumNamedArrays = 0x0506
                , callDataUpdateEventCallType = SafeEnum (Right (CtiOpt (Just CallTypePreRouteAcdIn)))
                , callDataUpdateEventConnectionDeviceIDType = SafeEnum (Right (CtiOpt (Just ConnectionDevIDStatic)))
                , callDataUpdateEventConnectionCallID = ConnectionCallID (CtiOpt (Just 0x06070809))
                , callDataUpdateEventNewConnectionDeviceIDType = SafeEnum (Right (CtiOpt (Just ConnectionDevIDStatic)))
                , callDataUpdateEventNewConnectionCallID = ConnectionCallID (CtiOpt (Just 0x02030405))
                , callDataUpdateEventCalledPartyDisposition = SafeEnum (Right DispositionDisconnectDropHandledPrimaryRoute)
                , callDataUpdateEventCampaignID = CampaignID (CtiOpt0 (Just 0x04050607))
                , callDataUpdateEventQueryRuleID = QueryRuleID (CtiOpt0 (Just 0x05060708))
                }
          float = FloatPart
                [ ConnectionDevID "ConnectionDeviceID"
                , NewConnectionDevID "NewConnectionDeviceID"
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
                , CustomerPhoneNumber "08012345678"
                , CustomerAccountNumber "123123123"
                , CtiClientSignature "CtiClientSignature"
                , CtiClientTimestamp (Timestamp 0x090a0b0c)
                , CallReferenceID (fromWord8List [9,8,7,6,5,4,3,2,1,0])
                ]
          raw = [ 0,0,0,25, 1,2,3,4, 2,3,4,5, 0,17, 3,4, 4,5, 5,6, 0,2, 0,0, 6,7,8,9
                , 0,0, 2,3,4,5, 0,13, 4,5,6,7, 5,6,7,8
                , 0,25,0,19,  0x43,0x6f,0x6e,0x6e,0x65,0x63,0x74,0x69,0x6f,0x6e,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,47,0,22,  0x4e,0x65,0x77,0x43,0x6f,0x6e,0x6e,0x65,0x63,0x74,0x69,0x6f,0x6e,0x44,0x65,0x76,0x69,0x63,
                              0x65,0x49,0x44,0
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
                , 0,95,0,12,  0x30,0x38,0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0
                , 0,96,0,10,  0x31,0x32,0x33,0x31,0x32,0x33,0x31,0x32,0x33,0
                , 0,23,0,19,  0x43,0x74,0x69,0x43,0x6c,0x69,0x65,0x6e,0x74,0x53,0x69,0x67,0x6e,0x61,0x74,0x75,0x72,0x65,0
                , 0,24,0,4,   9,10,11,12
                , 0,223,0,10, 9,8,7,6,5,4,3,2,1,0
                ]
      in MessageSample name raw (Message fixed float)

    , let name = "SET_CALL_DATA_REQ"
          fixed = SetCallDataReq SetCallDataReqR
                { setCallDataReqInvokeID = InvokeID 0x07080809
                , setCallDataReqPeripheralID = PeripheralID (CtiOpt (Just 0x09080706))
                , setCallDataReqConnectionDeviceIDType = SafeEnum (Right (CtiOpt (Just ConnectionDevIDStatic)))
                , setCallDataReqConnectionCallID = ConnectionCallID (CtiOpt (Just 0x08070605))
                , setCallDataReqNumNamedVariables = 0x0706
                , setCallDataReqNumNamedArrays = 0x0605
                , setCallDataReqCallType = SafeEnum (Right (CtiOpt (Just CallTypePreRouteAcdIn)))
                , setCallDataReqCalledPartyDisposition = SafeEnum (Right DispositionDisconnectDropHandledPrimaryRoute)
                , setCallDataReqCampaignID = CampaignID (CtiOpt0 (Just 0x05040302))
                , setCallDataReqQueryRuleID = QueryRuleID (CtiOpt0 (Just 0x04030201))
                }
          float = FloatPart
                [ ConnectionDevID "ConnectionDeviceID"
                , Ani "09012345678"
                , UserToUserInfo (fromWord8List [0,1,2,3,4,5,6,7,8,9])
                , CallerEnteredDigit "321"
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
                , CustomerPhoneNumber "CustPhoneNum"
                , CustomerAccountNumber "CustAcctNum"
                , RouterCallKeyDay 0x01020304
                , RouterCallkeyCallID 0x02030405
                , RouterCallKeySequenceNum 0x03040506
                , CallOriginatedFrom 0x44
                ]
          raw = [ 0,0,0,26, 7,8,8,9, 9,8,7,6, 0,0, 8,7,6,5, 7,6, 6,5, 0,2, 0,13, 5,4,3,2, 4,3,2,1
                , 0,25,0,19,  0x43,0x6f,0x6e,0x6e,0x65,0x63,0x74,0x69,0x6f,0x6e,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,8,0,12,   0x30, 0x39, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38,0
                , 0,9,0,10,   0,1,2,3,4,5,6,7,8,9
                , 0,12,0,4,   0x33, 0x32, 0x31,0
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
                , 0,95,0,13,  0x43,0x75,0x73,0x74,0x50,0x68,0x6f,0x6e,0x65,0x4e,0x75,0x6d,0
                , 0,96,0,12,  0x43,0x75,0x73,0x74,0x41,0x63,0x63,0x74,0x4e,0x75,0x6d,0
                , 0,72,0,4,   1,2,3,4
                , 0,73,0,4,   2,3,4,5
                , 0,110,0,4,  3,4,5,6
                , 0,232,0,1,  0x44
                ]
      in MessageSample name raw (Message fixed float)

    , let name = "SET_CALL_DATA_CONF"
          fixed = SetCallDataConf $ SetCallDataConfR (InvokeID 0x07080809)
          raw = [ 0,0,0,27, 7,8,8,9 ]
      in MessageSample name raw (Message fixed (FloatPart []))

    , let name = "RELEASE_CALL_REQ"
          fixed = ReleaseCallReq ReleaseCallReqR
                { releaseCallReqInvokeID = InvokeID 0x0809090a
                , releaseCallReqPeripheralID = PeripheralID (CtiOpt (Just 0x090a0a0b))
                , releaseCallReqConnectionDeviceIDType = SafeEnum (Right (CtiOpt (Just ConnectionDevIDStatic)))
                , releaseCallReqConnectionCallID = ConnectionCallID (CtiOpt (Just 0x0a0b0b0c))
                }
          float = FloatPart [ ConnectionDevID "ConnectionDeviceID" ]
          raw = [ 0,0,0,28, 8,9,9,10, 9,10,10,11, 0,0, 10,11,11,12
                , 0,25,0,19,  0x43,0x6f,0x6e,0x6e,0x65,0x63,0x74,0x69,0x6f,0x6e,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                ]
      in MessageSample name raw (Message fixed float)

    , let name = "RELEASE_CALL_CONF"
          fixed = ReleaseCallConf $ ReleaseCallConfR (InvokeID 0x05060708)
          raw = [ 0,0,0,29, 5,6,7,8 ]
      in MessageSample name raw (Message fixed (FloatPart []))

    , let name = "AGENT_STATE_EVENT"
          fixed = AgentStateEvent AgentStateEventR
                { agentStateEventMonitorID = MonitorID 0x01020304
                , agentStateEventPeripheralID = PeripheralID (CtiOpt (Just 0x02030405))
                , agentStateEventSessionID = SessionID (CtiOpt0 (Just 0x03040506))
                , agentStateEventPeripheralType = SafeEnum (Right (CtiOpt (Just PeripheralTypeEnterpriseAgent)))
                , agentStateEventSkillGroupState = SafeEnum (Right AgentStateBusyOther)
                , agentStateEventStateDuration = 0x04050607
                , agentStateEventSkillGroupNumber = SkillGroupNumber (CtiOpt (Just 0x06070809))
                , agentStateEventSkillGroupID = SkillGroupID (CtiOpt (Just 0x0708090a))
                , agentStateEventSkillGroupPriority = SkillGroupPriority (CtiOpt0 (Just 0x0809))
                , agentStateEventAgentState = SafeEnum (Right AgentStateHold)
                , agentStateEventEventReasonCode = EventReasonCode 0x090a
                , agentStateEventMrdID = MrdID 0x090a0b0c
                , agentStateEventNumTasks = 0x0a0b0c0d
                , agentStateEventAgentMode = SafeEnum (Right AgentModeNotRoutable)
                , agentStateEventMaxTaskLimit = 0x09080706
                , agentStateEventIcmAgentID = IcmAgentID 0x08070605
                , agentStateEventAgentAvailabilityStatus = SafeEnum (Right AgentAvailabilityStatusIcmAvailable)
                , agentStateEventNumFltSkillGroups = 0x0706
                , agentStateEventDepartmentID = DepartmentID 0x0b0c0d0e
                }
          float = FloatPart
                [ ClientSignature "ClientSignature"
                , AgentID "1001"
                , AgentExtension "3001"
                , AgentInstrument "3001"
                , Duration 0x06050403
                , NextAgentState (SafeEnum (Right AgentStateTalking))
                , Direction (SafeEnum (Right DirectionIn))
                , SkillGroupNumberF (SkillGroupNumber (CtiOpt (Just 0x0708090a)))
                , SkillGroupIDF (SkillGroupID (CtiOpt (Just 0x08090a0b)))
                , SkillGroupPriorityF (SkillGroupPriority (CtiOpt0 (Just 0x090a)))
                , SkillGroupState (SafeEnum (Right AgentStateBusyOther))
                ]
          raw = [ 0,0,0,30, 1,2,3,4, 2,3,4,5, 3,4,5,6, 0,17, 0,7, 4,5,6,7, 6,7,8,9, 7,8,9,10
                , 8,9, 0,10, 9,10, 9,10,11,12, 10,11,12,13, 0,0, 9,8,7,6, 8,7,6,5, 0,0,0,1, 7,6, 11,12,13,14
                , 0,3,0,16,  0x43,0x6c,0x69,0x65,0x6e,0x74,0x53,0x69,0x67,0x6e,0x61,0x74,0x75,0x72,0x65,0
                , 0,5,0,5,   0x31,0x30,0x30,0x31,0
                , 0,4,0,5,   0x33,0x30,0x30,0x31,0
                , 0,6,0,5,   0x33,0x30,0x30,0x31,0
                , 0,150,0,4, 6,5,4,3
                , 0,123,0,2, 0,4
                , 0,244,0,4, 0,0,0,1
                , 0,62,0,4,  7,8,9,10
                , 0,63,0,4,  8,9,10,11
                , 0,64,0,2,  9,10
                , 0,65,0,2,  0,7
                ]
      in MessageSample name raw (Message fixed float)

    , let name = "SYSTEM_EVENT"
          fixed = SystemEvent SystemEventR
                { systemEventPGStatus = fromList [PgsOpcDown, PgsCCDown]
                , systemEventIcmCentralControllerTime = Timestamp 0x01020304
                , systemEventSystemEventID = SafeEnum (Right SysTextFyi)
                , systemEventSystemEventArg1 = SystemEventArg1 0x02030405
                , systemEventSystemEventArg2 = SystemEventArg2 0x03040506
                , systemEventSystemEventArg3 = SystemEventArg3 0x04050607
                , systemEventEventDeviceType = SafeEnum (Right (CtiOpt (Just DevIDTrunk)))
                }
          float = FloatPart
                [ FloatFieldText "SystemEventText"
                , EventDeviceID "EventDeviceID"
                ]
          raw = [ 0,0,0,31, 0,0,0,3, 1,2,3,4, 0,0,0,5, 2,3,4,5, 3,4,5,6, 4,5,6,7, 0,70
                , 0,7,0,16,    0x53,0x79,0x73,0x74,0x65,0x6d,0x45,0x76,0x65,0x6e,0x74,0x54,0x65,0x78,0x74,0
                , 0,206,0,14,  0x45,0x76,0x65,0x6e,0x74,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                ]
      in MessageSample name raw (Message fixed float)

    , let name = "CLIENT_EVENT_REPORT_REQ"
          fixed = ClientEventReportReq ClientEventReportReqR
                { clientEventReportReqInvokeID = InvokeID 0x03040505
                , clientEventReportReqState = SafeEnum (Right ClientEventReportReqStateWarning)
                }
          float = FloatPart
                [ ObjectName "ObjectName"
                , FloatFieldText "Text"
                ]
          raw = [ 0,0,0,32, 3,4,5,5, 0,1
                , 0,66,0,11,  0x4f,0x62,0x6a,0x65,0x63,0x74,0x4e,0x61,0x6d,0x65,0
                , 0,7,0,5,    0x54,0x65,0x78,0x74,0
                ]
      in MessageSample name raw (Message fixed float)

    , let name = "CLIENT_EVENT_REPORT_CONF"
          fixed = ClientEventReportConf $ ClientEventReportConfR (InvokeID 0x03040505)
          raw = [ 0,0,0,33, 3,4,5,5 ]
      in MessageSample name raw (Message fixed (FloatPart []))

    , let name = "CALL_REACHED_NETWORK_EVENT"
          fixed = CallReachedNetworkEvent CallReachedNetworkEventR
                { callReachedNetworkEventMonitorID = MonitorID 0x01020304
                , callReachedNetworkEventPeripheralID = PeripheralID (CtiOpt (Just 0x02030405))
                , callReachedNetworkEventPeripheralType = SafeEnum (Right (CtiOpt (Just PeripheralTypeEnterpriseAgent)))
                , callReachedNetworkEventConnectionDeviceIDType = SafeEnum (Right (CtiOpt (Just ConnectionDevIDDynamic)))
                , callReachedNetworkEventConnectionCallID = ConnectionCallID (CtiOpt (Just 0x03040506))
                , callReachedNetworkEventLineHandle = LineHandle (CtiOpt (Just 0x0405))
                , callReachedNetworkEventLineType = SafeEnum (Right (CtiOpt (Just LineTypeSupervisor)))
                , callReachedNetworkEventTrunkUsedDeviceType = SafeEnum (Right (CtiOpt (Just DevIDTrunk)))
                , callReachedNetworkEventCalledDeviceType = SafeEnum (Right (CtiOpt (Just DevIDRoutePoint)))
                , callReachedNetworkEventLocalConnectionState = SafeEnum (Right (CtiOpt (Just LocalConnectionStateConnect)))
                , callReachedNetworkEventEventCause = SafeEnum (Right (CtiOpt (Just EventCauseNewCall)))
                }
          float = FloatPart
                [ ConnectionDevID "ConnectionDeviceID"
                , TrunkUsedDevID "TrunkUsedDeviceID"
                , CalledDevID "CalledDeviceID"
                , TrunkNumber 0x04050607
                , TrunkGroupNumber 0x05060708
                ]
          raw = [ 0,0,0,34, 1,2,3,4, 2,3,4,5, 0,17, 0,1, 3,4,5,6, 4,5, 0,4
                , 0,70, 0,74, 0,3, 0,22
                , 0,25,0,19,  0x43,0x6f,0x6e,0x6e,0x65,0x63,0x74,0x69,0x6f,0x6e,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,48,0,18,  0x54,0x72,0x75,0x6e,0x6b,0x55,0x73,0x65,0x64,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,28,0,15,  0x43,0x61,0x6c,0x6c,0x65,0x64,0x44,0x65,0x76,0x69,0x63,0x65,0x49,0x44,0
                , 0,121,0,4,  4,5,6,7
                , 0,122,0,4,  5,6,7,8
                ]
      in MessageSample name raw (Message fixed float)

    , let name = "CONTROL_FAILURE_CONF"
          fixed = ControlFailureConf ControlFailureConfR
                { controlFailureConfInvokeID = InvokeID 0x01020304
                , controlFailureConfFailureCode = SafeEnum (Right CFRequestTimeoutRejection)
                , controlFailureConfPeripheralErrorCode = PeripheralErrorCode 0x02030405
                }
          raw = [ 0,0,0,35, 1,2,3,4, 0,78, 2,3,4,5 ]
      in MessageSample name raw (Message fixed (FloatPart []))




    ]

