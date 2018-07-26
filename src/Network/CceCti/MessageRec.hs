{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Network.CceCti.MessageRec where

import           Data.Binary            (Binary)
import           Data.Word              (Word16, Word32)
import           GHC.Generics

import           Network.CceCti.BitMask
import           Network.CceCti.Enum
import           Network.CceCti.Types


data FailureConfR = FailureConfR            -- 1
    { failureConfInvokeID :: InvokeID
    , failureConfStatus   :: SafeEnum FailureIndicationMessageStatusCode
    } deriving (Binary, Eq, Generic, Show)

data FailureEventR = FailureEventR          -- 2
    { failureEventStatus :: SafeEnum FailureIndicationMessageStatusCode
    } deriving (Binary, Eq, Generic, Show)

data OpenReqR = OpenReqR                    -- 3
    { openReqInvokeID         :: InvokeID
    , openReqVersionNumber    :: VersionNumber
    , openReqIdleTimeout      :: CtiOpt Word32
    , openReqPeripheralID     :: PeripheralID
    , openReqserviceRequested :: BitSet (Holder CtiServiceMask) CtiServiceMask
    , openReqcallMsgMask      :: BitSet (Holder CallEventMessageMask) CallEventMessageMask
    , openReqagentStateMask   :: BitSet (Holder AgentStateMask) AgentStateMask
    , openReqconfigMsgMask    :: BitSet (Holder ConfigInformationMask) ConfigInformationMask
    , openReqReserved1        :: Word32
    , openReqReserved2        :: Word32
    , openReqReserved3        :: Word32
    } deriving (Binary, Eq, Generic, Show)

data OpenConfR = OpenConfR                  -- 4
    { openConfInvokeID                 :: InvokeID
    , openConfServiceGranted           :: BitSet (Holder CtiServiceMask) CtiServiceMask
    , openConfMonitorID                :: MonitorID
    , openConfPGStatus                 :: BitSet (Holder PGStatusCodeMask) PGStatusCodeMask
    , openConfIcmCentralControllerTime :: Timestamp
    , openConfPeripheralOnline         :: SafeEnum CtiBool
    , openConfPeripheralType           :: SafeEnum (CtiOpt PeripheralType)
    , openConfAgentState               :: SafeEnum AgentState
    , openConfDepartmentID             :: DepartmentID
    } deriving (Binary, Eq, Generic, Show)

data HeartBeatReqR = HeartBeatReqR          -- 5
    { heartBeatReqInvokeID :: InvokeID
    } deriving (Binary, Eq, Generic, Show)

data HeartBeatConfR = HeartBeatConfR        -- 6
    { heartBeatConfInvokeID :: InvokeID
    } deriving (Binary, Eq, Generic, Show)

data CloseReqR = CloseReqR                  -- 7
    { closeReqInvokeID :: InvokeID
    , closeReqStatus   :: SafeEnum FailureIndicationMessageStatusCode
    } deriving (Binary, Eq, Generic, Show)

data CloseConfR = CloseConfR                -- 8
    { closeConfInvokeID :: InvokeID
    } deriving (Binary, Eq, Generic, Show)

data CallDeliveredEventR = CallDeliveredEventR  -- 9
    { callDeliveredEventMonitorID              :: MonitorID
    , callDeliveredEventPeripheralID           :: PeripheralID
    , callDeliveredEventPeripheralType         :: SafeEnum (CtiOpt PeripheralType)
    , callDeliveredEventConnectionDeviceIDType :: SafeEnum (CtiOpt ConnectionDeviceIDType)
    , callDeliveredEventConnectionCallID       :: ConnectionCallID
    , callDeliveredEventLineHandle             :: LineHandle
    , callDeliveredEventLineType               :: SafeEnum (CtiOpt LineType)
    , callDeliveredEventServiceNumber          :: ServiceNumber
    , callDeliveredEventServiceID              :: ServiceID
    , callDeliveredEventSkillGroupNumber       :: SkillGroupNumber
    , callDeliveredEventSkillGroupID           :: SkillGroupID
    , callDeliveredEventSkillGroupPriority     :: SkillGroupPriority
    , callDeliveredEventAlertingDeviceType     :: SafeEnum (CtiOpt DeviceIDType)
    , callDeliveredEventCallingDeviceType      :: SafeEnum (CtiOpt DeviceIDType)
    , callDeliveredEventCalledDeviceType       :: SafeEnum (CtiOpt DeviceIDType)
    , callDeliveredEventLastRedirectDeviceType :: SafeEnum (CtiOpt DeviceIDType)
    , callDeliveredEventLocalConnectionState   :: SafeEnum (CtiOpt LocalConnectionState)
    , callDeliveredEventEventCause             :: SafeEnum (CtiOpt EventCause)
    , callDeliveredEventNumNamedVariables      :: Word16
    , callDeliveredEventNumNamedArrays         :: Word16
    } deriving (Binary, Eq, Generic, Show)

data CallEstablishedEventR = CallEstablishedEventR  -- 10
    { callEstablishedEventMonitorID              :: MonitorID
    , callEstablishedEventPeripheralID           :: PeripheralID
    , callEstablishedEventPeripheralType         :: SafeEnum (CtiOpt PeripheralType)
    , callEstablishedEventConnectionDeviceIDType :: SafeEnum (CtiOpt ConnectionDeviceIDType)
    , callEstablishedEventConnectionCallID       :: ConnectionCallID
    , callEstablishedEventLineHandle             :: LineHandle
    , callEstablishedEventLineType               :: SafeEnum (CtiOpt LineType)
    , callEstablishedEventServiceNumber          :: ServiceNumber
    , callEstablishedEventServiceID              :: ServiceID
    , callEstablishedEventSkillGroupNumber       :: SkillGroupNumber
    , callEstablishedEventSkillGroupID           :: SkillGroupID
    , callEstablishedEventSkillGroupPriority     :: SkillGroupPriority
    , callEstablishedEventAnsweringDeviceType    :: SafeEnum (CtiOpt DeviceIDType)
    , callEstablishedEventCallingDeviceType      :: SafeEnum (CtiOpt DeviceIDType)
    , callEstablishedEventCalledDeviceType       :: SafeEnum (CtiOpt DeviceIDType)
    , callEstablishedEventLastRedirectDeviceType :: SafeEnum (CtiOpt DeviceIDType)
    , callEstablishedEventLocalConnectionState   :: SafeEnum (CtiOpt LocalConnectionState)
    , callEstablishedEventEventCause :: SafeEnum (CtiOpt EventCause)
    } deriving (Binary, Eq, Generic, Show)

data CallHeldEventR = CallHeldEventR    -- 11
    { callHeldEventMonitorID              :: MonitorID
    , callHeldEventPeripheralID           :: PeripheralID
    , callHeldEventPeripheralType         :: SafeEnum (CtiOpt PeripheralType)
    , callHeldEventConnectionDeviceIDType :: SafeEnum (CtiOpt ConnectionDeviceIDType)
    , callHeldEventConnectionCallID       :: ConnectionCallID
    , callHeldEventHoldingDeviceType      :: SafeEnum (CtiOpt DeviceIDType)
    , callHeldEventLocalConnectionState   :: SafeEnum (CtiOpt LocalConnectionState)
    , callHeldEventEventCause             :: SafeEnum (CtiOpt EventCause)
    } deriving (Binary, Eq, Generic, Show)

data CallRetrievedEventR = CallRetrievedEventR  -- 12
    { callRetrievedEventMonitorID              :: MonitorID
    , callRetrievedEventPeripheralID           :: PeripheralID
    , callRetrievedEventPeripheralType         :: SafeEnum (CtiOpt PeripheralType)
    , callRetrievedEventConnectionDeviceIDType :: SafeEnum (CtiOpt ConnectionDeviceIDType)
    , callRetrievedEventConnectionCallID       :: ConnectionCallID
    , callRetrievedEventRetrievingDeviceType   :: SafeEnum (CtiOpt DeviceIDType)
    , callRetrievedEventLocalConnectionState   :: SafeEnum (CtiOpt LocalConnectionState)
    , callRetrievedEventEventCause             :: SafeEnum (CtiOpt EventCause)
    } deriving (Binary, Eq, Generic, Show)

data CallClearedEventR = CallClearedEventR  -- 13
    { callClearedEventMonitorID              :: MonitorID
    , callClearedEventPeripheralID           :: PeripheralID
    , callClearedEventPeripheralType         :: SafeEnum (CtiOpt PeripheralType)
    , callClearedEventConnectionDeviceIDType :: SafeEnum (CtiOpt ConnectionDeviceIDType)
    , callClearedEventConnectionCallID       :: ConnectionCallID
    , callClearedEventLocalConnectionState   :: SafeEnum (CtiOpt LocalConnectionState)
    , callClearedEventEventCause             :: SafeEnum (CtiOpt EventCause)
    } deriving (Binary, Eq, Generic, Show)

data CallConnectionClearedEventR = CallConnectionClearedEventR  -- 14
    { callConnectionClearedEventMonitorID              :: MonitorID
    , callConnectionClearedEventPeripheralID           :: PeripheralID
    , callConnectionClearedEventPeripheralType         :: SafeEnum (CtiOpt PeripheralType)
    , callConnectionClearedEventConnectionDeviceIDType :: SafeEnum (CtiOpt ConnectionDeviceIDType)
    , callConnectionClearedEventConnectionCallID       :: ConnectionCallID
    , callConnectionClearedEventReleasingDeviceType    :: SafeEnum (CtiOpt DeviceIDType)
    , callConnectionClearedEventLocalConnectionState   :: SafeEnum (CtiOpt LocalConnectionState)
    , callConnectionClearedEventEventCause             :: SafeEnum (CtiOpt EventCause)
    } deriving (Binary, Eq, Generic, Show)

data CallOriginatedEventR = CallOriginatedEventR    -- 15
    { callOriginatedEventMonitorID              :: MonitorID
    , callOriginatedEventPeripheralID           :: PeripheralID
    , callOriginatedEventPeripheralType         :: SafeEnum (CtiOpt PeripheralType)
    , callOriginatedEventConnectionDeviceIDType :: SafeEnum (CtiOpt ConnectionDeviceIDType)
    , callOriginatedEventConnectionCallID       :: ConnectionCallID
    , callOriginatedEventLineHandle             :: LineHandle
    , callOriginatedEventLineType               :: SafeEnum (CtiOpt LineType)
    , callOriginatedEventServiceNumber          :: ServiceNumber
    , callOriginatedEventServiceID              :: ServiceID
    , callOriginatedEventSkillGroupNumber       :: SkillGroupNumber
    , callOriginatedEventSkillGroupID           :: SkillGroupID
    , callOriginatedEventSkillGroupPriority     :: SkillGroupPriority
    , callOriginatedEventCallingDeviceType      :: SafeEnum (CtiOpt DeviceIDType)
    , callOriginatedEventCalledDeviceType       :: SafeEnum (CtiOpt DeviceIDType)
    , callOriginatedEventLocalConnectionState   :: SafeEnum (CtiOpt LocalConnectionState)
    , callOriginatedEventEventCause             :: SafeEnum (CtiOpt EventCause)
    } deriving (Binary, Eq, Generic, Show)

data CallFailedEventR = CallFailedEventR    -- 16
    { callFailedEventMonitorID              :: MonitorID
    , callFailedEventPeripheralID           :: PeripheralID
    , callFailedEventPeripheralType         :: SafeEnum (CtiOpt PeripheralType)
    , callFailedEventConnectionDeviceIDType :: SafeEnum (CtiOpt ConnectionDeviceIDType)
    , callFailedEventConnectionCallID       :: ConnectionCallID
    , callFailedEventFailingDeviceType      :: SafeEnum (CtiOpt DeviceIDType)
    , callFailedEventCalledDeviceType       :: SafeEnum (CtiOpt DeviceIDType)
    , callFailedEventLocalConnectionState   :: SafeEnum (CtiOpt LocalConnectionState)
    , callFailedEventEventCause             :: SafeEnum (CtiOpt EventCause)
    } deriving (Binary, Eq, Generic, Show)

data CallConferencedEventR = CallConferencedEventR  -- 17
    { callConferencedEventMonitorID             :: MonitorID
    , callConferencedEventPeripheralID          :: PeripheralID
    , callConferencedEventPeripheralType        :: SafeEnum (CtiOpt PeripheralType)
    , callConferencedEventPrimaryDeviceIDType   :: SafeEnum (CtiOpt ConnectionDeviceIDType)
    , callConferencedEventPrimaryCallID         :: ConnectionCallID
    , callConferencedEventLineHandle            :: LineHandle
    , callConferencedEventLineType              :: SafeEnum (CtiOpt LineType)
    , callConferencedEventSkillGroupNumber      :: SkillGroupNumber
    , callConferencedEventSkillGroupID          :: SkillGroupID
    , callConferencedEventSkillGroupPriority    :: SkillGroupPriority
    , callConferencedEventNumParties            :: Word16
    , callConferencedEventSecondaryDeviceIDType :: SafeEnum (CtiOpt ConnectionDeviceIDType)
    , callConferencedEventSecondaryCallID       :: ConnectionCallID
    , callConferencedEventControllerDeviceType  :: SafeEnum (CtiOpt DeviceIDType)
    , callConferencedEventAddedPartyDeviceType  :: SafeEnum (CtiOpt DeviceIDType)
    , callConferencedEventLocalConnectionState  :: SafeEnum (CtiOpt LocalConnectionState)
    , callConferencedEventEventCause            :: SafeEnum (CtiOpt EventCause)
    } deriving (Binary, Eq, Generic, Show)

data CallTransferredEventR = CallTransferredEventR  -- 18
    { callTransferredEventMonitorID              :: MonitorID
    , callTransferredEventPeripheralID           :: PeripheralID
    , callTransferredEventPeripheralType         :: SafeEnum (CtiOpt PeripheralType)
    , callTransferredEventPrimaryDeviceIDType    :: SafeEnum (CtiOpt ConnectionDeviceIDType)
    , callTransferredEventPrimaryCallID          :: ConnectionCallID
    , callTransferredEventLineHandle             :: LineHandle
    , callTransferredEventLineType               :: SafeEnum (CtiOpt LineType)
    , callTransferredEventSkillGroupNumber       :: SkillGroupNumber
    , callTransferredEventSkillGroupID           :: SkillGroupID
    , callTransferredEventSkillGroupPriority     :: SkillGroupPriority
    , callTransferredEventNumParties             :: Word16
    , callTransferredEventSecondaryDeviceIDType  :: SafeEnum (CtiOpt ConnectionDeviceIDType)
    , callTransferredEventSecondaryCallID        :: ConnectionCallID
    , callTransferredEventTransferringDeviceType :: SafeEnum (CtiOpt DeviceIDType)
    , callTransferredEventTransferredDeviceType  :: SafeEnum (CtiOpt DeviceIDType)
    , callTransferredEventLocalConnectionState   :: SafeEnum (CtiOpt LocalConnectionState)
    , callTransferredEventEventCause             :: SafeEnum (CtiOpt EventCause)
    } deriving (Binary, Eq, Generic, Show)

data CallDivertedEventR = CallDivertedEventR    -- 19
    { callDivertedEventMonitorID              :: MonitorID
    , callDivertedEventPeripheralID           :: PeripheralID
    , callDivertedEventPeripheralType         :: SafeEnum (CtiOpt PeripheralType)
    , callDivertedEventConnectionDeviceIDType :: SafeEnum (CtiOpt ConnectionDeviceIDType)
    , callDivertedEventConnectionCallID       :: ConnectionCallID
    , callDivertedEventServiceNumber          :: ServiceNumber
    , callDivertedEventServiceID              :: ServiceID
    , callDivertedEventDivertingDeviceType    :: SafeEnum (CtiOpt DeviceIDType)
    , callDivertedEventCalledDeviceType       :: SafeEnum (CtiOpt DeviceIDType)
    , callDivertedEventLocalConnectionState   :: SafeEnum (CtiOpt LocalConnectionState)
    , callDivertedEventEventCause             :: SafeEnum (CtiOpt EventCause)
    } deriving (Binary, Eq, Generic, Show)

data CallServiceInitiatedEventR = CallServiceInitiatedEventR    -- 20
    { callServiceInitiatedEventMonitorID              :: MonitorID
    , callServiceInitiatedEventPeripheralID           :: PeripheralID
    , callServiceInitiatedEventPeripheralType         :: SafeEnum (CtiOpt PeripheralType)
    , callServiceInitiatedEventConnectionDeviceIDType :: SafeEnum (CtiOpt ConnectionDeviceIDType)
    , callServiceInitiatedEventConnectionCallID       :: ConnectionCallID
    , callServiceInitiatedEventLineHandle             :: LineHandle
    , callServiceInitiatedEventLineType               :: SafeEnum (CtiOpt LineType)
    , callServiceInitiatedEventServiceNumber          :: ServiceNumber
    , callServiceInitiatedEventServiceID              :: ServiceID
    , callServiceInitiatedEventSkillGroupNumber       :: SkillGroupNumber
    , callServiceInitiatedEventSkillGroupID           :: SkillGroupID
    , callServiceInitiatedEventSkillGroupPriority     :: SkillGroupPriority
    , callServiceInitiatedEventCallingDeviceType      :: SafeEnum (CtiOpt DeviceIDType)
    , callServiceInitiatedEventLocalConnectionState   :: SafeEnum (CtiOpt LocalConnectionState)
    , callServiceInitiatedEventEventCause             :: SafeEnum (CtiOpt EventCause)
    } deriving (Binary, Eq, Generic, Show)

data CallQueuedEventR = CallQueuedEventR    -- 21
    { callQueuedEventMonitorID              :: MonitorID
    , callQueuedEventPeripheralID           :: PeripheralID
    , callQueuedEventPeripheralType         :: SafeEnum (CtiOpt PeripheralType)
    , callQueuedEventConnectionDeviceIDType :: SafeEnum (CtiOpt ConnectionDeviceIDType)
    , callQueuedEventConnectionCallID       :: ConnectionCallID
    , callQueuedEventServiceNumber          :: ServiceNumber
    , callQueuedEventServiceID              :: ServiceID
    , callQueuedEventQueueDeviceType        :: SafeEnum (CtiOpt DeviceIDType)
    , callQueuedEventCallingDeviceType      :: SafeEnum (CtiOpt DeviceIDType)
    , callQueuedEventCalledDeviceType       :: SafeEnum (CtiOpt DeviceIDType)
    , callQueuedEventLastRedirectDeviceType :: SafeEnum (CtiOpt DeviceIDType)
    , callQueuedEventNumQueued              :: Word16
    , callQueuedEventNumSkillGroups         :: Word16
    , callQueuedLocalConnectionState        :: SafeEnum (CtiOpt LocalConnectionState)
    , callQueuedEventEventCause             :: SafeEnum (CtiOpt EventCause)
    } deriving (Binary, Eq, Generic, Show)

data CallTranslationRouteEventR = CallTranslationRouteEventR    -- 22
    { callTranslationRoutenumNamedVariables :: Word16
    , callTranslationRoutenumNamedArrays    :: Word16
    } deriving (Binary, Eq, Generic, Show)

data BeginCallEventR = BeginCallEventR      -- 23
    { beginCallEventMonitorID              :: MonitorID
    , beginCallEventPeripheralID           :: PeripheralID
    , beginCallEventPeripheralType         :: SafeEnum (CtiOpt PeripheralType)
    , beginCallEventNumCtiClients          :: Word16
    , beginCallEventNumNamedVariables      :: Word16
    , beginCallEventNumNamedArrays         :: Word16
    , beginCallEventCallType               :: SafeEnum (CtiOpt CallType)
    , beginCallEventConnectionDeviceIDType :: SafeEnum (CtiOpt ConnectionDeviceIDType)
    , beginCallEventConnectionCallID       :: ConnectionCallID
    , beginCallEventCalledPartyDisposition :: SafeEnum Disposition
    } deriving (Binary, Eq, Generic, Show)

data EndCallEventR = EndCallEventR          -- 24
    { endCallEventMonitorID              :: MonitorID
    , endCallEventPeripheralID           :: PeripheralID
    , endCallEventPeripheralType         :: SafeEnum (CtiOpt PeripheralType)
    , endCallEventConnectionDeviceIDType :: SafeEnum (CtiOpt ConnectionDeviceIDType)
    , endCallEventConnectionCallID       :: ConnectionCallID
    } deriving (Binary, Eq, Generic, Show)

data CallDataUpdateEventR = CallDataUpdateEventR    -- 25
    { callDataUpdateEventMonitorID                 :: MonitorID
    , callDataUpdateEventPeripheralID              :: PeripheralID
    , callDataUpdateEventPeripheralType            :: SafeEnum (CtiOpt PeripheralType)
    , callDataUpdateEventNumCtiClients             :: Word16
    , callDataUpdateEventNumNamedVariables         :: Word16
    , callDataUpdateEventNumNamedArrays            :: Word16
    , callDataUpdateEventCallType                  :: SafeEnum (CtiOpt CallType)
    , callDataUpdateEventConnectionDeviceIDType    :: SafeEnum (CtiOpt ConnectionDeviceIDType)
    , callDataUpdateEventConnectionCallID          :: ConnectionCallID
    , callDataUpdateEventNewConnectionDeviceIDType :: SafeEnum (CtiOpt ConnectionDeviceIDType)
    , callDataUpdateEventNewConnectionCallID       :: ConnectionCallID
    , callDataUpdateEventCalledPartyDisposition    :: SafeEnum Disposition
    , callDataUpdateEventCampaignID                :: CampaignID
    , callDataUpdateEventQueryRuleID               :: QueryRuleID
    } deriving (Binary, Eq, Generic, Show)

data SetCallDataReqR = SetCallDataReqR      -- 26
    { setCallDataReqInvokeID               :: InvokeID
    , setCallDataReqPeripheralID           :: PeripheralID
    , setCallDataReqConnectionDeviceIDType :: SafeEnum (CtiOpt ConnectionDeviceIDType)
    , setCallDataReqConnectionCallID       :: ConnectionCallID
    , setCallDataReqNumNamedVariables      :: Word16
    , setCallDataReqNumNamedArrays         :: Word16
    , setCallDataReqCallType               :: SafeEnum (CtiOpt CallType)
    , setCallDataReqCalledPartyDisposition :: SafeEnum Disposition
    , setCallDataReqCampaignID             :: CampaignID
    , setCallDataReqQueryRuleID            :: QueryRuleID
    } deriving (Binary, Eq, Generic, Show)

data SetCallDataConfR = SetCallDataConfR    -- 27
    { setCallDataConfInvokeID :: InvokeID
    } deriving (Binary, Eq, Generic, Show)

data ReleaseCallReqR = ReleaseCallReqR      -- 28
    { releaseCallReqInvokeID               :: InvokeID
    , releaseCallReqPeripheralID           :: PeripheralID
    , releaseCallReqConnectionDeviceIDType :: SafeEnum (CtiOpt ConnectionDeviceIDType)
    , releaseCallReqConnectionCallID       :: ConnectionCallID
    } deriving (Binary, Eq, Generic, Show)

data ReleaseCallConfR = ReleaseCallConfR    -- 29
    { releaseCallConfInvokeID :: InvokeID
    } deriving (Binary, Eq, Generic, Show)

data AgentStateEventR = AgentStateEventR    -- 30
    { agentStateEventMonitorID               :: MonitorID
    , agentStateEventPeripheralID            :: PeripheralID
    , agentStateEventSessionID               :: SessionID
    , agentStateEventPeripheralType          :: SafeEnum (CtiOpt PeripheralType)
    , agentStateEventSkillGroupState         :: SafeEnum AgentState
    , agentStateEventStateDuration           :: Word32
    , agentStateEventSkillGroupNumber        :: SkillGroupNumber
    , agentStateEventSkillGroupID            :: SkillGroupID
    , agentStateEventSkillGroupPriority      :: SkillGroupPriority
    , agentStateEventAgentState              :: SafeEnum AgentState
    , agentStateEventEventReasonCode         :: EventReasonCode
    , agentStateEventMrdID                   :: MrdID
    , agentStateEventNumTasks                :: Word32
    , agentStateEventAgentMode               :: SafeEnum AgentMode
    , agentStateEventMaxTaskLimit            :: Word32
    , agentStateEventIcmAgentID              :: IcmAgentID
    , agentStateEventAgentAvailabilityStatus :: SafeEnum AgentAvailabilityStatus
    , agentStateEventNumFltSkillGroups       :: Word16
    , agentStateEventDepartmentID            :: DepartmentID
    } deriving (Binary, Eq, Generic, Show)

data SystemEventR = SystemEventR    -- 31
    { systemEventPGStatus                 :: BitSet (Holder PGStatusCodeMask) PGStatusCodeMask
    , systemEventIcmCentralControllerTime :: Timestamp
    , systemEventSystemEventID            :: SafeEnum SystemEventID
    , systemEventSystemEventArg1          :: SystemEventArg1
    , systemEventSystemEventArg2          :: SystemEventArg2
    , systemEventSystemEventArg3          :: SystemEventArg3
    , systemEventEventDeviceType          :: SafeEnum (CtiOpt DeviceIDType)
    } deriving (Binary, Eq, Generic, Show)

data ClientEventReportReqR = ClientEventReportReqR  -- 32
    { clientEventReportReqInvokeID :: InvokeID
    , clientEventReportReqState    :: SafeEnum ClientEventReportReqState
    } deriving (Binary, Eq, Generic, Show)

data ClientEventReportConfR = ClientEventReportConfR    -- 33
    { clientEventReportConfInvokeID :: InvokeID
    } deriving (Binary, Eq, Generic, Show)

data CallReachedNetworkEventR = CallReachedNetworkEventR    -- 34
    { callReachedNetworkEventMonitorID              :: MonitorID
    , callReachedNetworkEventPeripheralID           :: PeripheralID
    , callReachedNetworkEventPeripheralType         :: SafeEnum (CtiOpt PeripheralType)
    , callReachedNetworkEventConnectionDeviceIDType :: SafeEnum (CtiOpt ConnectionDeviceIDType)
    , callReachedNetworkEventConnectionCallID       :: ConnectionCallID
    , callReachedNetworkEventLineHandle             :: LineHandle
    , callReachedNetworkEventLineType               :: SafeEnum (CtiOpt LineType)
    , callReachedNetworkEventTrunkUsedDeviceType    :: SafeEnum (CtiOpt DeviceIDType)
    , callReachedNetworkEventCalledDeviceType       :: SafeEnum (CtiOpt DeviceIDType)
    , callReachedNetworkEventLocalConnectionState   :: SafeEnum (CtiOpt LocalConnectionState)
    , callReachedNetworkEventEventCause             :: SafeEnum (CtiOpt EventCause)
    } deriving (Binary, Eq, Generic, Show)

data ControlFailureConfR = ControlFailureConfR  -- 35
    { controlFailureConfInvokeID            :: InvokeID
    , controlFailureConfFailureCode         :: SafeEnum ControlFailureCode
    , controlFailureConfPeripheralErrorCode :: PeripheralErrorCode
    } deriving (Binary, Eq, Generic, Show)

data QueryAgentStateReqR = QueryAgentStateReqR  -- 36
    { queryAgentStateReqInvokeID     :: InvokeID
    , queryAgentStateReqPeripheralID :: PeripheralID
    , queryAgentStateReqMrdID        :: MrdID
    , queryAgentStateIcmAgentID      :: IcmAgentID
    } deriving (Binary, Eq, Generic, Show)

data QueryAgentStateConfR = QueryAgentStateConfR    -- 37
    { queryAgentStateConfInvokeID                :: InvokeID
    , queryAgentStateConfAgentState              :: SafeEnum AgentState
    , queryAgentStateConfNumSkillGroups          :: Word16
    , queryAgentStateConfMrdID                   :: MrdID
    , queryAgentStateConfNumTasks                :: Word32
    , queryAgentStateConfAgentMode               :: SafeEnum AgentMode
    , queryAgentStateConfMaxTaskLimit            :: Word32
    , queryAgentStateConfIcmAgentID              :: IcmAgentID
    , queryAgentStateConfAgentAvailabilityStatus :: SafeEnum AgentAvailabilityStatus
    , queryAgentStateConfDepartmentID            :: DepartmentID
    } deriving (Binary, Eq, Generic, Show)

data SetAgentStateReqR = SetAgentStateReqR  -- 38
    { setAgentStateReqInvokeID            :: InvokeID
    , setAgentStateReqPeripheralID        :: PeripheralID
    , setAgentStateReqAgentState          :: SafeEnum AgentState
    , setAgentStateReqAgentWOrkMode       :: SafeEnum ForcedFlag
    , setAgentStateReqAgentServiceRequest :: BitSet (Holder AgentServiceRequestMask) AgentServiceRequestMask
    } deriving (Binary, Eq, Generic, Show)

data SetAgentStateConfR = SetAgentStateConfR    -- 39
    { setAgentStateConfInvokeID :: InvokeID
    } deriving (Binary, Eq, Generic, Show)

