{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Network.CceCti.BitMask where

import           Data.Binary  (Binary)
import           Data.Bits    (Bits, setBit, testBit)
import           Data.Word    (Word16, Word32)
import           GHC.Generics (Generic)

newtype BitSet bits mask = BitSet bits deriving (Binary, Eq, Generic, Show)

class (Bounded mask, Bits (Holder mask), Enum mask, Num (Holder mask)) => BitMask mask where
    type Holder mask :: *
    type Holder m = Word32

    insert :: mask -> BitSet (Holder mask) mask -> BitSet (Holder mask) mask
    insert m (BitSet bits) = BitSet $ bits `setBit` fromEnum m

    empty :: (Num bits) => BitSet bits mask
    empty = BitSet 0

    member :: mask -> BitSet (Holder mask) mask -> Bool
    member m (BitSet bits) = bits `testBit` fromEnum m

    fromList :: [mask] -> BitSet (Holder mask) mask
    fromList = BitSet . foldr (\mask bits -> bits `setBit` fromEnum mask) 0

    toList :: BitSet (Holder mask) mask -> [mask]
    toList bits = filter (`member` bits) [minBound..maxBound]


data AgentFlagsMask
    = PrimarySupervisorMask -- 0
    | TemporaryAgentMask    -- 1
    | SupervisorMask        -- 2
    deriving (Bounded, Enum, Eq, Generic, Show)

instance BitMask AgentFlagsMask where
    type Holder AgentFlagsMask = Word16


data AgentServiceRequestMask
    = AgentServiceOutboundSupportMask   -- 0
    deriving (Bounded, Enum, Eq, Generic, Show)
instance BitMask AgentServiceRequestMask

data AgentStateMask
    = AgentLoginMask        -- 0
    | AgentLogoutMask       -- 1
    | AgentNotReadyMask     -- 2
    | AgentAvailableMask    -- 3
    | AgentTalkingMask      -- 4
    | AgentWorkNotReadyMask -- 5
    | AgentWorkReadyMask    -- 6
    | AgentBusyOtherMask    -- 7
    | AgentReservedMask     -- 8
    | AgentHoldMask         -- 9
    deriving (Bounded, Enum, Eq, Generic, Show)
instance BitMask AgentStateMask

data CallControlMask
    = ControlQueryAgentStateMask    --  0
    | ControlSetAgentStateMask      --  1
    | ControlAlternateCallMask      --  2
    | ControlAnswerCallMask         --  3
    | ControlClearCallMask          --  4
    | ControlClearConnectionMask    --  5
    | ControlConferenceCallMask     --  6
    | ControlConsultationCallMask   --  7
    | ControlDeflectCallMask        --  8
    | ControlHoldCallMask           --  9
    | ControlMakeCallMask           -- 10
    | ControlMakePredictiveCall     -- 11
    | ControlReconnectCallMask      -- 12
    | ControlRetrieveCallMask       -- 13
    | ControlTransferCallMask       -- 14
    | ControlQueryDeviceInfoMask    -- 15
    | ControlSnapshotCallMask       -- 16
    | ControlSnapshotDeviceMask     -- 17
    | ControlSendDtmfSignalMask     -- 18
    deriving (Bounded, Enum, Eq, Generic, Show)
instance BitMask CallControlMask

data CallEventMessageMask
    = CallDeliveredMask         --  0
    | CallQueuedMask            --  1
    | CallEstablishedMask       --  2
    | CallHeldMask              --  3
    | CallRetrievedMask         --  4
    | CallClearedMask           --  5
    | CallConnectionClearedMask --  6
    | CallOriginatedMask        --  7
    | CallConferencedMask       --  8
    | CallTransferredMask       --  9
    | CallDivertedMask          -- 10
    | CallServiceInitiatedMask  -- 11
    | CallTranslationRouteMask  -- 12
    | BeginCallMask             -- 13
    | EndCallMask               -- 14
    | CallDataUpdateMask        -- 15
    | CallFailedMask            -- 16
    | CallReachedNetworkMask    -- 17
    | CallDequeuedMask          -- 18
    | AgentPreCallMask          -- 19
    | AgentPreCallAbortMask     -- 20
    | RtpStartedMask            -- 21
    | RtpStoppedMask            -- 22
    | AgentTeamConfigMask       -- 23
    | AgentLegacyPreCallMask    -- 24
    | CallAttributeChangeMask   -- 25
    | CallTerminationMask       -- 26
    | CallAgentGreetingMask     -- 27
    deriving (Bounded, Enum, Eq, Generic, Show)
instance BitMask CallEventMessageMask


data CallVariableMask
    = CallVariable1Mask     -- 0
    | CallVariable2Mask     -- 1
    | CallVariable3Mask     -- 2
    | CallVariable4Mask     -- 3
    | CallVariable5Mask     -- 4
    | CallVariable6Mask     -- 5
    | CallVariable7Mask     -- 6
    | CallVariable8Mask     -- 7
    | CallVariable9Mask     -- 8
    | CallVariable10Mask    -- 9
    deriving (Bounded, Enum, Eq, Generic, Show)

instance BitMask CallVariableMask where
    type Holder CallVariableMask = Word16


data ClassOfDevice
    = DevCReserved0Mask -- 0
    | DevCReserved1Mask -- 1
    | DevCReserved2Mask -- 2
    | DevCReserved3Mask -- 3
    | DevCOtherMask     -- 4
    | DevCImageMask     -- 5
    | DevCDataMask      -- 6
    | DevCVoiceMask     -- 7
    deriving (Bounded, Enum, Eq, Generic, Show)

instance BitMask ClassOfDevice where
    type Holder ClassOfDevice = Word16


data ConfigInformationMask
    = ConfigInfoServiceMask             -- 0
    | ConfigInfoSkillGroupMask          -- 1
    | ConfigInfoAgentMask               -- 2
    | ConfigInfoDeviceMask              -- 3
    | ConfigInfoCallTypeMask            -- 4
    | ConfigInfoMediaRoutingDomainMask  -- 5
    deriving (Bounded, Enum, Eq, Generic, Show)
instance BitMask ConfigInformationMask

data CtiServiceMask
    = CtiServiceClientEvents                --  0
    | CtiServiceCallDataUpdate              --  1
    | CtiServiceClientControl               --  2
    | CtiServiceConnectionMonitor           --  3
    | CtiServiceAllEvents                   --  4
    | CtiServicePeripheralMonitor           --  5
    | CtiServiceClientMonitor               --  6
    | CtiServiceSupervisor                  --  7
    | CtiServiceServer                      --  8
    | CtiServiceReserved9                   --  9
    | CtiServiceAgentReporting              -- 10
    | CtiServiceAllTaskEvents               -- 11
    | CtiServiceTaskMonitor                 -- 12
    | CtiServiceAgentStateControlOnly       -- 13
    | CtiServiceReserved14                  -- 14
    | CtiServiceDeviceStateControl          -- 15
    | CtiServiceReserved16                  -- 16
    | CtiServiceReserved17                  -- 17
    | CtiServiceReserved18                  -- 18
    | CtiServiceUpdateEvents                -- 19
    | CtiServiceIgnoreDuplicateAgentEvents  -- 20
    | CtiServiceIgnoreConf                  -- 21
    | CtiServiceAcdLineOnly                 -- 22
    | CtiServiceReserved23                  -- 23
    | CtiServiceReserved24                  -- 24
    | CtiServiceReserved25                  -- 25
    | CtiServiceReserved26                  -- 26
    | CtiServiceReserved27                  -- 27
    | CtiServiceReserved28                  -- 28
    | CtiServiceReserved29                  -- 29
    | CtiServiceReserved30                  -- 30
    | CtiServiceDebug                       -- 31
    deriving (Bounded, Enum, Eq, Generic, Show)
instance BitMask CtiServiceMask

data DeskSettingsMask
    = DeskAvailAfterIncomingMask                --  0
    | DeskAvailAfterOutgoingMask                --  1
    | DeskAutoAnswerEnabledMask                 --  2
    | DeskIdleReasonRequiredMask                --  3
    | DeskLogoutReasonRequiredMask              --  4
    | DeskSupervisorCallsAllowedMask            --  5
    | DeskAgentToAgentCallsAllowedMask          --  6
    | DeskOutboundAccessInternationalMask       --  7
    | DeskOutboundAccessPublicNetMask           --  8
    | DeskOutboundAccessPrivateNetMask          --  9
    | DeskOutboundAccessOperatorAssistedMask    -- 10
    | DeskOutboundAccessPbxMask                 -- 11
    | DeskNonAcdCallsAllowedMask                -- 12
    | DeskAgentCanSelectGroupMask               -- 13
    deriving (Bounded, Enum, Eq, Generic, Show)
instance BitMask DeskSettingsMask

data OtherFeatureMask
    = FeaturePostRouteMask              -- 0
    | FeatureUniqueConsultCallIDMask    -- 1
    deriving (Bounded, Enum, Eq, Generic, Show)
instance BitMask OtherFeatureMask

data PGStatusCodeMask
    = PgsOpcDown            -- 0
    | PgsCCDown             -- 1
    | PgsPeripheralOffline  -- 2
    | PgsCtiServerOffline   -- 3
    | PgsLimitedFunction    -- 4
    deriving (Bounded, Enum, Eq, Generic, Show)
instance BitMask PGStatusCodeMask

data TransferConferenceSetupMask
    = ConfSetupConsultSpecificMask  -- 0
    | ConfSetupConsultAnyMask       -- 1
    | ConfSetupConnHeldMask         -- 2
    | ConfSetupAnyTwoCallsMask      -- 3
    | ConfSetupSingleAcdCallMask    -- 4
    | TransSetupSingleAcdCallMask   -- 5
    | ConfSetupAnySingleCallMask    -- 6
    | TransSetupAnySingleCallMask   -- 7
    deriving (Bounded, Enum, Eq, Generic, Show)
instance BitMask TransferConferenceSetupMask
