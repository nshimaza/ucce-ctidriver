{- |
Module          : Network.CceCti
Description     : CTI Server Protocol driver for Cisco UCCE
Copyright       : Naoto Shimazaki
License         : MIT
Maintainer      : nshimaza@cisco.com
Stability       : unstable
Portability     : unknown

Driver for CTI Server Protocol of Cisco Unified Contact Center Enterprise.
It decodes binary based CTI messages into Haskell objects and encodes them to binary.
-}
module Network.CceCti (
    -- * Protocol version constant
      protocolVersion

    -- * Simple integral types
    , CallID(..)
    , DepartmentID(..)
    , EventReasonCode(..)
    , IcmAgentID(..)
    , InvokeID(..)
    , MonitorID(..)
    , MrdID(..)
    , PeripheralErrorCode(..)
    , SystemEventArg1(..)
    , SystemEventArg2(..)
    , SystemEventArg3(..)
    , Timestamp(..)

    -- * Integral types with special value
    -- ** Local Maybe to avoid conflict of binary encoder implementation
    , CtiOpt(..), CtiOpt0(..)
    -- ** 32bit unsigned types
    , ConnectionCallID(..)
    , PeripheralID(..)
    , ServiceID(..)
    , ServiceNumber(..)
    , SkillGroupID(..)
    , SkillGroupNumber(..)
    -- ** 16bit unsigned types
    , LineHandle(..)
    -- ** 32bit unsigned types with zero special value
    , CampaignID(..)
    , QueryRuleID(..)
    , SessionID(..)
    -- ** 16bit unsigned types with zero special value
    , SkillGroupPriority(..)

    -- * Enumeration types
    , SafeEnum(..)
    , AgentAvailabilityStatus(..)
    , AgentInternalState(..)
    , AgentMode(..)
    , AgentState(..)
    , AgentWorkMode(..)
    , CallType(..)
    , ClientEventReportReqState(..)
    , ConnectionDeviceIDType(..)
    , ControlFailureCode(..)
    , CtiBool(..)
    , DeviceIDType(..)
    , Disposition(..)
    , Direction(..)
    , EventCause(..)
    , FailureIndicationMessageStatusCode(..)
    , LineType(..)
    , LocalConnectionState(..)
    , MultilineAgentConrol(..)
    , PeripheralType(..)
    , SystemEventID(..)

    -- * Operations on BitMask
    , fromList, toList, empty, insert, member

    -- * Bitmask types
    , AgentFlagsMask(..)
    , AgentServiceRequestMask(..)
    , AgentStateMask(..)
    , CallControlMask(..)
    , CallEventMessageMask(..)
    , CallVariableMask(..)
    , ClassOfDevice(..)
    , ConfigInformationMask(..)
    , CtiServiceMask(..)
    , DeskSettingsMask(..)
    , OtherFeatureMask(..)
    , PGStatusCodeMask(..)
    , TransferConferenceSetupMask(..)

    -- * Floating part types
    , Unspec8(..), Unspec32(..), Unspec64(..), Unspec131(..)
    , fromWord8List
    , String2(..), String4(..), String12(..), String16(..), String20(..), String32(..)
    , String40(..), String41(..), String64(..), String128(..), String255(..)
    , fromString
    , NamedVar(..), NamedArr(..)
    , FloatPart(..), FloatElem(..)

    -- * Fixed part of messages
    , Message(..), FixedPart(..)
    , FailureConfR(..)
    , FailureEventR(..)
    , OpenReqR(..)
    , OpenConfR(..)
    , HeartBeatReqR(..)
    , HeartBeatConfR(..)
    , CloseReqR(..)
    , CloseConfR(..)
    , CallDeliveredEventR(..)
    , CallEstablishedEventR(..)
    , CallHeldEventR(..)
    , CallRetrievedEventR(..)
    , CallClearedEventR(..)
    , CallConnectionClearedEventR(..)
    , CallOriginatedEventR(..)
    , CallFailedEventR(..)
    , CallConferencedEventR(..)
    , CallTransferredEventR(..)
    , CallDivertedEventR(..)
    , CallServiceInitiatedEventR(..)
    , CallQueuedEventR(..)
    , CallTranslationRouteEventR(..)
    , BeginCallEventR(..)
    , EndCallEventR(..)
    , CallDataUpdateEventR(..)
    , SetCallDataReqR(..)
    , SetCallDataConfR(..)
    , ReleaseCallReqR(..)
    , ReleaseCallConfR(..)
    , AgentStateEventR(..)
    , SystemEventR(..)
    , ClientEventReportReqR(..)
    , ClientEventReportConfR(..)
    , CallReachedNetworkEventR(..)
    , ControlFailureConfR(..)
) where

import           Data.String (fromString)

import           Network.CceCti.BitMask
import           Network.CceCti.Enum
import           Network.CceCti.FixedPart
import           Network.CceCti.FloatElem
import           Network.CceCti.FloatPart
import           Network.CceCti.MessageRec
import           Network.CceCti.Types
