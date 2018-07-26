{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.CceCti.Enum where

import           Data.Binary          (Binary, get, put)
import           Data.Binary.Get      (Get, getWord16be, getWord32be, getWord8)
import           Data.Binary.Put      (Put, putWord16be, putWord32be, putWord8)
import           Data.Monoid          ((<>))
import           Data.Typeable        (Typeable, typeOf)
import           GHC.Generics         (Generic)

import           Network.CceCti.Types

{-
Safe polymorphic enumeration decoder for enum types which consists of only sequential enum values.
Decodes enumeration in Right when object binary contains valid enum value.
Decodes Left when the binary contains unrecognized value.
-}
newtype SafeEnum a = SafeEnum (Either String a) deriving (Eq, Generic, Show)

getSafeEnum :: (Bounded a, Enum a, Typeable a, Integral i, Show i) => Get i -> Get (SafeEnum a)
getSafeEnum getIntegral = do
    w <- getIntegral
    let r = toEnum $ fromIntegral w
    return $ if fromIntegral w <= fromEnum (maxBound `asTypeOf` r)
             then SafeEnum $ Right r
             else SafeEnum . Left $ getEnumError w r

getEnumError :: (Bounded a, Enum a, Typeable a, Show i) => i -> a -> String
getEnumError v t = "Decoded value " <> sv <> " is outside of enumeration " <> st <> " " <> sr
  where
    sv = show v
    st = show $ typeOf t
    sr = show (fromEnum (minBound `asTypeOf` t), fromEnum (maxBound `asTypeOf` t))

{-
Unsafe polymorphic enumeration decoder for enum types which consists of only sequential enum values.
Decodes enumeration when object binary contains valid enum value.
Fail decoder when the binary contains unrecognized value.
User can catch failure using Data.Binary.decodeOrFail
-}
getEnum :: (Bounded a, Enum a, Typeable a, Integral i, Show i) => Get i -> Get a
getEnum getIntegral = do
    w <- getIntegral
    let r = toEnum $ fromIntegral w
    if fromIntegral w <= fromEnum (maxBound `asTypeOf` r)
    then return r
    else fail $ getEnumError w r

{-
Encoding SafeEnum
Generally you should not use these functions.  SafeEnum is not to be used for encoding but for decoding.
You should use deterministic enums instead for encoding.
You must not use putSafeEnum with Left value.  It doesn't encode properly.  It generates error.
-}
putSafeEnumError :: a
putSafeEnumError = error "SafeEnum containing Left cannot be used for encoding message"

putSafeEnum :: (Binary a) => SafeEnum a -> Put
putSafeEnum (SafeEnum (Right e)) = put e
putSafeEnum _                    = putSafeEnumError

{-
Safe concrete enumeration decoders with only sequential values
-}
getSafeEnum32 :: (Bounded a, Enum a, Typeable a) => Get (SafeEnum a)
getSafeEnum32 = getSafeEnum getWord32be

getSafeEnum16 :: (Bounded a, Enum a, Typeable a) => Get (SafeEnum a)
getSafeEnum16 = getSafeEnum getWord16be

getSafeEnum8 :: (Bounded a, Enum a, Typeable a) => Get (SafeEnum a)
getSafeEnum8 = getSafeEnum getWord8

{-
Unsafe concrete enumeration decoders with only sequential values
-}
getEnum32 :: (Bounded a, Enum a, Typeable a) => Get a
getEnum32 = getEnum getWord32be

getEnum16 :: (Bounded a, Enum a, Typeable a) => Get a
getEnum16 = getEnum getWord16be

getEnum8 :: (Bounded a, Enum a, Typeable a) => Get a
getEnum8 = getEnum getWord8

{-
Concrete enumeration encoders with only sequential values
-}
putEnum32 :: Enum a => a -> Put
putEnum32 = putWord32be . fromIntegral . fromEnum

putEnum16 :: Enum a => a -> Put
putEnum16 = putWord16be . fromIntegral . fromEnum

putEnum8 :: Enum a => a -> Put
putEnum8 = putWord8 . fromIntegral . fromEnum



{-
Enumerations which consists of sequential index and special index indicating "none".
Special index is maximum bounded number of representing unsigned integer.
-}
--data OptEnum a = None' | Some' a deriving (Eq, Generic, Show)


getSafeOptEnum :: (Bounded a, Enum a, Typeable a, Bounded i, Integral i, Show i) =>
                    Get i -> Get (SafeEnum (CtiOpt a))
getSafeOptEnum getIntegral = do
    w <- getIntegral
    let e = toEnum $ fromIntegral w
    let se = case w of
                n | fromIntegral n <= fromEnum (maxBound `asTypeOf` e) -> SafeEnum . Right . CtiOpt $ Just e
                  | n == (maxBound `asTypeOf` n)                       -> SafeEnum . Right $ CtiOpt Nothing
                  | otherwise                                          -> SafeEnum . Left $ getEnumError w e
    return se

{-
Unsafe polymorphic enumeration decoder for enum types which has special value in max bound of binary data.
Decodes enumeration in Just when object binary contains valid enum value.
Decodes Nothing when object binary contains special value which is max bound of binary data.
Fail decoder when the binary contains unrecognized value.
User can catch failure using Data.Binary.decodeOrFail
-}
getOptEnum :: (Bounded a, Enum a, Typeable a, Bounded i, Integral i, Show i) => Get i -> Get (CtiOpt a)
getOptEnum getIntegral = do
    w <- getIntegral
    let e = toEnum $ fromIntegral w
    case w of
        n | fromIntegral n <= fromEnum (maxBound `asTypeOf` e) -> return . CtiOpt $ Just e
          | n == (maxBound `asTypeOf` n)                       -> return $ CtiOpt Nothing
          | otherwise                                          -> fail $ getEnumError w e

{-
Safe concrete enumeration decoders with special value
-}
getSafeOptEnum32 :: (Bounded a, Enum a, Typeable a) => Get (SafeEnum (CtiOpt a))
getSafeOptEnum32 = getSafeOptEnum getWord32be

getSafeOptEnum16 :: (Bounded a, Enum a, Typeable a) => Get (SafeEnum (CtiOpt a))
getSafeOptEnum16 = getSafeOptEnum getWord16be

{-
Concrete enumeration encoders with special value
-}
getOptEnum32 :: (Bounded a, Enum a, Typeable a) => Get (CtiOpt a)
getOptEnum32 = getOptEnum getWord32be

getOptEnum16 :: (Bounded a, Enum a, Typeable a) => Get (CtiOpt a)
getOptEnum16 = getOptEnum getWord16be


{-
Definitions of CTI enumerations
-}
data AgentAvailabilityStatus
    = AgentAvailabilityStatusNotAvailable           -- 0
    | AgentAvailabilityStatusIcmAvailable           -- 1
    | AgentAvailabilityStatusApplicationAvailable   -- 2
    deriving (Bounded, Enum, Eq, Generic, Show)

instance Binary AgentAvailabilityStatus where
    get = getEnum32
    put = putEnum32

instance Binary (SafeEnum AgentAvailabilityStatus) where
    get = getSafeEnum32
    put = putSafeEnum


data AgentInternalState
    = AgentInternalStateLogin           --  0
    | AgentInternalStateLogout          --  1
    | AgentInternalStateNotReady        --  2
    | AgentInternalStateAvailable       --  3
    | AgentInternalStateTalking         --  4
    | AgentInternalStateWorkNotReady    --  5
    | AgentInternalStateWorkReady       --  6
    | AgentInternalStateBusyOther       --  7
    | AgentInternalStateReserved8       --  8
    | AgentInternalStateReserved9       --  9
    | AgentInternalStateReserved10      -- 10
    | AgentInternalStateActive          -- 11
    deriving (Bounded, Enum, Eq, Generic, Show)

instance Binary AgentInternalState where
    get = getEnum16
    put = putEnum16

instance Binary (SafeEnum AgentInternalState) where
    get = getSafeEnum16
    put = putSafeEnum


data AgentMode
    = AgentModeNotRoutable  -- 0
    | AgentModeRoutable     -- 1
    deriving (Bounded, Enum, Eq, Generic, Show)

instance Binary AgentMode where
    get = getEnum16
    put = putEnum16

instance Binary (SafeEnum AgentMode) where
    get = getSafeEnum16
    put = putSafeEnum


data AgentState
    = AgentStateLogin           --  0
    | AgentStateLogout          --  1
    | AgentStateNotReady        --  2
    | AgentStateAvailable       --  3
    | AgentStateTalking         --  4
    | AgentStateWorkNotReady    --  5
    | AgentStateWorkReady       --  6
    | AgentStateBusyOther       --  7
    | AgentStateReserved        --  8
    | AgentStateUnknown         --  9
    | AgentStateHold            -- 10
    | AgentStateActive          -- 11
    | AgentStatePaused          -- 12
    | AgentStateInterrupted     -- 13
    | AgentStateNotActive       -- 14
    deriving (Bounded, Enum, Eq, Generic, Show)

instance Binary AgentState where
    get = getEnum16
    put = putEnum16

instance Binary (SafeEnum AgentState) where
    get = getSafeEnum16
    put = putSafeEnum


data AgentType
    = AgentTypeAgent        --  0
    | AgentTypeSupervisor   --  1
    deriving (Bounded, Enum, Eq, Generic, Show)

instance Binary AgentType where
    get = getEnum16
    put = putEnum16

instance Binary (SafeEnum AgentType) where
    get = getSafeEnum16
    put = putSafeEnum


data AgentWorkMode
    = AgentWorkModeUnspecified                  -- 0
    | AgentWorkModeAutoIn                       -- 1
    | AgentWorkModeManualIn                     -- 2
    | AgentWorkModeRemoteAgentCallByCall        -- 3
    | AgentWrokModeRemoteAgentNailedConnection  -- 4
    deriving (Bounded, Enum, Eq, Generic, Show)

instance Binary AgentWorkMode where
    get = getEnum16
    put = putEnum16

instance Binary (SafeEnum AgentWorkMode) where
    get = getSafeEnum16
    put = putSafeEnum


data CallType
    = CallTypeZero                          --  0
    | CallTypeAcdIn                         --  1
    | CallTypePreRouteAcdIn                 --  2
    | CallTypePreRouteDirectAgent           --  3
    | CallTypeTransferIn                    --  4
    | CallTypeOverflowIn                    --  5
    | CallTypeOtherIn                       --  6
    | CallTypeAutOut                        --  7
    | CallTypeAgentOut                      --  8
    | CallTypeOut                           --  9
    | CallTypeAgentInside                   -- 10
    | CallTypeOffered                       -- 11
    | CallTypeConsult                       -- 12
    | CallTypeConsultOferred                -- 13
    | CallTypeConsultConference             -- 14
    | CallTypeConference                    -- 15
    | CallTypeUnmonitored                   -- 16
    | CallTypePreview                       -- 17
    | CallTypeReservation                   -- 18
    | CallTypeAssist                        -- 19
    | CallTypeEmergency                     -- 20
    | CallTypeSupervisorMonitor             -- 21
    | CallTypeSupervisorWhisper             -- 22
    | CallTypeSupervisorBargeIn             -- 23
    | CallTypeSupervisorIntercept           -- 24
    | CallTypeTaskRoutedByIcm               -- 25
    | CallTypeTaskRoutedByApplication       -- 26
    | CallTypeReservationPreview            -- 27
    | CallTypeReservationPreviewDirect      -- 28
    | CallTypeReservationPredictive         -- 29
    | CallTypeReservationCallback           -- 30
    | CallTypeReservationPersonalCallback   -- 31
    | CallTypeCustomerPreview               -- 32
    | CallTypeCustomerPreviewDirect         -- 33
    | CallTypeCustomerPredictive            -- 34
    | CallTypeCustomerCallback              -- 35
    | CallTypeCustomerPersonalCallback      -- 36
    | CallTypeCustomerIvr                   -- 37
    | CallTypeNonAcd                        -- 38
    | CallTypePlayAgentGreeting             -- 39
    | CallTypeRecordAgantGreeting           -- 40
    | CallTypeVoiceCallBack                 -- 41
    deriving (Bounded, Enum, Eq, Generic, Show)

instance Binary CallType where
    get = getEnum16
    put = putEnum16

instance Binary (CtiOpt CallType) where
    get = getOptEnum16
    put = putCtiOpt16

instance Binary (SafeEnum (CtiOpt CallType)) where
    get = getSafeOptEnum16
    put = putSafeEnum


data ClientEventReportReqState
    = ClientEventReportReqStateNormal    -- 0
    | ClientEventReportReqStateWarning   -- 1
    | ClientEventReportReqStateError     -- 2
    deriving (Bounded, Enum, Eq, Generic, Show)


instance Binary ClientEventReportReqState where
    get = getEnum16
    put = putEnum16

instance Binary (SafeEnum ClientEventReportReqState) where
    get = getSafeEnum16
    put = putSafeEnum


data ConnectionDeviceIDType
    = ConnectionDevIDStatic     -- 0
    | ConnectionDevIDDynamic    -- 1
    deriving (Bounded, Enum, Eq, Generic, Show)

instance Binary ConnectionDeviceIDType where
    get = getEnum16
    put = putEnum16

instance Binary (CtiOpt ConnectionDeviceIDType) where
    get = getOptEnum16
    put = putCtiOpt16

instance Binary (SafeEnum (CtiOpt ConnectionDeviceIDType)) where
    get = getSafeOptEnum16
    put = putSafeEnum


data ControlFailureCode
    = CFGenericUnspecified                          --    0
    | CFGenericOperation                            --    1
    | CFRequestIncompatibleWithObject               --    2
    | CFValueOutOfRange                             --    3
    | CFObjectNotKnown                              --    4
    | CFInvalidCallingDevice                        --    5
    | CFInvalidCalledDevice                         --    6
    | CFInvalidForwardingDestination                --    7
    | CFPrivilegeViolationOnSpecifiedDevice         --    8
    | CFPrivilegeViolationOnCalledDevice            --    9
    | CFPrivilegeViolationOnCallingDevice           --   10
    | CFInvalidCstaCallIdentifier                   --   11
    | CFInvalidCstaDeviceIdentifier                 --   12
    | CFInvalidCstaConnectionIdentifier             --   13
    | CFInvalidDestination                          --   14
    | CFInvalidFeature                              --   15
    | CFInvalidAllocationState                      --   16
    | CFInvalidCrossRefID                           --   17
    | CFInvalidObjectType                           --   18
    | CFSecurityViolation                           --   19
    | CFReserved20                                  --   20
    | CFGenericStateIncompatibility                 --   21
    | CFInvalidObjectState                          --   22
    | CFInvalidConnectionIDForActiveCall            --   23
    | CFNoActiveCall                                --   24
    | CFNoHeldCall                                  --   25
    | CFNoCallToClear                               --   26
    | CFNoConnectionToClear                         --   27
    | CFNoCallToAnswer                              --   28
    | CFNoCallToComplete                            --   29
    | CFReserved30                                  --   30
    | CFGenericSystemResourceAvailability           --   31
    | CFServiceBusy                                 --   32
    | CFResourceBusy                                --   33
    | CFResourceOutOfService                        --   34
    | CFNetworkBusy                                 --   35
    | CFNetworkOutOfService                         --   36
    | CFOverallMonitorLimitExceeded                 --   37
    | CFConferenceMemberLimitExceeded               --   38
    | CFReserved39                                  --   39
    | CFReserved40                                  --   40
    | CFGenericSubscribedResourceAvailability       --   41
    | CFObjectMonitorLimitExceeded                  --   42
    | CFExternalTrunkLimitExceeded                  --   43
    | CFOutstandingRequestLimitExceeded             --   44
    | CFReserved45                                  --   45
    | CFReserved46                                  --   46
    | CFReserved47                                  --   47
    | CFReserved48                                  --   48
    | CFReserved49                                  --   49
    | CFReserved50                                  --   50
    | CFGenericPerformanceManagement                --   51
    | CFGenericPerformanceExceeded                  --   52
    | CFReserved53                                  --   53
    | CFReserved54                                  --   54
    | CFReserved55                                  --   55
    | CFReserved56                                  --   56
    | CFReserved57                                  --   57
    | CFReserved58                                  --   58
    | CFReserved59                                  --   59
    | CFReserved60                                  --   60
    | CFSequenceNumberViolated                      --   61
    | CFTimeStampViolated                           --   62
    | CFPacViolated                                 --   63
    | CFSealViolated                                --   64
    | CFReserved65                                  --   65
    | CFReserved66                                  --   66
    | CFReserved67                                  --   67
    | CFReserved68                                  --   68
    | CFReserved69                                  --   69
    | CFGenericUnspecifiedRejection                 --   70
    | CFGenericOperationRejection                   --   71
    | CFDuplicateInvocationRejection                --   72
    | CFUnrecognizedOperationRejection              --   73
    | CFMistypedArgumentRejection                   --   74
    | CFResourceLimitationRejection                 --   75
    | CFAcsHandleTerminationRejection               --   76
    | CFServiceTerminationRejection                 --   77
    | CFRequestTimeoutRejection                     --   78
    | CFRequestsOnDeviceExceededRejection           --   79
    | CFInvalidAgentIDSpecified                     --  256
    | CFInvalidPasswordSpecified                    --  257
    | CFInvalidAgentIDOrPasswordSpecified           --  258
    | CFSpecifiedAgentAlreadySignedOn               --  259
    | CFInvalidLogonDeviceSpecified                 --  260
    | CFInvalidAnsweringDeviceSpecified             --  261
    | CFInvalidSkillGroupSpecified                  --  262
    | CFInvalidClassOfServiceSpecified              --  263
    | CFInvalidTermSpecified                        --  264
    | CFInvalidAgentWorkMode                        --  265
    | CFInvalidAgentReasonCode                      --  266
    | CFAdjunctSwitchCommError                      --  267
    | CFAgentNotPartyOnCall                         --  268
    | CFInternalProcessingError                     --  269
    | CFTakeCallControlRejection                    --  270
    | CFTakeDomainControlRejection                  --  271
    | CFRequestedServiceNotRegistered               --  272
    | CFInvalidConsultType                          --  273
    | CFAnsmapOrAdparamFieldNotValid                --  274
    | CFInvalidCallControlTableSpecified            --  275
    | CFInvalidDigitsRnaTimeoutAmsDelayOrCountry    --  276
    | CFAnswerDetectPortUnavailable                 --  277
    | CFVirtualAgentUnavailable                     --  278
    | CFTakebackNXferRouteEnd                       --  279
    | CFWrapupDataRequired                          --  280
    | CFReasonCodeRequired                          --  281
    | CFInvalidTrunkIDSpecified                     --  282
    | CFSpecifiedExtensionAlreadyInUse              --  283
    | CFArbitraryConfOrXferNotSupported             --  284
    | CFNetworkTransferOrConsult                    --  285
    | CFNetworkTransferOrConsultFailed              --  286
    | CFDeviceRestricted                            --  287
    | CFLineRestricted                              --  288
    | CFAgentAccountLockedOut                       --  289
    | CFDropAnyPartyNotEnabledCti                   --  290
    | CFMaximumLineLimitExceeded                    --  291
    | CFSharedLinesNotSupported                     --  292
    | CFExtensionNotUnique                          --  293
    | CFUnknownInterfaceControllerID                -- 1001
    | CFInvalidInterfaceControllerID                -- 1002
    | CFSoftwareRevisionNoSupported                 -- 1003
    | CFUnknownPid                                  -- 1004
    | CFInvalidTableSpecified                       -- 1005
    | CFPDServiceInactive                           -- 1006
    | CFUnknownRoutingClientID                      -- 1007
    | CFRCServiceInactive                           -- 1008
    | CFInvalidDialedNumber                         -- 1009
    | CFInvalidParameter                            -- 1010
    | CFUnknownRoutingProblem                       -- 1011
    | CFUnsupportedPDMessageRevision                -- 1012
    | CFUnsupportedRCMessageRevision                -- 1013
    | CFUnsupportedIDMessageRevision                -- 1014
    | CFRCServiceInactivatePim                      -- 1015
    | CFAgentGreetingControlOperationFailure        -- 1016
    deriving (Eq, Generic, Show)

toControlFailureCodeError :: Show i => i -> String
toControlFailureCodeError i = show i <> " is not a member of ControlFailureCode"

instance Enum ControlFailureCode where
    toEnum    0 = CFGenericUnspecified
    toEnum    1 = CFGenericOperation
    toEnum    2 = CFRequestIncompatibleWithObject
    toEnum    3 = CFValueOutOfRange
    toEnum    4 = CFObjectNotKnown
    toEnum    5 = CFInvalidCallingDevice
    toEnum    6 = CFInvalidCalledDevice
    toEnum    7 = CFInvalidForwardingDestination
    toEnum    8 = CFPrivilegeViolationOnSpecifiedDevice
    toEnum    9 = CFPrivilegeViolationOnCalledDevice
    toEnum   10 = CFPrivilegeViolationOnCallingDevice
    toEnum   11 = CFInvalidCstaCallIdentifier
    toEnum   12 = CFInvalidCstaDeviceIdentifier
    toEnum   13 = CFInvalidCstaConnectionIdentifier
    toEnum   14 = CFInvalidDestination
    toEnum   15 = CFInvalidFeature
    toEnum   16 = CFInvalidAllocationState
    toEnum   17 = CFInvalidCrossRefID
    toEnum   18 = CFInvalidObjectType
    toEnum   19 = CFSecurityViolation
    toEnum   20 = CFReserved20
    toEnum   21 = CFGenericStateIncompatibility
    toEnum   22 = CFInvalidObjectState
    toEnum   23 = CFInvalidConnectionIDForActiveCall
    toEnum   24 = CFNoActiveCall
    toEnum   25 = CFNoHeldCall
    toEnum   26 = CFNoCallToClear
    toEnum   27 = CFNoConnectionToClear
    toEnum   28 = CFNoCallToAnswer
    toEnum   29 = CFNoCallToComplete
    toEnum   30 = CFReserved30
    toEnum   31 = CFGenericSystemResourceAvailability
    toEnum   32 = CFServiceBusy
    toEnum   33 = CFResourceBusy
    toEnum   34 = CFResourceOutOfService
    toEnum   35 = CFNetworkBusy
    toEnum   36 = CFNetworkOutOfService
    toEnum   37 = CFOverallMonitorLimitExceeded
    toEnum   38 = CFConferenceMemberLimitExceeded
    toEnum   39 = CFReserved39
    toEnum   40 = CFReserved40
    toEnum   41 = CFGenericSubscribedResourceAvailability
    toEnum   42 = CFObjectMonitorLimitExceeded
    toEnum   43 = CFExternalTrunkLimitExceeded
    toEnum   44 = CFOutstandingRequestLimitExceeded
    toEnum   45 = CFReserved45
    toEnum   46 = CFReserved46
    toEnum   47 = CFReserved47
    toEnum   48 = CFReserved48
    toEnum   49 = CFReserved49
    toEnum   50 = CFReserved50
    toEnum   51 = CFGenericPerformanceManagement
    toEnum   52 = CFGenericPerformanceExceeded
    toEnum   53 = CFReserved53
    toEnum   54 = CFReserved54
    toEnum   55 = CFReserved55
    toEnum   56 = CFReserved56
    toEnum   57 = CFReserved57
    toEnum   58 = CFReserved58
    toEnum   59 = CFReserved59
    toEnum   60 = CFReserved60
    toEnum   61 = CFSequenceNumberViolated
    toEnum   62 = CFTimeStampViolated
    toEnum   63 = CFPacViolated
    toEnum   64 = CFSealViolated
    toEnum   65 = CFReserved65
    toEnum   66 = CFReserved66
    toEnum   67 = CFReserved67
    toEnum   68 = CFReserved68
    toEnum   69 = CFReserved69
    toEnum   70 = CFGenericUnspecifiedRejection
    toEnum   71 = CFGenericOperationRejection
    toEnum   72 = CFDuplicateInvocationRejection
    toEnum   73 = CFUnrecognizedOperationRejection
    toEnum   74 = CFMistypedArgumentRejection
    toEnum   75 = CFResourceLimitationRejection
    toEnum   76 = CFAcsHandleTerminationRejection
    toEnum   77 = CFServiceTerminationRejection
    toEnum   78 = CFRequestTimeoutRejection
    toEnum   79 = CFRequestsOnDeviceExceededRejection
    toEnum  256 = CFInvalidAgentIDSpecified
    toEnum  257 = CFInvalidPasswordSpecified
    toEnum  258 = CFInvalidAgentIDOrPasswordSpecified
    toEnum  259 = CFSpecifiedAgentAlreadySignedOn
    toEnum  260 = CFInvalidLogonDeviceSpecified
    toEnum  261 = CFInvalidAnsweringDeviceSpecified
    toEnum  262 = CFInvalidSkillGroupSpecified
    toEnum  263 = CFInvalidClassOfServiceSpecified
    toEnum  264 = CFInvalidTermSpecified
    toEnum  265 = CFInvalidAgentWorkMode
    toEnum  266 = CFInvalidAgentReasonCode
    toEnum  267 = CFAdjunctSwitchCommError
    toEnum  268 = CFAgentNotPartyOnCall
    toEnum  269 = CFInternalProcessingError
    toEnum  270 = CFTakeCallControlRejection
    toEnum  271 = CFTakeDomainControlRejection
    toEnum  272 = CFRequestedServiceNotRegistered
    toEnum  273 = CFInvalidConsultType
    toEnum  274 = CFAnsmapOrAdparamFieldNotValid
    toEnum  275 = CFInvalidCallControlTableSpecified
    toEnum  276 = CFInvalidDigitsRnaTimeoutAmsDelayOrCountry
    toEnum  277 = CFAnswerDetectPortUnavailable
    toEnum  278 = CFVirtualAgentUnavailable
    toEnum  279 = CFTakebackNXferRouteEnd
    toEnum  280 = CFWrapupDataRequired
    toEnum  281 = CFReasonCodeRequired
    toEnum  282 = CFInvalidTrunkIDSpecified
    toEnum  283 = CFSpecifiedExtensionAlreadyInUse
    toEnum  284 = CFArbitraryConfOrXferNotSupported
    toEnum  285 = CFNetworkTransferOrConsult
    toEnum  286 = CFNetworkTransferOrConsultFailed
    toEnum  287 = CFDeviceRestricted
    toEnum  288 = CFLineRestricted
    toEnum  289 = CFAgentAccountLockedOut
    toEnum  290 = CFDropAnyPartyNotEnabledCti
    toEnum  291 = CFMaximumLineLimitExceeded
    toEnum  292 = CFSharedLinesNotSupported
    toEnum  293 = CFExtensionNotUnique
    toEnum 1001 = CFUnknownInterfaceControllerID
    toEnum 1002 = CFInvalidInterfaceControllerID
    toEnum 1003 = CFSoftwareRevisionNoSupported
    toEnum 1004 = CFUnknownPid
    toEnum 1005 = CFInvalidTableSpecified
    toEnum 1006 = CFPDServiceInactive
    toEnum 1007 = CFUnknownRoutingClientID
    toEnum 1008 = CFRCServiceInactive
    toEnum 1009 = CFInvalidDialedNumber
    toEnum 1010 = CFInvalidParameter
    toEnum 1011 = CFUnknownRoutingProblem
    toEnum 1012 = CFUnsupportedPDMessageRevision
    toEnum 1013 = CFUnsupportedRCMessageRevision
    toEnum 1014 = CFUnsupportedIDMessageRevision
    toEnum 1015 = CFRCServiceInactivatePim
    toEnum 1016 = CFAgentGreetingControlOperationFailure
    toEnum i  = error $ toControlFailureCodeError i

    fromEnum CFGenericUnspecified                          =    0
    fromEnum CFGenericOperation                            =    1
    fromEnum CFRequestIncompatibleWithObject               =    2
    fromEnum CFValueOutOfRange                             =    3
    fromEnum CFObjectNotKnown                              =    4
    fromEnum CFInvalidCallingDevice                        =    5
    fromEnum CFInvalidCalledDevice                         =    6
    fromEnum CFInvalidForwardingDestination                =    7
    fromEnum CFPrivilegeViolationOnSpecifiedDevice         =    8
    fromEnum CFPrivilegeViolationOnCalledDevice            =    9
    fromEnum CFPrivilegeViolationOnCallingDevice           =   10
    fromEnum CFInvalidCstaCallIdentifier                   =   11
    fromEnum CFInvalidCstaDeviceIdentifier                 =   12
    fromEnum CFInvalidCstaConnectionIdentifier             =   13
    fromEnum CFInvalidDestination                          =   14
    fromEnum CFInvalidFeature                              =   15
    fromEnum CFInvalidAllocationState                      =   16
    fromEnum CFInvalidCrossRefID                           =   17
    fromEnum CFInvalidObjectType                           =   18
    fromEnum CFSecurityViolation                           =   19
    fromEnum CFReserved20                                  =   20
    fromEnum CFGenericStateIncompatibility                 =   21
    fromEnum CFInvalidObjectState                          =   22
    fromEnum CFInvalidConnectionIDForActiveCall            =   23
    fromEnum CFNoActiveCall                                =   24
    fromEnum CFNoHeldCall                                  =   25
    fromEnum CFNoCallToClear                               =   26
    fromEnum CFNoConnectionToClear                         =   27
    fromEnum CFNoCallToAnswer                              =   28
    fromEnum CFNoCallToComplete                            =   29
    fromEnum CFReserved30                                  =   30
    fromEnum CFGenericSystemResourceAvailability           =   31
    fromEnum CFServiceBusy                                 =   32
    fromEnum CFResourceBusy                                =   33
    fromEnum CFResourceOutOfService                        =   34
    fromEnum CFNetworkBusy                                 =   35
    fromEnum CFNetworkOutOfService                         =   36
    fromEnum CFOverallMonitorLimitExceeded                 =   37
    fromEnum CFConferenceMemberLimitExceeded               =   38
    fromEnum CFReserved39                                  =   39
    fromEnum CFReserved40                                  =   40
    fromEnum CFGenericSubscribedResourceAvailability       =   41
    fromEnum CFObjectMonitorLimitExceeded                  =   42
    fromEnum CFExternalTrunkLimitExceeded                  =   43
    fromEnum CFOutstandingRequestLimitExceeded             =   44
    fromEnum CFReserved45                                  =   45
    fromEnum CFReserved46                                  =   46
    fromEnum CFReserved47                                  =   47
    fromEnum CFReserved48                                  =   48
    fromEnum CFReserved49                                  =   49
    fromEnum CFReserved50                                  =   50
    fromEnum CFGenericPerformanceManagement                =   51
    fromEnum CFGenericPerformanceExceeded                  =   52
    fromEnum CFReserved53                                  =   53
    fromEnum CFReserved54                                  =   54
    fromEnum CFReserved55                                  =   55
    fromEnum CFReserved56                                  =   56
    fromEnum CFReserved57                                  =   57
    fromEnum CFReserved58                                  =   58
    fromEnum CFReserved59                                  =   59
    fromEnum CFReserved60                                  =   60
    fromEnum CFSequenceNumberViolated                      =   61
    fromEnum CFTimeStampViolated                           =   62
    fromEnum CFPacViolated                                 =   63
    fromEnum CFSealViolated                                =   64
    fromEnum CFReserved65                                  =   65
    fromEnum CFReserved66                                  =   66
    fromEnum CFReserved67                                  =   67
    fromEnum CFReserved68                                  =   68
    fromEnum CFReserved69                                  =   69
    fromEnum CFGenericUnspecifiedRejection                 =   70
    fromEnum CFGenericOperationRejection                   =   71
    fromEnum CFDuplicateInvocationRejection                =   72
    fromEnum CFUnrecognizedOperationRejection              =   73
    fromEnum CFMistypedArgumentRejection                   =   74
    fromEnum CFResourceLimitationRejection                 =   75
    fromEnum CFAcsHandleTerminationRejection               =   76
    fromEnum CFServiceTerminationRejection                 =   77
    fromEnum CFRequestTimeoutRejection                     =   78
    fromEnum CFRequestsOnDeviceExceededRejection           =   79
    fromEnum CFInvalidAgentIDSpecified                     =  256
    fromEnum CFInvalidPasswordSpecified                    =  257
    fromEnum CFInvalidAgentIDOrPasswordSpecified           =  258
    fromEnum CFSpecifiedAgentAlreadySignedOn               =  259
    fromEnum CFInvalidLogonDeviceSpecified                 =  260
    fromEnum CFInvalidAnsweringDeviceSpecified             =  261
    fromEnum CFInvalidSkillGroupSpecified                  =  262
    fromEnum CFInvalidClassOfServiceSpecified              =  263
    fromEnum CFInvalidTermSpecified                        =  264
    fromEnum CFInvalidAgentWorkMode                        =  265
    fromEnum CFInvalidAgentReasonCode                      =  266
    fromEnum CFAdjunctSwitchCommError                      =  267
    fromEnum CFAgentNotPartyOnCall                         =  268
    fromEnum CFInternalProcessingError                     =  269
    fromEnum CFTakeCallControlRejection                    =  270
    fromEnum CFTakeDomainControlRejection                  =  271
    fromEnum CFRequestedServiceNotRegistered               =  272
    fromEnum CFInvalidConsultType                          =  273
    fromEnum CFAnsmapOrAdparamFieldNotValid                =  274
    fromEnum CFInvalidCallControlTableSpecified            =  275
    fromEnum CFInvalidDigitsRnaTimeoutAmsDelayOrCountry    =  276
    fromEnum CFAnswerDetectPortUnavailable                 =  277
    fromEnum CFVirtualAgentUnavailable                     =  278
    fromEnum CFTakebackNXferRouteEnd                       =  279
    fromEnum CFWrapupDataRequired                          =  280
    fromEnum CFReasonCodeRequired                          =  281
    fromEnum CFInvalidTrunkIDSpecified                     =  282
    fromEnum CFSpecifiedExtensionAlreadyInUse              =  283
    fromEnum CFArbitraryConfOrXferNotSupported             =  284
    fromEnum CFNetworkTransferOrConsult                    =  285
    fromEnum CFNetworkTransferOrConsultFailed              =  286
    fromEnum CFDeviceRestricted                            =  287
    fromEnum CFLineRestricted                              =  288
    fromEnum CFAgentAccountLockedOut                       =  289
    fromEnum CFDropAnyPartyNotEnabledCti                   =  290
    fromEnum CFMaximumLineLimitExceeded                    =  291
    fromEnum CFSharedLinesNotSupported                     =  292
    fromEnum CFExtensionNotUnique                          =  293
    fromEnum CFUnknownInterfaceControllerID                = 1001
    fromEnum CFInvalidInterfaceControllerID                = 1002
    fromEnum CFSoftwareRevisionNoSupported                 = 1003
    fromEnum CFUnknownPid                                  = 1004
    fromEnum CFInvalidTableSpecified                       = 1005
    fromEnum CFPDServiceInactive                           = 1006
    fromEnum CFUnknownRoutingClientID                      = 1007
    fromEnum CFRCServiceInactive                           = 1008
    fromEnum CFInvalidDialedNumber                         = 1009
    fromEnum CFInvalidParameter                            = 1010
    fromEnum CFUnknownRoutingProblem                       = 1011
    fromEnum CFUnsupportedPDMessageRevision                = 1012
    fromEnum CFUnsupportedRCMessageRevision                = 1013
    fromEnum CFUnsupportedIDMessageRevision                = 1014
    fromEnum CFRCServiceInactivatePim                      = 1015
    fromEnum CFAgentGreetingControlOperationFailure        = 1016

instance Binary ControlFailureCode where
    get = do
        w <- getWord16be
        case w of n |    0 <= n && n <=   79 -> nToCFC w
                    |  256 <= n && n <=  293 -> nToCFC w
                    | 1001 <= n && n <= 1016 -> nToCFC w
                    | otherwise              -> fail $ toControlFailureCodeError w
                    where nToCFC = return . toEnum . fromIntegral
    put = putEnum16

instance Binary (SafeEnum ControlFailureCode) where
    get = do
        w <- getWord16be
        case w of n |    0 <= n && n <=   79 -> nToSafeCFC w
                    |  256 <= n && n <=  293 -> nToSafeCFC w
                    | 1001 <= n && n <= 1016 -> nToSafeCFC w
                    | otherwise              -> return . SafeEnum . Left $ toControlFailureCodeError w
                    where nToSafeCFC = return . SafeEnum . Right . toEnum . fromIntegral
    put = putSafeEnum


data CtiBool
    = CtiBoolFalse  -- 0
    | CtiBoolTrue   -- 1
    deriving (Bounded, Enum, Eq, Generic, Show)

instance Binary CtiBool where
    get = getEnum16
    put = putEnum16

instance Binary (SafeEnum CtiBool) where
    get = getSafeEnum16
    put = putSafeEnum


data DeviceIDType
    = DevIDDevice       --  0
    | DevIDTrunk        -- 70
    | DevIDTrunkGroup   -- 71
    | DevIDIPPhoneMac   -- 72
    | DevIDCtiPort      -- 73
    | DevIDRoutePoint   -- 74
    | DevIDExternal     -- 75
    | DevIDAgentDevice  -- 76
    | DevIDQueue        -- 77
    | DevIDNonAcdDevice -- 78
    | DevIDSharedDevice -- 79
    deriving (Eq, Generic, Show)

toDeviceIDTypeError :: Show i => i -> String
toDeviceIDTypeError i = show i <> " is not a member of DeviceIDType"

instance Enum DeviceIDType where
    toEnum  0 = DevIDDevice
    toEnum 70 = DevIDTrunk
    toEnum 71 = DevIDTrunkGroup
    toEnum 72 = DevIDIPPhoneMac
    toEnum 73 = DevIDCtiPort
    toEnum 74 = DevIDRoutePoint
    toEnum 75 = DevIDExternal
    toEnum 76 = DevIDAgentDevice
    toEnum 77 = DevIDQueue
    toEnum 78 = DevIDNonAcdDevice
    toEnum 79 = DevIDSharedDevice
    toEnum i  = error $ toDeviceIDTypeError i

    fromEnum DevIDDevice       =  0
    fromEnum DevIDTrunk        = 70
    fromEnum DevIDTrunkGroup   = 71
    fromEnum DevIDIPPhoneMac   = 72
    fromEnum DevIDCtiPort      = 73
    fromEnum DevIDRoutePoint   = 74
    fromEnum DevIDExternal     = 75
    fromEnum DevIDAgentDevice  = 76
    fromEnum DevIDQueue        = 77
    fromEnum DevIDNonAcdDevice = 78
    fromEnum DevIDSharedDevice = 79

instance Binary DeviceIDType where
    get = do
        w <- getWord16be
        case w of n | n == 0 || 70 <= n && n <= 79 -> return . toEnum $ fromIntegral w
                    | otherwise                    -> fail $ toDeviceIDTypeError w
    put = putEnum16

instance Binary (CtiOpt DeviceIDType) where
    get = do
        w <- getWord16be
        case w of n | n == 0 || 70 <= n && n <= 79 -> return . CtiOpt . Just . toEnum $ fromIntegral w
                    | n == (maxBound `asTypeOf` n) -> return $ CtiOpt Nothing
                    | otherwise                    -> fail $ toDeviceIDTypeError w
    put = putCtiOpt16

instance Binary (SafeEnum (CtiOpt DeviceIDType)) where
    get = do
        w <- getWord16be
        let r = case w of
                    n  | n == 0 || 70 <= n && n <= 79 -> SafeEnum . Right . CtiOpt . Just . toEnum $ fromIntegral w
                       | n == (maxBound `asTypeOf` n) -> SafeEnum . Right $ CtiOpt Nothing
                       | otherwise                    -> SafeEnum . Left $ toDeviceIDTypeError w
        return r
    put = putSafeEnum


data DeviceType
    = DeviceTypeUnknown                 -- 0
    | DeviceTypeService                 -- 1
    | DeviceTypeSkillGroup              -- 2
    | DeviceTypeAgentID                 -- 3
    | DeviceTypeAgentDeviceExtension    -- 4
    | DeviceTypeRoutePoint              -- 5
    | DeviceTypeCtiPort                 -- 6
    | DeviceTypeCallControlGroup        -- 7
    deriving (Bounded, Enum, Eq, Generic, Show)

instance Binary DeviceType where
    get = getEnum16
    put = putEnum16

instance Binary (SafeEnum DeviceType) where
    get = getSafeEnum16
    put = putSafeEnum


data Direction
    = DirectionNone                 -- 0
    | DirectionIn                   -- 1
    | DirectionOut                  -- 2
    | DirectionOtherIn              -- 3
    | DirectionOtherOut             -- 4
    | DirectionOutboundReserve      -- 5
    | DirectionOutboundPreview      -- 6
    | DirectionOutboundPredictive   -- 7
    deriving (Bounded, Enum, Eq, Generic, Show)

instance Binary Direction where
    get = getEnum32
    put = putEnum32

instance Binary (SafeEnum Direction) where
    get = getSafeEnum32
    put = putSafeEnum


data Disposition
    = DispositionZero                               --  0
    | DispositionAbandonedInNetwork                 --  1
    | DispositionAbandonedInLocalQueue              --  2
    | DispositionAbandonedRing                      --  3
    | DispositionAbandonedDelay                     --  4
    | DispositionAbandonedInterFlow                 --  5
    | DispositionAbandonedAgentTerminal             --  6
    | DispositionShort                              --  7
    | DispositionBusy                               --  8
    | DispositionForcedBusy                         --  9
    | DispositionDisconnectDropNoAnswer             -- 10
    | DispositionDisconnectDropBusy                 -- 11
    | DispositionDisconnectDropReorder              -- 12
    | DispositionDisconnectDropHandledPrimaryRoute  -- 13
    | DispositionDisconnectDropHandledOther         -- 14
    | DispositionRedirected                         -- 15
    | DispositionCutThrough                         -- 16
    | DispositionIntraFlow                          -- 17
    | DispositionInterFlow                          -- 18
    | DispositionRingNoAnswer                       -- 19
    | DispositionInterceptReorder                   -- 20
    | DispositionInterceptDenial                    -- 21
    | DispositionTimeOut                            -- 22
    | DispositionVoiceEnergy                        -- 23
    | DispositionNonClassifiedEnergyDetected        -- 24
    | DispositionNoCutThrough                       -- 25
    | DispositionUAbort                             -- 26
    | DispositionFailedSoftware                     -- 27
    | DispositionBlindTransfer                      -- 28
    | DispositionAnnouncedTransfer                  -- 29
    | DispositionConferenced                        -- 30
    | DispositionDuplicateTransfer                  -- 31
    | DispositionUnmonitoredDevice                  -- 32
    | DispositionAnsweringMachine                   -- 33
    | DispositionNetworkBlindTransfer               -- 34
    | DispositionTaskAbandonedInRouter              -- 35
    | DispositionTaskAbandonedBeforeOffered         -- 36
    | DispositionTaskAbandonedWhileOffered          -- 37
    | DispositionNormalEndTask                      -- 38
    | DispositionCantObtainTaskID                   -- 39
    | DispositionAgentLoggedOutDuringTask           -- 40
    | DispositionMaximumTaskLifetimeExceeded        -- 41
    | DispositionApplicationPathWentDown            -- 42
    | DispositionUnifiedCceRoutingComplete          -- 43
    | DispositionUnifiedCceRoutingDisabled          -- 44
    | DispositionApplicationInvalidMrdID            -- 45
    | DispositionApplicationInvalidDialogueID       -- 46
    | DispositionApplicationDuplicateDialogueID     -- 47
    | DispositionApplicationInvalidInvokeID         -- 48
    | DispositionApplicationInvalidScriptSelector   -- 49
    | DispositionApplicationTerminateDialogue       -- 50
    | DispositionTaskEnedDuringApplicationInit      -- 51
    | DispositionCalledPartyDisconnected            -- 52
    | DispositionPartialCall                        -- 53
    | DispositionDropnetworkConsult                 -- 54
    | DispositionNetworkConsultTransfer             -- 55
    | DispositionReserved56                         -- 56
    | DispositionAbandonNetworkConsult              -- 57
    | DispositionRouterRequeryBeforeAnswer          -- 58
    | DispositionRouterRequeryAfterAnswer           -- 59
    | DispositionNetworkError                       -- 60
    | DispositionNetworkErrorBeforeAnswer           -- 61
    | DispositionNetworkErrorAfterAnswer            -- 62
    | DispositionTaskTransfer                       -- 63
    | DispositionApplicationDisconnected            -- 64
    | DispositionTaskTransferredOnAgentLogout       -- 65
    deriving (Bounded, Enum, Eq, Generic, Show)

instance Binary Disposition where
    get = getEnum16
    put = putEnum16

instance Binary (SafeEnum Disposition) where
    get = getSafeEnum16
    put = putSafeEnum


data EventCause
    = EventCauseZero                    --  0
    | EventCauseActiveMonitor           --  1
    | EventCauseAlternate               --  2
    | EventCauseBusy                    --  3
    | EventCauseCallBack                --  4
    | EventCauseCallCancelled           --  5
    | EventCauseCallForwardAlways       --  6
    | EventCauseCallForwardBusy         --  7
    | EventCauseCallForwardNoAnswer     --  8
    | EventCauseCallForward             --  9
    | EventCauseCallNotAnswered         -- 10
    | EventCauseCallPickup              -- 11
    | EventCauseCampOn                  -- 12
    | EventCauseDestNotObtainable       -- 13
    | EventCauseDoNotDisturb            -- 14
    | EventCauseIncompatibleDestination -- 15
    | EventCauseInvalidAccountCode      -- 16
    | EventCauseKeyConference           -- 17
    | EventCauseLockout                 -- 18
    | EventCauseMaintenance             -- 19
    | EventCauseNetworkCongestion       -- 20
    | EventCauseNetworkNotObtainable    -- 21
    | EventCauseNewCall                 -- 22
    | EventCauseNoAvailableAgents       -- 23
    | EventCauseOverride                -- 24
    | EventCausePark                    -- 25
    | EventCauseOverflow                -- 26
    | EventCauseRecall                  -- 27
    | EventCauseRedirected              -- 28
    | EventCauseReorderTone             -- 29
    | EventCauseResourcesNotAvailable   -- 30
    | EventCauseSilentMonitor           -- 31
    | EventCauseTransfer                -- 32
    | EventCauseTrunksBusy              -- 33
    | EventCauseVoiceUnitInitiator      -- 34
    | EventCauseTimeOut                 -- 35
    | EventCauseNewCallInterflow        -- 36
    | EventCauseSimulationInitRequest   -- 37
    | EventCauseSimulationResetRequest  -- 38
    | EventCauseCtiLinkDown             -- 39
    | EventCausePeripheralResetRequest  -- 40
    | EventCauseMD110ConferenceTransfer -- 41
    | EventCauseRemainsInQ              -- 42
    | EventCauseSupervisorAssist        -- 43
    | EventCauseEmergencyCall           -- 44
    | EventCauseSupervisorClear         -- 45
    | EventCauseSupervisorMonitor       -- 46
    | EventCauseSupervisorWhisper       -- 47
    | EventCauseSupervisorBargeIn       -- 48
    | EventCauseSupervisorIntercept     -- 49
    | EventCauseCallPartyUpdateInd      -- 50
    | EventCauseConsult                 -- 51
    | EventCauseNicCallClear            -- 52
    deriving (Bounded, Enum, Eq, Generic, Show)

instance Binary EventCause where
    get = getEnum16
    put = putEnum16

instance Binary (CtiOpt EventCause) where
    get = getOptEnum16
    put = putCtiOpt16

instance Binary (SafeEnum (CtiOpt EventCause)) where
    get = getSafeOptEnum16
    put = putSafeEnum


data FailureIndicationMessageStatusCode
    = ECtiNoError                               --   0
    | ECtiInvalidVersion                        --   1
    | ECtiInvalidMessageType                    --   2
    | ECtiInvalidFieldTag                       --   3
    | ECtiSessionNotOpen                        --   4
    | ECtiSessionAlreadyOpen                    --   5
    | ECtiRequiredDataMissing                   --   6
    | ECtiInvalidPeripheralID                   --   7
    | ECtiInvalidAgentData                      --   8
    | ECtiAgentNotLoggedOn                      --   9
    | ECtiDeviceInUse                           --  10
    | ECtiNewSessionOpened                      --  11
    | ECtiFunctionNotAvailable                  --  12
    | ECtiInvalidCallID                         --  13
    | ECtiProtectedVariable                     --  14
    | ECtiCtiServerOffline                      --  15
    | ECtiTimeout                               --  16
    | ECtiUnspecifiedFailure                    --  17
    | ECtiInvalidTimeout                        --  18
    | ECtiInvalidServiceMask                    --  19
    | ECtiInvalidCallMsgMask                    --  20
    | ECtiInvalidAgentStateMask                 --  21
    | ECtiInvalidReservedField                  --  22
    | ECtiInvalidFieldLength                    --  23
    | ECtiInvalidDigits                         --  24
    | ECtiBadMessageFormat                      --  25
    | ECtiInvalidTagForMsgType                  --  26
    | ECtiInvalidDeviceIDType                   --  27
    | ECtiInvalidLclConnState                   --  28
    | ECtiInvalidEventCause                     --  29
    | ECtiInvalidNumParties                     --  30
    | ECtiInvalidSysEventID                     --  31
    | ECtiInconsistentAgentData                 --  32
    | ECtiInvalidConnectionIDType               --  33
    | ECtiInvalidCallType                       --  34
    | ECtiNotCallParty                          --  35
    | ECtiInvalidPassword                       --  36
    | ECtiClientDisconnected                    --  37
    | ECtiInvalidObjectState                    --  38
    | ECtiInvalidNumSkillGroups                 --  39
    | ECtiInvalidNumLines                       --  40
    | ECtiInvalidLineType                       --  41
    | ECtiInvalidAllocationState                --  42
    | ECtiInvalidAnsweringMachine               --  43
    | ECtiInvalidCallMannerType                 --  44
    | ECtiInvalidCallPlacementType              --  45
    | ECtiInvalidConsultType                    --  46
    | ECtiInvalidFacilityType                   --  47
    | ECtiInvalidMsgTypeForVersion              --  48
    | ECtiInvalidTagForVersion                  --  49
    | ECtiInvalidAgentWorkMode                  --  50
    | ECtiInvalidCallOption                     --  51
    | ECtiInvalidDestinationCountry             --  52
    | ECtiInvalidAnswerDetectMode               --  53
    | ECtiMutuallyExclusDeviceIDTypes           --  54
    | ECtiInvalidMonitorID                      --  55
    | ECtiSessionMonitorAlreadyExists           --  56
    | ECtiSessionMonitorIsClient                --  57
    | ECtiInvalidCallControlMask                --  58
    | ECtiInvalidFeatureMask                    --  59
    | ECtiInvalidTransferConferenceSetupMask    --  60
    | ECtiInvalidArrayIndex                     --  61
    | ECtiInvalidCharacter                      --  62
    | ECtiClientNotFound                        --  63
    | ECtiSupervisorNotFound                    --  64
    | ECtiTeamNotFound                          --  65
    | ECtiNoCallActive                          --  66
    | ECtiNamedVariableNotConfigured            --  67
    | ECtiNamedArrayNotConfigured               --  68
    | ECtiInvalidCallVariableMask               --  69
    | ECtiElementNotFound                       --  70
    | ECtiInvalidDistributionType               --  71
    | ECtiInvalidSkillGroup                     --  72
    | ECtiTooMuchData                           --  73
    | ECtiValueTooLong                          --  74
    | ECtiScalarFunctionOnArray                 --  75
    | ECtiArrayFunctionOnScalar                 --  76
    | ECtiInvalidNumNamedVariables              --  77
    | ECtiInvalidNumNamedArrays                 --  78
    | ECtiInvalidRtpDirection                   --  79
    | ECtiInvalidRtpType                        --  80
    | ECtiCalledPartyDisposition                --  81
    | ECtiInvalidSupervisoryAction              --  82
    | ECtiAgentTeamMonitorAlreadyExists         --  83
    | ECtiInvalidService                        --  84
    | ECtiServiceConflict                       --  85
    | ECtiSkillGroupConflict                    --  86
    | ECtiInvalidDevice                         --  87
    | ECtiInvalidMrDomain                       --  88
    | ECtiMonitorAlreadyExists                  --  89
    | ECtiMonitorTerminated                     --  90
    | ECtiInvalidTaskMsgMask                    --  91
    | ECtiServerNotMaster                       --  92
    | ECtiInvalidCsd                            --  93
    | ECtiJtapiCcmProblem                       --  94
    | ECtiInvalidConfigMsgMask                  --  95
    | ECtiAutoConfigReset                       --  96
    | ECtiInvalidMonitorStatus                  --  97
    | ECtiInvalidRequestType                    --  98
    deriving (Bounded, Enum, Eq, Generic, Show)

instance Binary FailureIndicationMessageStatusCode where
    get = getEnum32
    put = putEnum32

instance Binary (SafeEnum FailureIndicationMessageStatusCode) where
    get = getSafeEnum32
    put = putSafeEnum


data ForcedFlag
    = ForcedFlagFalse                   -- 0
    | ForcedFlagTrue                    -- 1
    | ForcedFlagAgentAuthenticationOnly -- 2
    deriving (Bounded, Enum, Eq, Generic, Show)

instance Binary ForcedFlag where
    get = getEnum8
    put = putEnum8

instance Binary (SafeEnum ForcedFlag) where
    get = getSafeEnum8
    put = putSafeEnum


data LineType
    = LineTypeInboundAcd    --  0
    | LineTypeOutboundAcd   --  1
    | LineTypeInside        --  2
    | LineTypeUnknown       --  3
    | LineTypeSupervisor    --  4
    | LineTypeMessage       --  5
    | LineTypeHelp          --  6
    | LineTypeOutbound      --  7
    | LineTypeDid           --  8
    | LineTypeSilentMonitor --  9
    | LineTypeNonAcdIn      -- 10
    | LineTypeNonAcdOut     -- 11
    deriving (Bounded, Enum, Eq, Generic, Show)

instance Binary LineType where
    get = getEnum16
    put = putEnum16

instance Binary (CtiOpt LineType) where
    get = getOptEnum16
    put = putCtiOpt16

instance Binary (SafeEnum (CtiOpt LineType)) where
    get = getSafeOptEnum16
    put = putSafeEnum


data LocalConnectionState
    = LocalConnectionStateNull      -- 0
    | LocalConnectionStateInitiate  -- 1
    | LocalConnectionStateAlerting  -- 2
    | LocalConnectionStateConnect   -- 3
    | LocalConnectionStateHold      -- 4
    | LocalConnectionStateQueued    -- 5
    | LocalConnectionStateFail      -- 6
    deriving (Bounded, Enum, Eq, Generic, Show)

instance Binary LocalConnectionState where
    get = getEnum16
    put = putEnum16

instance Binary (CtiOpt LocalConnectionState) where
    get = getOptEnum16
    put = putCtiOpt16

instance Binary (SafeEnum (CtiOpt LocalConnectionState)) where
    get = getSafeOptEnum16
    put = putSafeEnum


data MultilineAgentConrol
    = MultilineAgentControlSingleLineOnly   -- 0
    | MultilineAgentControlMultilineEnabled -- 1
    deriving (Bounded, Enum, Eq, Generic, Show)

instance Binary MultilineAgentConrol where
    get = getEnum16
    put = putEnum16

instance Binary (SafeEnum MultilineAgentConrol) where
    get = getSafeEnum16
    put = putSafeEnum


data PeripheralType
    = PeripheralTypeZero                --  0
    | PeripheralTypeAspect              --  1
    | PeripheralTypeMeridian            --  2
    | PeripheralTypeG2                  --  3
    | PeripheralTypeDefinityEcsNonEas   --  4
    | PeripheralTypeDefinityEcsEas      --  5
    | PeripheralTypeGalaxy              --  6
    | PeripheralTypeSpectrum            --  7
    | PeripheralTypeVru                 --  8
    | PeripheralTypeVruPolled           --  9
    | PeripheralTypeDms100              -- 10
    | PeripheralTypeSiemens9006         -- 11
    | PeripheralTypeSiemens9005         -- 12
    | PeripheralTypeAlcatel             -- 13
    | PeripheralTypeNecNeax2x00         -- 14
    | PeripheralTypeAcp1000             -- 15
    | PeripheralTypeSymposium           -- 16
    | PeripheralTypeEnterpriseAgent     -- 17
    | PeripheralTypeMD100               -- 18
    | PeripheralTypeMediaRouting        -- 19
    | PeripheralTypeGeneric             -- 20
    | PeripheralTypeAcmiCrs             -- 21
    | PeripheralTypeAcmiIpcc            -- 22
    | PeripheralTypeSimplifiedIpcc      -- 23
    | PeripheralTypeArs                 -- 24
    | PeripheralTypeAcmiErs             -- 25
    | PeripheralTypeAcmiExpertAdvisor   -- 26
    | PeripheralTypeReserved27          -- 27
    deriving (Bounded, Enum, Eq, Generic, Show)

instance Binary PeripheralType where
    get = getEnum16
    put = putEnum16

instance Binary (CtiOpt PeripheralType) where
    get = getOptEnum16
    put = putCtiOpt16

instance Binary (SafeEnum (CtiOpt PeripheralType)) where
    get = getSafeOptEnum16
    put = putSafeEnum


data RecordType
    = RecordTypeAdd     -- 0
    | RecordTypeChange  -- 1
    | RecordTypeDelete  -- 2
    deriving (Bounded, Enum, Eq, Generic, Show)

instance Binary RecordType where
    get = getEnum16
    put = putEnum16

instance Binary (SafeEnum RecordType) where
    get = getSafeEnum16
    put = putSafeEnum


data SilentMonitorStatus
    = SilentMonitorStatusNone
    | SilentMonitorStatusInitiator
    | SilentMonitorStatusTarget
    deriving (Bounded, Enum, Eq, Generic, Show)

instance Binary SilentMonitorStatus where
    get = getEnum16
    put = putEnum16

instance Binary (SafeEnum SilentMonitorStatus) where
    get = getSafeEnum16
    put = putSafeEnum

data SsoEnabled
    = SsoDisabled   -- 0
    | SsoEnabled    -- 1
    deriving (Bounded, Enum, Eq, Generic, Show)

instance Binary SsoEnabled where
    get = getEnum16
    put = putEnum16

instance Binary (SafeEnum SsoEnabled) where
    get = getSafeEnum16
    put = putSafeEnum

data SystemEventID
    = SysEventIDZero                --  0
    | SysCentralControllerOnline    --  1
    | SysCentralControllerOffline   --  2
    | SysPeripheralOnline           --  3
    | SysPeripheralOffline          --  4
    | SysTextFyi                    --  5
    | SysPeripheralGatewayOffline   --  6
    | SysCtiServerOffline           --  7
    | SysCtiServerOnline            --  8
    | SysHalfHourChange             --  9
    | SysInstrumentOutOService      -- 10
    | SysInstrumentBackInService    -- 11
    deriving (Bounded, Enum, Eq, Generic, Show)

instance Binary SystemEventID where
    get = getEnum32
    put = putEnum32

instance Binary (SafeEnum SystemEventID) where
    get = getSafeEnum32
    put = putSafeEnum
