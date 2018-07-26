{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Network.CceCti.Types where

import           Prelude              hiding (take, takeWhile)

import           Data.Binary          (Binary, get, put)
import           Data.Binary.Get      (Get, getRemainingLazyByteString,
                                       getWord16be, getWord32be, getWord8)
import           Data.Binary.Put      (Put, putLazyByteString, putWord16be,
                                       putWord32be, putWord8)
import           Data.ByteString.Lazy (ByteString, empty, pack, split, take,
                                       takeWhile)
import           Data.Int             (Int64)
import           Data.Monoid          ((<>))
import           Data.String          (IsString)
import           Data.Typeable        (typeOf)
import           Data.Word            (Word16, Word32, Word8)
import           GHC.Generics         (Generic)


{-
Protocol version must be 20.
Starting from version 20, tag and length field of floating part changed to Word16
where those are Word8 in older versions.  Those types of tag and length are hard coded.
Thus, this code cannot interact with older protocol versions.
-}
newtype VersionNumber = VersionNumber Word32 deriving (Binary, Eq, Show)

protocolVersion :: VersionNumber
protocolVersion = VersionNumber 20


{-
Fixed sized integral type with special value.
Special value means "none" or "not specified" or something like that.
Special value is maximum bounded value of binary format such like 0xffff or 0xffffffff.
-}
newtype CtiOpt a = CtiOpt (Maybe a) deriving (Eq, Generic, Show)

putCtiOpt32 :: Binary a => CtiOpt a -> Put
putCtiOpt32 (CtiOpt (Just a)) = put a
putCtiOpt32 (CtiOpt Nothing)  = putWord32be maxBound

putCtiOpt16 :: Binary a => CtiOpt a -> Put
putCtiOpt16 (CtiOpt (Just a)) = put a
putCtiOpt16 (CtiOpt Nothing)  = putWord16be maxBound

instance Binary (CtiOpt Word32) where
    get = getWord32be >>= \w -> return $ if w == (maxBound `asTypeOf` w) then CtiOpt Nothing else CtiOpt $ Just w
    put = putCtiOpt32

instance Binary (CtiOpt Word16) where
    get = getWord16be >>= \w -> return $ if w == (maxBound `asTypeOf` w) then CtiOpt Nothing else CtiOpt $ Just w
    put = putCtiOpt16

{-
Similar to CtiOpt but special value is 0 (zero) instead of 0xffff or 0xffffffff.
-}
newtype CtiOpt0 a = CtiOpt0 (Maybe a) deriving (Eq, Generic, Show)

putCtiOpt032 :: Binary a => CtiOpt0 a -> Put
putCtiOpt032 (CtiOpt0 (Just a)) = put a
putCtiOpt032 (CtiOpt0 Nothing)  = putWord32be 0

putCtiOpt016 :: Binary a => CtiOpt0 a -> Put
putCtiOpt016 (CtiOpt0 (Just a)) = put a
putCtiOpt016 (CtiOpt0 Nothing)  = putWord16be 0

instance Binary (CtiOpt0 Word32) where
    get = getWord32be >>= \w -> return $ if w == 0 then CtiOpt0 Nothing else CtiOpt0 $ Just w
    put = putCtiOpt032

instance Binary (CtiOpt0 Word16) where
    get = getWord16be >>= \w -> return $ if w == 0 then CtiOpt0 Nothing else CtiOpt0 $ Just w
    put = putCtiOpt016

{-
32bit fixed sized integral types with special value
-}
newtype ConnectionCallID = ConnectionCallID (CtiOpt Word32) deriving (Binary, Eq, Generic, Show)
newtype PeripheralID = PeripheralID (CtiOpt Word32) deriving (Binary, Eq, Generic, Show)
newtype ServiceID = ServiceID (CtiOpt Word32) deriving (Binary, Eq, Generic, Show)
newtype ServiceNumber = ServiceNumber (CtiOpt Word32) deriving (Binary, Eq, Generic, Show)
newtype SkillGroupID = SkillGroupID (CtiOpt Word32) deriving (Binary, Eq, Generic, Show)
newtype SkillGroupNumber = SkillGroupNumber (CtiOpt Word32) deriving (Binary, Eq, Generic, Show)

{-
16bit fixed sized integral types with special value
-}
newtype LineHandle = LineHandle (CtiOpt Word16) deriving (Binary, Eq, Generic, Show)

{-
32bit fixed sized integral types with special 0 value
-}
newtype CampaignID = CampaignID (CtiOpt0 Word32) deriving (Binary, Eq, Generic, Show)
newtype QueryRuleID = QueryRuleID (CtiOpt0 Word32) deriving (Binary, Eq, Generic, Show)
newtype SessionID = SessionID (CtiOpt0 Word32) deriving (Binary, Eq, Generic, Show)

{-
16bit fixed sized integral types with special 0 value
-}
newtype SkillGroupPriority = SkillGroupPriority (CtiOpt0 Word16) deriving (Binary, Eq, Generic, Show)

{-
Simple data types
-}
newtype CallID = CallID Word32 deriving (Binary, Eq, Show)
newtype DepartmentID = DepartmentID Word32 deriving (Binary, Eq, Show)
newtype EventReasonCode = EventReasonCode Word16 deriving (Binary, Eq, Show)
newtype IcmAgentID = IcmAgentID Word32 deriving (Binary, Eq, Show)
newtype InvokeID = InvokeID Word32 deriving (Binary, Eq, Show)
newtype MonitorID = MonitorID Word32 deriving (Binary, Eq, Show)
newtype MrdID = MrdID Word32 deriving (Binary, Eq, Show)
newtype PeripheralErrorCode = PeripheralErrorCode Word32 deriving (Binary, Eq, Show)
newtype SystemEventArg1 = SystemEventArg1 Word32 deriving (Binary, Eq, Show)
newtype SystemEventArg2 = SystemEventArg2 Word32 deriving (Binary, Eq, Show)
newtype SystemEventArg3 = SystemEventArg3 Word32 deriving (Binary, Eq, Show)
newtype Timestamp = Timestamp Word32 deriving (Binary, Eq, Show)


{-
Types for floating part
-}
{-
Row binary data without specified data type
-}
class Unspec a where
    toUnspec :: ByteString -> a
    fromWord8List :: [Word8] -> a
    fromWord8List = toUnspec . pack

getUnspec :: Unspec a => Get a
getUnspec = toUnspec <$> getRemainingLazyByteString

putUnspecN :: Int64 -> ByteString -> Put
putUnspecN limit bs = putLazyByteString $ take limit bs


newtype Unspec8 = Unspec8 ByteString deriving (Eq, Show)

instance Unspec Unspec8 where
    toUnspec = Unspec8

instance Binary Unspec8 where
    get = getUnspec
    put (Unspec8 bs) = putUnspecN 8 bs


newtype Unspec32 = Unspec32 ByteString deriving (Eq, Show)

instance Unspec Unspec32 where
    toUnspec = Unspec32

instance Binary Unspec32 where
    get = getUnspec
    put (Unspec32 bs) = putUnspecN 32 bs


newtype Unspec64 = Unspec64 ByteString deriving (Eq, Show)

instance Unspec Unspec64 where
    toUnspec = Unspec64

instance Binary Unspec64 where
    get = getUnspec
    put (Unspec64 bs) = putUnspecN 64 bs


newtype Unspec131 = Unspec131 ByteString deriving (Eq, Show)

instance Unspec Unspec131 where
    toUnspec = Unspec131

instance Binary Unspec131 where
    get = getUnspec
    put (Unspec131 bs) = putUnspecN 131 bs


newtype UnspecUnlimited = UnspecUnlimited ByteString deriving (Eq, Show)

instance Unspec UnspecUnlimited where
    toUnspec = UnspecUnlimited

instance Binary UnspecUnlimited where
    get = getUnspec
    put (UnspecUnlimited bs) = putUnspecN maxBound bs


{-
Null-terminated strings with upper bounded length.
Its representation is ByteString but not safe for UTF-8.
-}
class CtiString a where
    toCtiString :: ByteString -> a

getStrZ :: CtiString a => Get a
getStrZ = (toCtiString . takeWhile (/=0)) <$> getRemainingLazyByteString

putStrZN :: Int64 -> ByteString -> Put
putStrZN limit bs = (putLazyByteString . takeWhile (/=0) . take (limit - 1)) bs <> putWord8 0

newtype String2 = String2 ByteString deriving (Eq, IsString, Show)

instance CtiString String2 where
    toCtiString = String2

instance Binary String2 where
    get = getStrZ
    put (String2 bs) = putStrZN 2 bs


newtype String4 = String4 ByteString deriving (Eq, IsString, Show)

instance CtiString String4 where
    toCtiString = String4

instance Binary String4 where
    get = getStrZ
    put (String4 bs) = putStrZN 4 bs


newtype String12 = String12 ByteString deriving (Eq, IsString, Show)

instance CtiString String12 where
    toCtiString = String12

instance Binary String12 where
    get = getStrZ
    put (String12 bs) = putStrZN 12 bs


newtype String16 = String16 ByteString deriving (Eq, IsString, Show)

instance CtiString String16 where
    toCtiString = String16

instance Binary String16 where
    get = getStrZ
    put (String16 bs) = putStrZN 16 bs


newtype String20 = String20 ByteString deriving (Eq, IsString, Show)

instance CtiString String20 where
    toCtiString = String20

instance Binary String20 where
    get = getStrZ
    put (String20 bs) = putStrZN 20 bs


newtype String32 = String32 ByteString deriving (Eq, IsString, Show)

instance CtiString String32 where
    toCtiString = String32

instance Binary String32 where
    get = getStrZ
    put (String32 bs) = putStrZN 32 bs


newtype String40 = String40 ByteString deriving (Eq, IsString, Show)

instance CtiString String40 where
    toCtiString = String40

instance Binary String40 where
    get = getStrZ
    put (String40 bs) = putStrZN 40 bs


newtype String41 = String41 ByteString deriving (Eq, IsString, Show)

instance CtiString String41 where
    toCtiString = String41

instance Binary String41 where
    get = getStrZ
    put (String41 bs) = putStrZN 41 bs


newtype String64 = String64 ByteString deriving (Eq, IsString, Show)

instance CtiString String64 where
    toCtiString = String64

instance Binary String64 where
    get = getStrZ
    put (String64 bs) = putStrZN 64 bs


newtype String128 = String128 ByteString deriving (Eq, IsString, Show)

instance CtiString String128 where
    toCtiString = String128

instance Binary String128 where
    get = getStrZ
    put (String128 bs) = putStrZN 128 bs


newtype String255 = String255 ByteString deriving (Eq, IsString, Show)

instance CtiString String255 where
    toCtiString = String255

instance Binary String255 where
    get = getStrZ
    put (String255 bs) = putStrZN 255 bs

{-
Named Variable decoder and encoder
Named Variable (Tag=82) contains two null-terminated string within its payload.
The first string is name of the variable.  The second string is value of the variable.
-}
data NamedVar = NamedVar { namedVarName :: ByteString, namedVarValue :: ByteString } deriving (Eq, Generic, Show)

instance Binary NamedVar where
    get = do
        bs <- getRemainingLazyByteString
        let (name, val) = splitNameVal bs
        return $ NamedVar name val

    put (NamedVar name val) = do
        putStrZN 33 name
        putStrZN 211 val

splitNameVal :: ByteString -> (ByteString, ByteString)
splitNameVal bs = go (split 0 bs)
  where
    go (name:val:_) = (name, val)
    go (name:_)     = (name, empty)
    go []           = (empty, empty)

{-
Named Array decoder and encoder
Named Array (Tag=83) contains one 8bit unsigned integer and two null-terminated string
within its payload.  The unsigned char is array index.  The first string is name of the variable.
The second string is value of the variable.
-}
data NamedArr = NamedArr { namedArrayIndex :: Word8
                         , namedArrayName  :: ByteString
                         , namedArrayValue :: ByteString
                         } deriving (Eq, Generic, Show)

instance Binary NamedArr where
    get = do
        arrIndex <- getWord8
        bs <- getRemainingLazyByteString
        let (name, val) = splitNameVal bs
        return $ NamedArr arrIndex name val

    put (NamedArr arrIndex name val) = do
        putWord8 arrIndex
        putStrZN 33 name
        putStrZN 211 val




{-

-- Enumerations
data MessageType
    = MessageTypeZero                   --   0
    | FailureConf                       --   1
    | FailureEvent                      --   2
    | OpenReq                           --   3
    | OpenConf                          --   4
    | HeartbeatReq                      --   5
    | HeartbeatConf                     --   6
    | CloseReq                          --   7
    | CloseConf                         --   8
    | CallDeliveredEvent                --   9
    | CallEstablishedEvent              --  10
    | CallHeldEvent                     --  11
    | CallRetrievedEvent                --  12
    | CallClearedEvent                  --  13
    | CallConnectionClearedEvent        --  14
    | CallOriginatedEvent               --  15
    | CallFailedEvent                   --  16
    | CallConferencedEvent              --  17
    | CallTransferredEvent              --  18
    | CallDivertedEvent                 --  19
    | CallServiceInitiatedEvent         --  20
    | CallQueuedEvent                   --  21
    | CallTranslationRouteEvent         --  22
    | BeginCallEvent                    --  23
    | EndCallEvent                      --  24
    | CallDataUpdateEvent               --  25
    | SetCallDataReq                    --  26
    | SetCallDataConf                   --  27
    | ReleaseCallReq                    --  28
    | ReleaseCallConf                   --  29
    | AgentStateEvent                   --  30
    | SystemEvent                       --  31
    | ClientEventReportReq              --  32
    | ClientEventReportConf             --  33
    | CallReachedNetworkEvent           --  34
    | ControlFailureConf                --  35
    | QueryAgentStateReq                --  36
    | QueryAgentStateConf               --  37
    | SetAgentStateReq                  --  38
    | SetAgentStateConf                 --  39
    | AlternateCallReq                  --  40
    | AlternateCallConf                 --  41
    | AnswerCallReq                     --  42
    | AnswerCallConf                    --  43
    | ClearCallReq                      --  44
    | ClearCallConf                     --  45
    | ClearConnectionReq                --  46
    | ClearConnectionConf               --  47
    | ConferenceCallReq                 --  48
    | ConferenceCallConf                --  49
    | ConsultationCallReq               --  50
    | ConsultationCallConf              --  51
    | DeflectCallReq                    --  52
    | DeflectCallConf                   --  53
    | HoldCallReq                       --  54
    | HoldCallConf                      --  55
    | MakeCallReq                       --  56
    | MakeCallConf                      --  57
    | MakePredictiveCallReq             --  58
    | MakePredictiveCallConf            --  59
    | ReconnectCallReq                  --  60
    | ReconnectCallConf                 --  61
    | RetrieveCallReq                   --  62
    | RetrieveCallConf                  --  63
    | TransferCallReq                   --  64
    | TransferCallConf                  --  65
    | ReservedMessageType66             --  66
    | ReservedMessageType67             --  67
    | ReservedMessageType68             --  68
    | ReservedMessageType69             --  69
    | ReservedMessageType70             --  70
    | ReservedMessageType71             --  71
    | ReservedMessageType72             --  72
    | ReservedMessageType73             --  73
    | ReservedMessageType74             --  74
    | ReservedMessageType75             --  75
    | ReservedMessageType76             --  76
    | ReservedMessageType77             --  77
    | QueryDeviceInfoReq                --  78
    | QueryDeviceInfoConf               --  79
    | ReservedMessageType80             --  80
    | ReservedMessageType81             --  81
    | SnapshotCallReq                   --  82
    | SnapshotCallConf                  --  83
    | SnapshotDeviceReq                 --  84
    | SnapshotDeviceConf                --  85
    | CallDequeuedEvent                 --  86
    | ReservedMessageType87             --  87
    | ReservedMessageType88             --  88
    | ReservedMessageType89             --  89
    | ReservedMessageType90             --  90
    | SendDtmfSignalReq                 --  91
    | SendDtmfSignalConf                --  92
    | MonitorStartReq                   --  93
    | MonitorStartConf                  --  94
    | MonitorStopReq                    --  95
    | MonitorStopConf                   --  96
    | ChangeMonitorMaskReq              --  97
    | ChangeMonitorMaskConf             --  98
    | ClientSessionOpenedEvent          --  99
    | ClientSessionClosedEvent          -- 100
    | SessionMonitorStartReq            -- 101
    | SessionMonitorStartConf           -- 102
    | SessionMonitorStopReq             -- 103
    | SessionMonitorStopConf            -- 104
    | AgentPreCallEvent                 -- 105
    | AgentPreCallAbortEvent            -- 106
    | UserMessageReq                    -- 107
    | UserMessageConf                   -- 108
    | UserMessageEvent                  -- 109
    | RegisterVariablesReq              -- 110
    | RegisterVariablesConf             -- 111
    | QueryAgentStatisticsReq           -- 112
    | QueryAgentStatisticsConf          -- 113
    | QuerySkillGroupStatisticsReq      -- 114
    | QuerySkillGroupStatisticsConf     -- 115
    | RtpStartedEvent                   -- 116
    | RtpStoppedEvent                   -- 117
    | SupervisorAssistReq               -- 118
    | SupervisorAssistConf              -- 119
    | SupervisorAssistEvent             -- 120
    | EmergencyCallReq                  -- 121
    | EmergencyCallConf                 -- 122
    | EmergencyCallEvent                -- 123
    | SuperviseCallReq                  -- 124
    | SuperviseCallConf                 -- 125
    | AgentTeamConfigReq                -- 126
    | AgentTeamConfigConf               -- 127
    | AgentTeamConfigEvent              -- 128
    | SetAppDataReq                     -- 129
    | SetAppDataConf                    -- 130
    | AgentDeskSettingsReq              -- 131
    | AgentDeskSettingsConf             -- 132
    | ListAgentTeamReq                  -- 133
    | ListAgentTeamConf                 -- 134
    | MonitorAgentTeamStartReq          -- 135
    | MonitorAgentTeamStartConf         -- 136
    | MonitorAgentTeamStopReq           -- 137
    | MonitorAgentTeamStopConf          -- 138
    | BadCallReq                        -- 139
    | BadCallConf                       -- 140
    | SetDeviceAttributesReq            -- 141
    | SetDeviceAttributesConf           -- 142
    | RegisterServiceReq                -- 143
    | RegisterServiceConf               -- 144
    | UnregisterServiceReq              -- 145
    | UnregisterServiceConf             -- 146
    | StartRecordingReq                 -- 147
    | StartRecordingConf                -- 148
    | StopRecordingReq                  -- 149
    | StopRecordingConf                 -- 150
    | MediaLoginReq                     -- 151
    | MediaLoginResp                    -- 152
    | MediaLogoutInd                    -- 153
    | MakeAgentRoutableInd              -- 154
    | MakeAgentNotRoutableReq           -- 155
    | MakeAgentNotRoutableResp          -- 156
    | MakeAgentReadyInd                 -- 157
    | MakeAgentNotReadyReq              -- 158
    | MakeAgentNotReadyResp             -- 159
    | OfferTaskInd                      -- 160
    | OfferApplicationTaskReq           -- 161
    | OfferApplicationTaskResp          -- 162
    | StartTaskInd                      -- 163
    | StartApplicationTaskReq           -- 164
    | StartApplicationTaskResp          -- 165
    | PauseTaskInd                      -- 166
    | ResumeTaskInd                     -- 167
    | WrapupTaskInd                     -- 168
    | EndTaskInd                        -- 169
    | AgentMakeNotRoutableEvent         -- 170
    | AgentInterruptAdvisoryEvent       -- 171
    | AgentInterruptAcceptedInd         -- 172
    | AgentInterruptUnacceptedInd       -- 173
    | AgentInterruptDoneAdvisoryEvent   -- 174
    | AgentInterruptDoneAcceptedInd     -- 175
    | ChangeMaxTaskLimitReq             -- 176
    | ChangeMaxTaskLimitResp            -- 177
    | OverrideLimitReq                  -- 178
    | OverrideLimitResp                 -- 179
    | UpdateTaskContextInd              -- 180
    | BeginAgentInitInd                 -- 181
    | AgentInitReq                      -- 182
    | AgentInitResp                     -- 183
    | EndAgentInitInd                   -- 184
    | TaskInitInd                       -- 185
    | AgentInitReadyEvent               -- 186
    | GetPrecallMessageReq              -- 187
    | GetPrecallMessageResp             -- 188
    | AgentLegacyPreCallEvent           -- 189
    | FailureResp                       -- 190
    | BeginTaskEvent                    -- 191
    | QueuedTaskEvent                   -- 192
    | DequeuedTaskEvent                 -- 193
    | OfferTaskEvent                    -- 194
    | StartTaskEvent                    -- 195
    | PauseTaskEvent                    -- 196
    | ResumeTaskEvent                   -- 197
    | WrapupTaskEvent                   -- 198
    | EndTaskEvent                      -- 199
    | TaskDataUpdateEvent               -- 200
    | TaskMonitorStartReq               -- 201
    | TaskMonitorStartConf              -- 202
    | TaskMonitorStopReq                -- 203
    | TaskMonitorStopConf               -- 204
    | ChangeTaskMonitorMaskReq          -- 205
    | ChangeTaskMonitorMaskConf         -- 206
    | MaxTaskLifetimeExceededEvent      -- 207
    | SetAppPathDataInd                 -- 208
    | TaskInitReq                       -- 209
    | TaskInitResp                      -- 210
    | RouteRegisterEvent                -- 211
    | RouteRegisterReplyEvent           -- 212
    | RouteRequestEvent                 -- 213
    | RouteSelectEvent                  -- 214
    | RouteEndEvent                     -- 215
    | ReservedMessageType216            -- 216
    | ReservedMessageType217            -- 217
    | ReservedMessageType218            -- 218
    | ReservedMessageType219            -- 219
    | ReservedMessageType220            -- 220
    | ReservedMessageType221            -- 221
    | ReservedMessageType222            -- 222
    | ReservedMessageType223            -- 223
    | ReservedMessageType224            -- 224
    | ReservedMessageType225            -- 225
    | ReservedMessageType226            -- 226
    | ReservedMessageType227            -- 227
    | ReservedMessageType228            -- 228
    | ReservedMessageType229            -- 229
    | ConfigRequestKeyEvent             -- 230
    | ConfigKeyEvent                    -- 231
    | ConfigRequestEvent                -- 232
    | ConfigBeginEvent                  -- 233
    | ConfigEndEvent                    -- 234
    | ConfigServiceEvent                -- 235
    | ConfigSkillGroupEvent             -- 236
    | ConfigAgentEvent                  -- 237
    | ConfigDeviceEvent                 -- 238
    | ReservedMessageType239            -- 239
    | ReservedMessageType230            -- 230
    | ReservedMessageType241            -- 241
    | TeamConfigReq                     -- 242
    | TeamConfigEvent                   -- 243
    | TeamConfigConf                    -- 244
    | ConfigCallTypeEvent               -- 245
    | ReservedMessageType246            -- 246
    | ReservedMessageType247            -- 247
    | CallAgentGreetingEvent            -- 248
    | AgentGreetingControlReq           -- 249
    | AgentGreetingControlConf          -- 250
    | ReservedMessageType251            -- 251
    | ReservedMessageType252            -- 252
    | ReservedMessageType253            -- 253
    | ConfigMrdEvent                    -- 254
    | GetAgentTaskReq                   -- 255
    | AgentTaskResp                     -- 256
    | SnapshotTaskReq                   -- 257
    | SnapshotTaskResp                  -- 258
    | ReservedMessageType259            -- 259
    deriving (Bounded, Enum, Eq, Generic, Show)

instance Binary MessageType where
    get = getEnum32
    put = putEnum32

instance Binary (SafeEnum MessageType) where
    get = getSafeEnum32
    put = putSafeEnum

data FloatTag
    = Reserved0Tag                  --   0
    | ClientIdTag                   --   1
    | ClientPasswordTag             --   2
    | ClientSignatureTag            --   3
    | AgentExtensionTag             --   4
    | AgentIDTag                    --   5
    | AgentInstrumentTag            --   6
    | TextTag                       --   7
    | AniTag                        --   8
    | UuiTag                        --   9
    | DnisTag                       --  10
    | DialedNumberTag               --  11
    | CedTag                        --  12
    | CallVar1Tag                   --  13
    | CallVar2Tag                   --  14
    | CallVar3Tag                   --  15
    | CallVar4Tag                   --  16
    | CallVar5Tag                   --  17
    | CallVar6Tag                   --  18
    | CallVar7Tag                   --  19
    | CallVar8Tag                   --  20
    | CallVar9Tag                   --  21
    | CallVar10Tag                  --  22
    | CtiClientSignatureTag         --  23
    | CtiClientTimestampTag         --  24
    | ConnectionDevIDTag            --  25
    | AlertingDevIDTag              --  26
    | CallingDevIDTag               --  27
    | CalledDevIDTag                --  28
    | LastRedirectDevIDTag          --  29
    | AnsweringDevIDTag             --  30
    | HoldingDevIDTag               --  31
    | RetrievingDevIDTag            --  32
    | ReleasingDevIDTag             --  33
    | FailingDevIDTag               --  34
    | PrimaryDevIDTag               --  35
    | SecondaryDevIDTag             --  36
    | ControllerDevIDTag            --  37
    | AddedPartyDevIDTag            --  38
    | PartyCallIDTag                --  39
    | PartyDevIDTypeTag             --  40
    | PartyDevIDTag                 --  41
    | TransferringDevIDTag          --  42
    | TransferredDevIDTag           --  43
    | DivertingDevIDTag             --  44
    | QueueDevIDTag                 --  45
    | CallWrapupDataTag             --  46
    | NewConnectionDevIDTag         --  47
    | TrunkUsedDevIDTag             --  48
    | AgentPasswordTag              --  49
    | ActiveConnDevIDTag            --  50
    | FacilityCodeTag               --  51
    | OtherConnDevIDTag             --  52
    | HeldConnDevIDTag              --  53
    | Reserved54Tag                 --  54
    | Reserved55Tag                 --  55
    | CallConnCallIDTag             --  56
    | CallConnDevIDTypeTag          --  57
    | CallConnDevIDTag              --  58
    | CallDevIDTypeTag              --  59
    | CallDevIDTag                  --  60
    | CallDevConnStateTag           --  61
    | SkillGroupNumberTag           --  62
    | SkillGroupIDTag               --  63
    | SkillGroupPriorityTag         --  64
    | SkillGroupStateTag            --  65
    | ObjectNameTag                 --  66
    | DtmfStringTag                 --  67
    | PositionIDTag                 --  68
    | SupervisorIDTag               --  69
    | LineHandleTag                 --  70
    | LineTypeTag                   --  71
    | RouterCallKeyDayTag           --  72
    | RouterCallKeyCallIDTag        --  73
    | Reserved74Tag                 --  74
    | CallStateTag                  --  75
    | MonitoredDevIDTag             --  76
    | AuthorizationCodeTag          --  77
    | AccountCodeTag                --  78
    | OriginatingDevIDTag           --  79
    | OriginatingLineIDTag          --  80
    | ClientAddressTag              --  81
    | NamedVariableTag              --  82
    | NamedArrayTag                 --  83
    | CallControlTableTag           --  84
    | SupervisorInstrumentTag       --  85
    | AtcAgentIDTag                 --  86
    | AgentFlagsTag                 --  87
    | AtcAgentStateTag              --  88
    | AtcStateDurationTag           --  89
    | AgentConnectionDevIDTag       --  90
    | SupervisorConnectionDevIDTag  --  91
    | ListTeamIDTag                 --  92
    | DefaultDevicePortAddressTag   --  93
    | ServiceNameTag                --  94
    | CustomerPhoneNumberTag        --  95
    | CustomerAccountNumberTag      --  96
    | AppPathIDTag                  --  97
    | Reserved98Tag                 --  98
    | ScriptSelectorTag             --  99
    | ApplicationString1Tag         -- 100
    | ApplicationString2Tag         -- 101
    | Reserved102Tag                -- 102
    | Reserved103Tag                -- 103
    | Reserved104Tag                -- 104
    | Reserved105Tag                -- 105
    | Reserved106Tag                -- 106
    | Reserved107Tag                -- 107
    | Reserved108Tag                -- 108
    | Reserved109Tag                -- 109
    | RouterCallKeySequenceNumTag   -- 110
    | Reserved111Tag                -- 111
    | Reserved112Tag                -- 112
    | Reserved113Tag                -- 113
    | Reserved114Tag                -- 114
    | Reserved115Tag                -- 115
    | Reserved116Tag                -- 116
    | Reserved117Tag                -- 117
    | Reserved118Tag                -- 118
    | Reserved119Tag                -- 119
    | Reserved120Tag                -- 120
    | TrunkNumberTag                -- 121
    | TrunkGroupNumberTag           -- 122
    | ExtAgentStateTag              -- 123
    | DequeueTypeTag                -- 124
    | SendingAddressTag             -- 125
    | SendingPortTag                -- 126
    | Reserved127Tag                -- 127
    | Reserved128Tag                -- 128
    | MaxQueuedTag                  -- 129
    | QueueIDTag                    -- 130
    | CustomerIDTag                 -- 131
    | ServiceSkillTargetIDTag       -- 132
    | PeripheralNameTag             -- 133
    | DescriptionTag                -- 134
    | ServiceMemberIDTag            -- 135
    | ServiceMemberPriorityTag      -- 136
    | FirstNameTag                  -- 137
    | LastNameTag                   -- 138
    | SkillGroupTag                 -- 139
    | Reserved140Tag                -- 140
    | AgentSkillTargetID141Tag      -- 141
    | ServiceTag                    -- 142
    | Reserved143Tag                -- 143
    | Reserved144Tag                -- 144
    | Reserved145Tag                -- 145
    | Reserved146Tag                -- 146
    | Reserved147Tag                -- 147
    | Reserved148Tag                -- 148
    | Reserved149Tag                -- 149
    | DurationTag                   -- 150
    | Reserved151Tag                -- 151
    | Reserved152Tag                -- 152
    | Reserved153Tag                -- 153
    | Reserved154Tag                -- 154
    | Reserved155Tag                -- 155
    | Reserved156Tag                -- 156
    | Reserved157Tag                -- 157
    | Reserved158Tag                -- 158
    | Reserved159Tag                -- 159
    | Reserved160Tag                -- 160
    | Reserved161Tag                -- 161
    | Reserved162Tag                -- 162
    | Reserved163Tag                -- 163
    | Reserved164Tag                -- 164
    | Reserved165Tag                -- 165
    | Reserved166Tag                -- 166
    | Reserved167Tag                -- 167
    | Reserved168Tag                -- 168
    | Reserved169Tag                -- 169
    | Reserved170Tag                -- 170
    | Reserved171Tag                -- 171
    | Reserved172Tag                -- 172
    | ExtensionTag                  -- 173
    | ServiceLevelThresholdTag      -- 174
    | ServiceLevelTypeTag           -- 175
    | ConfigParamTag                -- 176
    | ServiceConfigKeyTag           -- 177
    | SkillGroupConfigKeyTag        -- 178
    | AgentConfigKeyTag             -- 179
    | DeviceConfigKeyTag            -- 180
    | Reserved181Tag                -- 181
    | Reserved182Tag                -- 182
    | RecordTypeTag                 -- 183
    | PeripheralNumberTag           -- 184
    | AgentSkillTargetID185Tag      -- 185
    | NumServiceMembersTag          -- 186
    | ServiceMemberTag              -- 187
    | ServicePriorityTag            -- 188
    | AgentTypeTag                  -- 189
    | LoginIDTag                    -- 190
    | NumSkillsTag                  -- 191
    | SkillGroupSkillTargetIDTag    -- 192
    | ServiceIDTag                  -- 193
    | AgentIDLongTag                -- 194
    | DeviceTypeTag                 -- 195
    | Reserved196Tag                -- 196
    | Reserved197Tag                -- 197
    | EnableTag                     -- 198
    | DeviceIDTag                   -- 199
    | TimeoutTag                    -- 200
    | CurrentRouteTag               -- 201
    | SecondaryConnectionCallIDTag  -- 202
    | PriorityQueueNumberTag        -- 203
    | TeamNameTag                   -- 204
    | MemberTypeTag                 -- 205
    | EventDeviceIDTag              -- 206
    | LoginNameTag                  -- 207
    | PeripheralIDTag               -- 208
    | CallTypeKeyConfigTag          -- 209
    | CallTypeIDTag                 -- 210
    | CustomerDefinitionIDTag       -- 211
    | EnterpriseNameTag             -- 212
    | CurPeripheralNumberTag        -- 213
    | CurLoginIDTag                 -- 214
    | AniIITag                      -- 215
    | MRDomainIDTag                 -- 216
    | CtiosCilClientIDTag           -- 217
    | SilentMonitorStatusTag        -- 218
    | RequestingDeviceIDTag         -- 219
    | RequestingDeviceIDTypeTag     -- 220
    | PreCallInvokeIDTag            -- 221
    | EnterpriseQueueTimeTag        -- 222
    | CallReferenceIDTag            -- 223
    | MultiLineAgentControlTag      -- 224
    | NetworkControlledTag          -- 225
    | Reserved226Tag                -- 226
    | Reserved227Tag                -- 227
    | NumPeripheralsTag             -- 228
    | CocConnectionCallIDTag        -- 229
    | CocConnectionDeviceIDTypeTag  -- 230
    | CocConnectionDeviceIDTag      -- 231
    | CallOriginatedFromTag         -- 232
    | SetAppdataCallidTag           -- 233
    | ClientShareKeyTag             -- 234
    | Reserved235Tag                -- 235
    | Reserved236Tag                -- 236
    | Reserved237Tag                -- 237
    | Reserved238Tag                -- 238
    | Reserved239Tag                -- 239
    | Reserved240Tag                -- 240
    | Reserved241Tag                -- 241
    | Reserved242Tag                -- 242
    | AgentTeamNameTag              -- 243
    | DirectionTag                  -- 244
    | OptionsTag                    -- 245
    | FltMrdIDTag                   -- 246
    | MediaClassIDTag               -- 247
    | TaskLifeTag                   -- 248
    | TaskStartTimeoutTag           -- 249
    | MaxTaskDurationTag            -- 250
    | InterruptibleTag              -- 251
    | MaxCallsInQueueTag            -- 252
    | MaxCallsInQueuePreCallTypeTag -- 253
    | MaxTimeInQueueTag             -- 254
    | InternalAgentStateTag         -- 255
    | Reserved256Tag                -- 256
    | SsoEnabledTag                 -- 257
    | FltTaskIDTag                  -- 258
    | FltIcmDispTag                 -- 259
    | FltAppDispTag                 -- 260
    deriving (Bounded, Enum, Eq, Generic, Show)

instance Binary FloatTag where
    get = getEnum16
    put = putEnum16

instance Binary (SafeEnum FloatTag) where
    get = getSafeEnum16
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


data ParseError
    = UnknownFailureIndicationMessageStatusCode Word32
    | UnknownMessageType Word32
    deriving (Eq, Show)


-}
