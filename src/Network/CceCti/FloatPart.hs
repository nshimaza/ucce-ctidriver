{-# LANGUAGE DeriveGeneric #-}

module Network.CceCti.FloatPart where

import           Data.Binary              (Binary, get)
import           Data.Binary.Get          (isEmpty)
import           GHC.Generics             (Generic)

import           Network.CceCti.FloatElem


{-
FloatPart
FloatPart is list of FloatElem with special binary serialization / de-serialization methods.
-}

newtype FloatPart = FloatPart [FloatElem] deriving (Eq, Generic, Show)

instance Binary FloatPart where
    get = go []
      where
        go xs = do
            finished <- isEmpty
            if finished
            then return . FloatPart $ reverse xs
            else do
                x <- get
                go (x:xs)



{-
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
                         , namedArrayName :: ByteString
                         , namedArrayValue :: ByteString
                         } deriving (Eq, Generic, Show)

instance Binary NamedArr where
    get = do
        index <- getWord8
        bs <- getRemainingLazyByteString
        let (name, val) = splitNameVal bs
        return $ NamedArr index name val

    put (NamedArr index name val) = do
        putWord8 index
        putStrZN 33 name
        putStrZN 211 val
-}

{-
Large sum of floating element type.
-}
{-
data FloatElem
    = ClientID                  String64            --   1
    | ClientPassword            Unspec64            --   2
    | ClientSignature           String64            --   3
    | AgentExtension            String16            --   4
    | AgentID                   String12            --   5
    | AgentInstrument           String64            --   6
    | FloatFieldText            String255           --   7
    | Ani                       String40            --   8
    | UserToUserInfo            Unspec131           --   9
    | Dnis                      String32            --  10
    | DialedNumber              String40            --  11
    | CallerEnteredDigit        String40            --  12
    | CallVar1                  String41            --  13
    | CallVar2                  String41            --  14
    | CallVar3                  String41            --  15
    | CallVar4                  String41            --  16
    | CallVar5                  String41            --  17
    | CallVar6                  String41            --  18
    | CallVar7                  String41            --  19
    | CallVar8                  String41            --  20
    | CallVar9                  String41            --  21
    | CallVar10                 String41            --  22
    | CtiClientSignature        String64            --  23
    | CtiClientTimestamp        Timestamp           --  24
    | ConnectionDevID           String64            --  25
    | AlertingDevID             String64            --  26
    | CallingDevID              String64            --  27
    | CalledDevID               String64            --  28
    | LastRedirectDevID         String64            --  29
    | AnsweringDevID            String64            --  30
    | HoldingDevID              String64            --  31
    | RetrievingDevID           String64            --  32
    | ReleasingDevID            String64            --  33
    | FailingDevID              String64            --  34
    | PrimaryDevID              String64            --  35
    | SecondaryDevID            String64            --  36
    | ControllerDevID           String64            --  37
    | AddedPartyDevID           String64            --  38
    | ConnectedPartyCallID      CallID              --  39
    | ConnectedPartyDevIDType   (SafeEnum (CtiOpt DeviceIDType))    --  40 -- FIXME: or ConnectionDeviceIDType
    | ConnectedPartyDevID       String64            --  41
    | TransferringDevID         String64            --  42
    | TransferredDevID          String64            --  43
    | DivertingDevID            String64            --  44
    | QueueDevID                String64            --  45
    | CallWrapupData            String40            --  46
    | NewConnectionDevID        String64            --  47
    | TrunkUsedDevID            String64            --  48
    | AgentPassword             String64            --  49
    | ActiveConnDevID           String64            --  50
    | FacilityCode              String40            --  51
    | OtherConnDevID            String64            --  52
    | HeldConnDevID             String64            --  53
    | FloatElemRsv54            UnspecUnlimited     --  54
    | FloatElemRsv55            UnspecUnlimited     --  55
    | CallConnCallID            CallID              --  56
    | CallConnDevIDType         (SafeEnum (CtiOpt ConnectionDeviceIDType))  --  57 -- FIXME: or DeviceIDType
    | CallConnDevID             String64            --  58
    | CallDevIDType             (SafeEnum (CtiOpt DeviceIDType))    -- 59 -- FIXME: or ConnectionDeviceIDType
    | CallDevID                 String64            --  60
    | CallDevConnState          (SafeEnum (CtiOpt LocalConnectionState))    --  61
    | SkillGroupNumberF         SkillGroupNumber    --  62
    | SkillGroupIDF             SkillGroupID        --  63
    | SkillGroupPriority        Word16              --  64
    | SkillGroupState           (SafeEnum AgentState)   -- 65
    | ObjectName                String128           --  66
    | DtmfString                String32            --  67
    | PositionID                String12            --  68
    | SupervisorID              String12            --  69
    | LineHandleF               LineHandle          --  70
    | LineTypeF                 (SafeEnum (CtiOpt LineType))    --  71
    | RouterCallKeyDay          Word32              --  72
    | RouterCallkeyCallID       Word32              --  73
    | FloatElemRsv74            UnspecUnlimited     --  74
    | CallState                 (SafeEnum (CtiOpt LocalConnectionState))    --  75
    | MonitoredDevID            String64            --  76
    | AuthorizationCode         String40            --  77
    | AccountCode               String40            --  78
    | OriginatingDevID          String64            --  79
    | OriginatingLineID         String64            --  80
    | ClientAddress             String64            --  81
    | NamedVariable             NamedVar            --  82
    | NamedArray                NamedArr            --  83
    | CallControlTable          String4             --  84
    | SupervisorInstrument      String64            --  85
    | AtcAgentID                String12            --  86
    | AgentFlags                (BitSet (Holder AgentFlagsMask) AgentFlagsMask) --  87
    | AtcAgentState             (SafeEnum AgentState)   --  88
    | AtcStateDuration          Word32              --  89
    | AgentConnDevID            String64            --  90
    | SupervisorConnDevID       String64            --  91
    | ListTeamID                Word32              --  92
    | DefaultDevicePortAddress  String32            --  93
    | ServiceName               String64            --  94
    | CustomerPhoneNumber       String20            --  95
    | CustomerAccountNumber     String32            --  96
    | AppPathID                 Word32              --  97
    | FloatElemRsv98            UnspecUnlimited     --  98
    | ScriptSelector            UnspecUnlimited     --  99
    | ApplicationString1        UnspecUnlimited     -- 100
    | ApplicationString2        UnspecUnlimited     -- 101
    | FloatElemRsv102           UnspecUnlimited     -- 102
    | FloatElemRsv103           UnspecUnlimited     -- 103
    | FloatElemRsv104           UnspecUnlimited     -- 104
    | FloatElemRsv105           UnspecUnlimited     -- 105
    | FloatElemRsv106           UnspecUnlimited     -- 106
    | FloatElemRsv107           UnspecUnlimited     -- 107
    | FloatElemRsv108           UnspecUnlimited     -- 108
    | FloatElemRsv109           UnspecUnlimited     -- 109
    | RouterCallKeySequenceNum  Word32              -- 110
    | FloatElemRsv111           UnspecUnlimited     -- 111
    | FloatElemRsv112           UnspecUnlimited     -- 112
    | FloatElemRsv113           UnspecUnlimited     -- 113
    | FloatElemRsv114           UnspecUnlimited     -- 114
    | FloatElemRsv115           UnspecUnlimited     -- 115
    | FloatElemRsv116           UnspecUnlimited     -- 116
    | FloatElemRsv117           UnspecUnlimited     -- 117
    | FloatElemRsv118           UnspecUnlimited     -- 118
    | FloatElemRsv119           UnspecUnlimited     -- 119
    | FloatElemRsv120           UnspecUnlimited     -- 120
    | TrunkNumber               Word32              -- 121
    | TrunkGroupNumber          Word32              -- 122
    | NextAgentState            AgentState          -- 123
    | DequeueType               UnspecUnlimited     -- 124
    | SendingAddress            String64            -- 125
    | SendingPort               Word32              -- 126
    | FloatElemRsv127           UnspecUnlimited     -- 127
    | FloatElemRsv128           UnspecUnlimited     -- 128
    | MaxQueued                 Word32              -- 129
    | QueueID                   UnspecUnlimited     -- 130
    | CustomerID                Word32              -- 131
    | ServiceSkillTargetID      Word32              -- 132
    | PeripheralName            String64            -- 133
    | Description               String128           -- 134
    | ServiceMemberID           UnspecUnlimited     -- 135
    | ServiceMemberPriority     UnspecUnlimited     -- 136
    | FirstName                 String32            -- 137
    | LastName                  String32            -- 138
    | SkillGroup                UnspecUnlimited     -- 139
    | FloatElemRsv140           UnspecUnlimited     -- 140
    | AgentSkillTargetID141     Word32              -- 141
    | Service                   UnspecUnlimited     -- 142
    | FloatElemRsv143           UnspecUnlimited     -- 143
    | FloatElemRsv144           UnspecUnlimited     -- 144
    | FloatElemRsv145           UnspecUnlimited     -- 145
    | FloatElemRsv146           UnspecUnlimited     -- 146
    | FloatElemRsv147           UnspecUnlimited     -- 147
    | FloatElemRsv148           UnspecUnlimited     -- 148
    | FloatElemRsv149           UnspecUnlimited     -- 149
    | Duration                  Word32              -- 150
    | FloatElemRsv151           UnspecUnlimited     -- 151
    | FloatElemRsv152           UnspecUnlimited     -- 152
    | FloatElemRsv153           UnspecUnlimited     -- 153
    | FloatElemRsv154           UnspecUnlimited     -- 154
    | FloatElemRsv155           UnspecUnlimited     -- 155
    | FloatElemRsv156           UnspecUnlimited     -- 156
    | FloatElemRsv157           UnspecUnlimited     -- 157
    | FloatElemRsv158           UnspecUnlimited     -- 158
    | FloatElemRsv159           UnspecUnlimited     -- 159
    | FloatElemRsv160           UnspecUnlimited     -- 160
    | FloatElemRsv161           UnspecUnlimited     -- 161
    | FloatElemRsv162           UnspecUnlimited     -- 162
    | FloatElemRsv163           UnspecUnlimited     -- 163
    | FloatElemRsv164           UnspecUnlimited     -- 164
    | FloatElemRsv165           UnspecUnlimited     -- 165
    | FloatElemRsv166           UnspecUnlimited     -- 166
    | FloatElemRsv167           UnspecUnlimited     -- 167
    | FloatElemRsv168           UnspecUnlimited     -- 168
    | FloatElemRsv169           UnspecUnlimited     -- 169
    | FloatElemRsv170           UnspecUnlimited     -- 170
    | FloatElemRsv171           UnspecUnlimited     -- 171
    | FloatElemRsv172           UnspecUnlimited     -- 172
    | Extension                 String16            -- 173
    | ServiceLevelThreshold     Word32              -- 174
    | ServiceLevelType          Word32              -- 175
    | ConfigParam               String255           -- 176
    | ServiceConfigKey          Unspec8             -- 177
    | SkillGroupConfigKey       Unspec8             -- 178
    | AgentConfigKey            Unspec8             -- 179
    | DeviceConfigKey           Unspec8             -- 180
    | FloatElemRsv181           UnspecUnlimited     -- 181
    | FloatElemRsv182           UnspecUnlimited     -- 182
    | RecordTypeF               (SafeEnum RecordType) -- 183
    | PeripheralNumber          Word32              -- 184
    | AgentSkillTargetID185     Word32              -- 185
    | NumServiceMembers         Word16              -- 186
    | ServiceMember             Word32              -- 187
    | ServicePriority           Word16              -- 188
    | AgentType                 (SafeEnum AgentType) -- 189
    | LoginID                   String64            -- 190
    | NumSkills                 Word16              -- 191
    | SkillGroupSkillTargetID   Word32              -- 192
    | FltServiceID              Word32              -- 193
    | AgentIDLong               String32            -- 194
    | DeviceTypeF               (SafeEnum DeviceType)   -- 195
    | FloatElemRsv196           UnspecUnlimited     -- 196
    | FloatElemRsv197           UnspecUnlimited     -- 197
    | Enable                    UnspecUnlimited     -- 198
    | DeviceID                  UnspecUnlimited     -- 199
    | Timeout                   UnspecUnlimited     -- 200
    | CurrentRoute              UnspecUnlimited     -- 201
    | SecondaryConnCallID       Word32              -- 202
    | PriorityQueueNumber       UnspecUnlimited     -- 203
    | TeamName                  UnspecUnlimited     -- 204
    | MemberType                UnspecUnlimited     -- 205
    | EventDeviceID             String64            -- 206
    | LoginName                 String255           -- 207
    | FltPeripheralID           Word32              -- 208
    | CallTypeConfigKey         Unspec8             -- 209
    | CallTypeID                Word32              -- 210
    | CustomerDefinitionID      Word32              -- 211
    | FltEnterpriseName         String32            -- 212
    | OldPeripheralNumber       Word32              -- 213
    | OldLoginID                String64            -- 214
    | AniII                     String2             -- 215
    | FltMRDomainID             Word32              -- 216
    | CtiosCilClientID          String64            -- 217
    | SilentMonitorStatusF      (SafeEnum SilentMonitorStatus)  -- 218
    | RequestingDevID           String64            -- 219
    | RequestingDevIDType       (SafeEnum (CtiOpt DeviceIDType))    -- 220
    | PreCallInvokeID           Word32              -- 221
    | EnterpriseQueueTime       UnspecUnlimited     -- 222
    | CallReferenceID           Unspec32            -- 223
    | MultiLineAgnetControl     (SafeEnum MultilineAgentConrol) -- 224
    | NetworkControlled         UnspecUnlimited     -- 225
    | FloatElemRsv226           UnspecUnlimited     -- 226
    | FloatElemRsv227           UnspecUnlimited     -- 227
    | NumPeripherals            Word16              -- 228
    | CoCConnCallID             Word32              -- 229
    | CoCConnDevIDType          (SafeEnum (CtiOpt ConnectionDeviceIDType))  -- 230
    | CoCConnDevID              String64            -- 231
    | CallOriginatedFrom        Word8               -- 232
    | SetAppDataCallID          UnspecUnlimited     -- 233
    | ClientShareKey            UnspecUnlimited     -- 234
    | FloatElemRsv235           UnspecUnlimited     -- 235
    | FloatElemRsv236           UnspecUnlimited     -- 236
    | FloatElemRsv237           UnspecUnlimited     -- 237
    | FloatElemRsv238           UnspecUnlimited     -- 238
    | FloatElemRsv239           UnspecUnlimited     -- 239
    | FloatElemRsv240           UnspecUnlimited     -- 240
    | FloatElemRsv241           UnspecUnlimited     -- 241
    | FloatElemRsv242           UnspecUnlimited     -- 242
    | AgentTeamName             String32            -- 243
    | Direction                 (SafeEnum Direction)    -- 244
    | Options                   UnspecUnlimited     -- 245
    | FltMrdD                   UnspecUnlimited     -- 246
    | MediaClassID              UnspecUnlimited     -- 247
    | TaskLife                  UnspecUnlimited     -- 248
    | TaskStartTimeout          UnspecUnlimited     -- 249
    | MaxTaskDuration           Word32              -- 250
    | FltInterruptible          (SafeEnum CtiBool)  -- 251
    | MaxCallsInQueue           UnspecUnlimited     -- 252
    | MaxCallsInQPreCallType    UnspecUnlimited     -- 253
    | MaxTimeInQueue            UnspecUnlimited     -- 254
    | InternalAgentState        (SafeEnum AgentInternalState)   -- 255
    | FloatElemRsv256           UnspecUnlimited     -- 256
    | SsoEnabledF               (SafeEnum SsoEnabled)   -- 257
    | FltTaskID                 UnspecUnlimited     -- 258
    | FltIcmDisp                UnspecUnlimited     -- 259
    | FltAppDisp                UnspecUnlimited     -- 260


    deriving (Eq, Generic, Show)

instance Binary FloatElem where
    get = getFloatElem
    put = putF
    putList = mapM_ put

getFloatElem :: Get FloatElem
getFloatElem = do
    tag <- getWord16be
    len <- getWord16be
    bs <- getLazyByteString $ fromIntegral len
    getF tag bs


putFloatElem :: (Binary a) => Word16 -> a -> Put
putFloatElem tag x = do
    putWord16be tag
    let bs = encode x
    putWord16be . fromIntegral $ length bs
    putLazyByteString bs


getF :: Word16 -> ByteString -> Get FloatElem
getF 1 = return . ClientID . decode
getF 2 = return . ClientPassword . decode
getF 3 = return . ClientSignature . decode
getF 4 = return . AgentExtension . decode
getF 5 = return . AgentID . decode
getF 6 = return . AgentInstrument . decode
getF 7 = return . FloatFieldText . decode
getF 8 = return . Ani . decode
getF 9 = return . UserToUserInfo . decode
getF 10 = return . Dnis . decode
getF 11 = return . DialedNumber . decode
getF 12 = return . CallerEnteredDigit . decode
getF 13 = return . CallVar1 . decode
getF 14 = return . CallVar2 . decode
getF 15 = return . CallVar3 . decode
getF 16 = return . CallVar4 . decode
getF 17 = return . CallVar5 . decode
getF 18 = return . CallVar6 . decode
getF 19 = return . CallVar7 . decode
getF 20 = return . CallVar8 . decode
getF 21 = return . CallVar9 . decode
getF 22 = return . CallVar10 . decode
getF 23 = return . CtiClientSignature . decode
getF 24 = return . CtiClientTimestamp . decode
getF 25 = return . ConnectionDevID . decode
getF 26 = return . AlertingDevID . decode
getF 27 = return . CallingDevID . decode
getF 28 = return . CalledDevID . decode
getF 29 = return . LastRedirectDevID . decode
getF 30 = return . AnsweringDevID . decode
getF 31 = return . HoldingDevID . decode
getF 32 = return . RetrievingDevID . decode
getF 33 = return . ReleasingDevID . decode
getF 34 = return . FailingDevID . decode
getF 35 = return . PrimaryDevID . decode
getF 36 = return . SecondaryDevID . decode
getF 37 = return . ControllerDevID . decode
getF 38 = return . AddedPartyDevID . decode
getF 39 = return . ConnectedPartyCallID . decode
getF 40 = return . ConnectedPartyDevIDType . decode
getF 41 = return . ConnectedPartyDevID . decode
getF 42 = return . TransferringDevID . decode
getF 43 = return . TransferredDevID . decode
getF 44 = return . DivertingDevID . decode
getF 45 = return . QueueDevID . decode
getF 46 = return . CallWrapupData . decode
getF 47 = return . NewConnectionDevID . decode
getF 48 = return . TrunkUsedDevID . decode
getF 49 = return . AgentPassword . decode
getF 50 = return . ActiveConnDevID . decode
getF 51 = return . FacilityCode . decode
getF 52 = return . OtherConnDevID . decode
getF 53 = return . HeldConnDevID . decode
getF 54 = return . FloatElemRsv54 . decode
getF 55 = return . FloatElemRsv55 . decode
getF 56 = return . CallConnCallID . decode
getF 57 = return . CallConnDevIDType . decode
getF 58 = return . CallConnDevID . decode
getF 59 = return . CallDevIDType . decode
getF 60 = return . CallDevID . decode
getF 61 = return . CallDevConnState . decode
getF 62 = return . SkillGroupNumberF . decode
getF 63 = return . SkillGroupIDF . decode
getF 64 = return . SkillGroupPriority . decode
getF 65 = return . SkillGroupState . decode
getF 66 = return . ObjectName . decode
getF 67 = return . DtmfString . decode
getF 68 = return . PositionID . decode
getF 69 = return . SupervisorID . decode
getF 70 = return . LineHandleF . decode
getF 71 = return . LineTypeF . decode
getF 72 = return . RouterCallKeyDay . decode
getF 73 = return . RouterCallkeyCallID . decode
getF 74 = return . FloatElemRsv74 . decode
getF 75 = return . CallState . decode
getF 76 = return . MonitoredDevID . decode
getF 77 = return . AuthorizationCode . decode
getF 78 = return . AccountCode . decode
getF 79 = return . OriginatingDevID . decode
getF 80 = return . OriginatingLineID . decode
getF 81 = return . ClientAddress . decode
getF 82 = return . NamedVariable . decode
getF 83 = return . NamedArray . decode
getF 84 = return . CallControlTable . decode
getF 85 = return . SupervisorInstrument . decode
getF 86 = return . AtcAgentID . decode
getF 87 = return . AgentFlags . decode
getF 88 = return . AtcAgentState . decode
getF 89 = return . AtcStateDuration . decode
getF 90 = return . AgentConnDevID . decode
getF 91 = return . SupervisorConnDevID . decode
getF 92 = return . ListTeamID . decode
getF 93 = return . DefaultDevicePortAddress . decode
getF 94 = return . ServiceName . decode
getF 95 = return . CustomerPhoneNumber . decode
getF 96 = return . CustomerAccountNumber . decode
getF 97 = return . AppPathID . decode
getF 98 = return . FloatElemRsv98 . decode
getF 99 = return . ScriptSelector . decode
getF 100 = return . ApplicationString1 . decode
getF 101 = return . ApplicationString2 . decode
getF 102 = return . FloatElemRsv102 . decode
getF 103 = return . FloatElemRsv103 . decode
getF 104 = return . FloatElemRsv104 . decode
getF 105 = return . FloatElemRsv105 . decode
getF 106 = return . FloatElemRsv106 . decode
getF 107 = return . FloatElemRsv107 . decode
getF 108 = return . FloatElemRsv108 . decode
getF 109 = return . FloatElemRsv109 . decode
getF 110 = return . RouterCallKeySequenceNum . decode
getF 111 = return . FloatElemRsv111 . decode
getF 112 = return . FloatElemRsv112 . decode
getF 113 = return . FloatElemRsv113 . decode
getF 114 = return . FloatElemRsv114 . decode
getF 115 = return . FloatElemRsv115 . decode
getF 116 = return . FloatElemRsv116 . decode
getF 117 = return . FloatElemRsv117 . decode
getF 118 = return . FloatElemRsv118 . decode
getF 119 = return . FloatElemRsv119 . decode
getF 120 = return . FloatElemRsv120 . decode
getF 121 = return . TrunkNumber . decode
getF 122 = return . TrunkGroupNumber . decode
getF 123 = return . NextAgentState . decode
getF 124 = return . DequeueType . decode
getF 125 = return . SendingAddress . decode
getF 126 = return . SendingPort . decode
getF 127 = return . FloatElemRsv127 . decode
getF 128 = return . FloatElemRsv128 . decode
getF 129 = return . MaxQueued . decode
getF 130 = return . QueueID . decode
getF 131 = return . CustomerID . decode
getF 132 = return . ServiceSkillTargetID . decode
getF 133 = return . PeripheralName . decode
getF 134 = return . Description . decode
getF 135 = return . ServiceMemberID . decode
getF 136 = return . ServiceMemberPriority . decode
getF 137 = return . FirstName . decode
getF 138 = return . LastName . decode
getF 139 = return . SkillGroup . decode
getF 140 = return . FloatElemRsv140 . decode
getF 141 = return . AgentSkillTargetID141 . decode
getF 142 = return . Service . decode
getF 143 = return . FloatElemRsv143 . decode
getF 144 = return . FloatElemRsv144 . decode
getF 145 = return . FloatElemRsv145 . decode
getF 146 = return . FloatElemRsv146 . decode
getF 147 = return . FloatElemRsv147 . decode
getF 148 = return . FloatElemRsv148 . decode
getF 149 = return . FloatElemRsv149 . decode
getF 150 = return . Duration . decode
getF 151 = return . FloatElemRsv151 . decode
getF 152 = return . FloatElemRsv152 . decode
getF 153 = return . FloatElemRsv153 . decode
getF 154 = return . FloatElemRsv154 . decode
getF 155 = return . FloatElemRsv155 . decode
getF 156 = return . FloatElemRsv156 . decode
getF 157 = return . FloatElemRsv157 . decode
getF 158 = return . FloatElemRsv158 . decode
getF 159 = return . FloatElemRsv159 . decode
getF 160 = return . FloatElemRsv160 . decode
getF 161 = return . FloatElemRsv161 . decode
getF 162 = return . FloatElemRsv162 . decode
getF 163 = return . FloatElemRsv163 . decode
getF 164 = return . FloatElemRsv164 . decode
getF 165 = return . FloatElemRsv165 . decode
getF 166 = return . FloatElemRsv166 . decode
getF 167 = return . FloatElemRsv167 . decode
getF 168 = return . FloatElemRsv168 . decode
getF 169 = return . FloatElemRsv169 . decode
getF 170 = return . FloatElemRsv170 . decode
getF 171 = return . FloatElemRsv171 . decode
getF 172 = return . FloatElemRsv172 . decode
getF 173 = return . Extension . decode
getF 174 = return . ServiceLevelThreshold . decode
getF 175 = return . ServiceLevelType . decode
getF 176 = return . ConfigParam . decode
getF 177 = return . ServiceConfigKey . decode
getF 178 = return . SkillGroupConfigKey . decode
getF 179 = return . AgentConfigKey . decode
getF 180 = return . DeviceConfigKey . decode
getF 181 = return . FloatElemRsv181 . decode
getF 182 = return . FloatElemRsv182 . decode
getF 183 = return . RecordTypeF . decode
getF 184 = return . PeripheralNumber . decode
getF 185 = return . AgentSkillTargetID185 . decode
getF 186 = return . NumServiceMembers . decode
getF 187 = return . ServiceMember . decode
getF 188 = return . ServicePriority . decode
getF 189 = return . AgentType . decode
getF 190 = return . LoginID . decode
getF 191 = return . NumSkills . decode
getF 192 = return . SkillGroupSkillTargetID . decode
getF 193 = return . FltServiceID . decode
getF 194 = return . AgentIDLong . decode
getF 195 = return . DeviceTypeF . decode
getF 196 = return . FloatElemRsv196 . decode
getF 197 = return . FloatElemRsv197 . decode
getF 198 = return . Enable . decode
getF 199 = return . DeviceID . decode
getF 200 = return . Timeout . decode
getF 201 = return . CurrentRoute . decode
getF 202 = return . SecondaryConnCallID . decode
getF 203 = return . PriorityQueueNumber . decode
getF 204 = return . TeamName . decode
getF 205 = return . MemberType . decode
getF 206 = return . EventDeviceID . decode
getF 207 = return . LoginName . decode
getF 208 = return . FltPeripheralID . decode
getF 209 = return . CallTypeConfigKey . decode
getF 210 = return . CallTypeID . decode
getF 211 = return . CustomerDefinitionID . decode
getF 212 = return . FltEnterpriseName . decode
getF 213 = return . OldPeripheralNumber . decode
getF 214 = return . OldLoginID . decode
getF 215 = return . AniII . decode
getF 216 = return . FltMRDomainID . decode
getF 217 = return . CtiosCilClientID . decode
getF 218 = return . SilentMonitorStatusF . decode
getF 219 = return . RequestingDevID . decode
getF 220 = return . RequestingDevIDType . decode
getF 221 = return . PreCallInvokeID . decode
getF 222 = return . EnterpriseQueueTime . decode
getF 223 = return . CallReferenceID . decode
getF 224 = return . MultiLineAgnetControl . decode
getF 225 = return . NetworkControlled . decode
getF 226 = return . FloatElemRsv226 . decode
getF 227 = return . FloatElemRsv227 . decode
getF 228 = return . NumPeripherals . decode
getF 229 = return . CoCConnCallID . decode
getF 230 = return . CoCConnDevIDType . decode
getF 231 = return . CoCConnDevID . decode
getF 232 = return . CallOriginatedFrom . decode
getF 233 = return . SetAppDataCallID . decode
getF 234 = return . ClientShareKey . decode
getF 235 = return . FloatElemRsv235 . decode
getF 236 = return . FloatElemRsv236 . decode
getF 237 = return . FloatElemRsv237 . decode
getF 238 = return . FloatElemRsv238 . decode
getF 239 = return . FloatElemRsv239 . decode
getF 240 = return . FloatElemRsv240 . decode
getF 241 = return . FloatElemRsv241 . decode
getF 242 = return . FloatElemRsv242 . decode
getF 243 = return . AgentTeamName . decode
getF 244 = return . Direction . decode
getF 245 = return . Options . decode
getF 246 = return . FltMrdD . decode
getF 247 = return . MediaClassID . decode
getF 248 = return . TaskLife . decode
getF 249 = return . TaskStartTimeout . decode
getF 250 = return . MaxTaskDuration . decode
getF 251 = return . FltInterruptible . decode
getF 252 = return . MaxCallsInQueue . decode
getF 253 = return . MaxCallsInQPreCallType . decode
getF 254 = return . MaxTimeInQueue . decode
getF 255 = return . InternalAgentState . decode
getF 256 = return . FloatElemRsv256 . decode
getF 257 = return . SsoEnabledF . decode
getF 258 = return . FltTaskID . decode
getF 259 = return . FltIcmDisp . decode
getF 260 = return . FltAppDisp . decode




putF :: FloatElem -> Put
putF (ClientID x) = putFloatElem 1 x
putF (ClientPassword x) = putFloatElem 2 x
putF (ClientSignature x) = putFloatElem 3 x
putF (AgentExtension x) = putFloatElem 4 x
putF (AgentID x) = putFloatElem 5 x
putF (AgentInstrument x) = putFloatElem 6 x
putF (FloatFieldText x) = putFloatElem 7 x
putF (Ani x) = putFloatElem 8 x
putF (UserToUserInfo x) = putFloatElem 9 x
putF (Dnis x) = putFloatElem 10 x
putF (DialedNumber x) = putFloatElem 11 x
putF (CallerEnteredDigit x) = putFloatElem 12 x
putF (CallVar1 x) = putFloatElem 13 x
putF (CallVar2 x) = putFloatElem 14 x
putF (CallVar3 x) = putFloatElem 15 x
putF (CallVar4 x) = putFloatElem 16 x
putF (CallVar5 x) = putFloatElem 17 x
putF (CallVar6 x) = putFloatElem 18 x
putF (CallVar7 x) = putFloatElem 19 x
putF (CallVar8 x) = putFloatElem 20 x
putF (CallVar9 x) = putFloatElem 21 x
putF (CallVar10 x) = putFloatElem 22 x
putF (CtiClientSignature x) = putFloatElem 23 x
putF (CtiClientTimestamp x) = putFloatElem 24 x
putF (ConnectionDevID x) = putFloatElem 25 x
putF (AlertingDevID x) = putFloatElem 26 x
putF (CallingDevID x) = putFloatElem 27 x
putF (CalledDevID x) = putFloatElem 28 x
putF (LastRedirectDevID x) = putFloatElem 29 x
putF (AnsweringDevID x) = putFloatElem 30 x
putF (HoldingDevID x) = putFloatElem 31 x
putF (RetrievingDevID x) = putFloatElem 32 x
putF (ReleasingDevID x) = putFloatElem 33 x
putF (FailingDevID x) = putFloatElem 34 x
putF (PrimaryDevID x) = putFloatElem 35 x
putF (SecondaryDevID x) = putFloatElem 36 x
putF (ControllerDevID x) = putFloatElem 37 x
putF (AddedPartyDevID x) = putFloatElem 38 x
putF (ConnectedPartyCallID x) = putFloatElem 39 x
putF (ConnectedPartyDevIDType x) = putFloatElem 40 x
putF (ConnectedPartyDevID x) = putFloatElem 41 x
putF (TransferringDevID x) = putFloatElem 42 x
putF (TransferredDevID x) = putFloatElem 43 x
putF (DivertingDevID x) = putFloatElem 44 x
putF (QueueDevID x) = putFloatElem 45 x
putF (CallWrapupData x) = putFloatElem 46 x
putF (NewConnectionDevID x) = putFloatElem 47 x
putF (TrunkUsedDevID x) = putFloatElem 48 x
putF (AgentPassword x) = putFloatElem 49 x
putF (ActiveConnDevID x) = putFloatElem 50 x
putF (FacilityCode x) = putFloatElem 51 x
putF (OtherConnDevID x) = putFloatElem 52 x
putF (HeldConnDevID x) = putFloatElem 53 x
putF (FloatElemRsv54 x) = putFloatElem 54 x
putF (FloatElemRsv55 x) = putFloatElem 55 x
putF (CallConnCallID x) = putFloatElem 56 x
putF (CallConnDevIDType x) = putFloatElem 57 x
putF (CallConnDevID x) = putFloatElem 58 x
putF (CallDevIDType x) = putFloatElem 59 x
putF (CallDevID x) = putFloatElem 60 x
putF (CallDevConnState x) = putFloatElem 61 x
putF (SkillGroupNumberF x) = putFloatElem 62 x
putF (SkillGroupIDF x) = putFloatElem 63 x
putF (SkillGroupPriority x) = putFloatElem 64 x
putF (SkillGroupState x) = putFloatElem 65 x
putF (ObjectName x) = putFloatElem 66 x
putF (DtmfString x) = putFloatElem 67 x
putF (PositionID x) = putFloatElem 68 x
putF (SupervisorID x) = putFloatElem 69 x
putF (LineHandleF x) = putFloatElem 70 x
putF (LineTypeF x) = putFloatElem 71 x
putF (RouterCallKeyDay x) = putFloatElem 72 x
putF (RouterCallkeyCallID x) = putFloatElem 73 x
putF (FloatElemRsv74 x) = putFloatElem 74 x
putF (CallState x) = putFloatElem 75 x
putF (MonitoredDevID x) = putFloatElem 76 x
putF (AuthorizationCode x) = putFloatElem 77 x
putF (AccountCode x) = putFloatElem 78 x
putF (OriginatingDevID x) = putFloatElem 79 x
putF (OriginatingLineID x) = putFloatElem 80 x
putF (ClientAddress x) = putFloatElem 81 x
putF (NamedVariable x) = putFloatElem 82 x
putF (NamedArray x) = putFloatElem 83 x
putF (CallControlTable x) = putFloatElem 84 x
putF (SupervisorInstrument x) = putFloatElem 85 x
putF (AtcAgentID x) = putFloatElem 86 x
putF (AgentFlags x) = putFloatElem 87 x
putF (AtcAgentState x) = putFloatElem 88 x
putF (AtcStateDuration x) = putFloatElem 89 x
putF (AgentConnDevID x) = putFloatElem 90 x
putF (SupervisorConnDevID x) = putFloatElem 91 x
putF (ListTeamID x) = putFloatElem 92 x
putF (DefaultDevicePortAddress x) = putFloatElem 93 x
putF (ServiceName x) = putFloatElem 94 x
putF (CustomerPhoneNumber x) = putFloatElem 95 x
putF (CustomerAccountNumber x) = putFloatElem 96 x
putF (AppPathID x) = putFloatElem 97 x
putF (FloatElemRsv98 x) = putFloatElem 98 x
putF (ScriptSelector x) = putFloatElem 99 x
putF (ApplicationString1 x) = putFloatElem 100 x
putF (ApplicationString2 x) = putFloatElem 101 x
putF (FloatElemRsv102 x) = putFloatElem 102 x
putF (FloatElemRsv103 x) = putFloatElem 103 x
putF (FloatElemRsv104 x) = putFloatElem 104 x
putF (FloatElemRsv105 x) = putFloatElem 105 x
putF (FloatElemRsv106 x) = putFloatElem 106 x
putF (FloatElemRsv107 x) = putFloatElem 107 x
putF (FloatElemRsv108 x) = putFloatElem 108 x
putF (FloatElemRsv109 x) = putFloatElem 109 x
putF (RouterCallKeySequenceNum x) = putFloatElem 110 x
putF (FloatElemRsv111 x) = putFloatElem 111 x
putF (FloatElemRsv112 x) = putFloatElem 112 x
putF (FloatElemRsv113 x) = putFloatElem 113 x
putF (FloatElemRsv114 x) = putFloatElem 114 x
putF (FloatElemRsv115 x) = putFloatElem 115 x
putF (FloatElemRsv116 x) = putFloatElem 116 x
putF (FloatElemRsv117 x) = putFloatElem 117 x
putF (FloatElemRsv118 x) = putFloatElem 118 x
putF (FloatElemRsv119 x) = putFloatElem 119 x
putF (FloatElemRsv120 x) = putFloatElem 120 x
putF (TrunkNumber x) = putFloatElem 121 x
putF (TrunkGroupNumber x) = putFloatElem 122 x
putF (NextAgentState x) = putFloatElem 123 x
putF (DequeueType x) = putFloatElem 124 x
putF (SendingAddress x) = putFloatElem 125 x
putF (SendingPort x) = putFloatElem 126 x
putF (FloatElemRsv127 x) = putFloatElem 127 x
putF (FloatElemRsv128 x) = putFloatElem 128 x
putF (MaxQueued x) = putFloatElem 129 x
putF (QueueID x) = putFloatElem 130 x
putF (CustomerID x) = putFloatElem 131 x
putF (ServiceSkillTargetID x) = putFloatElem 132 x
putF (PeripheralName x) = putFloatElem 133 x
putF (Description x) = putFloatElem 134 x
putF (ServiceMemberID x) = putFloatElem 135 x
putF (ServiceMemberPriority x) = putFloatElem 136 x
putF (FirstName x) = putFloatElem 137 x
putF (LastName x) = putFloatElem 138 x
putF (SkillGroup x) = putFloatElem 139 x
putF (FloatElemRsv140 x) = putFloatElem 140 x
putF (AgentSkillTargetID141 x) = putFloatElem 141 x
putF (Service x) = putFloatElem 142 x
putF (FloatElemRsv143 x) = putFloatElem 143 x
putF (FloatElemRsv144 x) = putFloatElem 144 x
putF (FloatElemRsv145 x) = putFloatElem 145 x
putF (FloatElemRsv146 x) = putFloatElem 146 x
putF (FloatElemRsv147 x) = putFloatElem 147 x
putF (FloatElemRsv148 x) = putFloatElem 148 x
putF (FloatElemRsv149 x) = putFloatElem 149 x
putF (Duration x) = putFloatElem 150 x
putF (FloatElemRsv151 x) = putFloatElem 151 x
putF (FloatElemRsv152 x) = putFloatElem 152 x
putF (FloatElemRsv153 x) = putFloatElem 153 x
putF (FloatElemRsv154 x) = putFloatElem 154 x
putF (FloatElemRsv155 x) = putFloatElem 155 x
putF (FloatElemRsv156 x) = putFloatElem 156 x
putF (FloatElemRsv157 x) = putFloatElem 157 x
putF (FloatElemRsv158 x) = putFloatElem 158 x
putF (FloatElemRsv159 x) = putFloatElem 159 x
putF (FloatElemRsv160 x) = putFloatElem 160 x
putF (FloatElemRsv161 x) = putFloatElem 161 x
putF (FloatElemRsv162 x) = putFloatElem 162 x
putF (FloatElemRsv163 x) = putFloatElem 163 x
putF (FloatElemRsv164 x) = putFloatElem 164 x
putF (FloatElemRsv165 x) = putFloatElem 165 x
putF (FloatElemRsv166 x) = putFloatElem 166 x
putF (FloatElemRsv167 x) = putFloatElem 167 x
putF (FloatElemRsv168 x) = putFloatElem 168 x
putF (FloatElemRsv169 x) = putFloatElem 169 x
putF (FloatElemRsv170 x) = putFloatElem 170 x
putF (FloatElemRsv171 x) = putFloatElem 171 x
putF (FloatElemRsv172 x) = putFloatElem 172 x
putF (Extension x) = putFloatElem 173 x
putF (ServiceLevelThreshold x) = putFloatElem 174 x
putF (ServiceLevelType x) = putFloatElem 175 x
putF (ConfigParam x) = putFloatElem 176 x
putF (ServiceConfigKey x) = putFloatElem 177 x
putF (SkillGroupConfigKey x) = putFloatElem 178 x
putF (AgentConfigKey x) = putFloatElem 179 x
putF (DeviceConfigKey x) = putFloatElem 180 x
putF (FloatElemRsv181 x) = putFloatElem 181 x
putF (FloatElemRsv182 x) = putFloatElem 182 x
putF (RecordTypeF x) = putFloatElem 183 x
putF (PeripheralNumber x) = putFloatElem 184 x
putF (AgentSkillTargetID185 x) = putFloatElem 185 x
putF (NumServiceMembers x) = putFloatElem 186 x
putF (ServiceMember x) = putFloatElem 187 x
putF (ServicePriority x) = putFloatElem 188 x
putF (AgentType x) = putFloatElem 189 x
putF (LoginID x) = putFloatElem 190 x
putF (NumSkills x) = putFloatElem 191 x
putF (SkillGroupSkillTargetID x) = putFloatElem 192 x
putF (FltServiceID x) = putFloatElem 193 x
putF (AgentIDLong x) = putFloatElem 194 x
putF (DeviceTypeF x) = putFloatElem 195 x
putF (FloatElemRsv196 x) = putFloatElem 196 x
putF (FloatElemRsv197 x) = putFloatElem 197 x
putF (Enable x) = putFloatElem 198 x
putF (DeviceID x) = putFloatElem 199 x
putF (Timeout x) = putFloatElem 200 x
putF (CurrentRoute x) = putFloatElem 201 x
putF (SecondaryConnCallID x) = putFloatElem 202 x
putF (PriorityQueueNumber x) = putFloatElem 203 x
putF (TeamName x) = putFloatElem 204 x
putF (MemberType x) = putFloatElem 205 x
putF (EventDeviceID x) = putFloatElem 206 x
putF (LoginName x) = putFloatElem 207 x
putF (FltPeripheralID x) = putFloatElem 208 x
putF (CallTypeConfigKey x) = putFloatElem 209 x
putF (CallTypeID x) = putFloatElem 210 x
putF (CustomerDefinitionID x) = putFloatElem 211 x
putF (FltEnterpriseName x) = putFloatElem 212 x
putF (OldPeripheralNumber x) = putFloatElem 213 x
putF (OldLoginID x) = putFloatElem 214 x
putF (AniII x) = putFloatElem 215 x
putF (FltMRDomainID x) = putFloatElem 216 x
putF (CtiosCilClientID x) = putFloatElem 217 x
putF (SilentMonitorStatusF x) = putFloatElem 218 x
putF (RequestingDevID x) = putFloatElem 219 x
putF (RequestingDevIDType x) = putFloatElem 220 x
putF (PreCallInvokeID x) = putFloatElem 221 x
putF (EnterpriseQueueTime x) = putFloatElem 222 x
putF (CallReferenceID x) = putFloatElem 223 x
putF (MultiLineAgnetControl x) = putFloatElem 224 x
putF (NetworkControlled x) = putFloatElem 225 x
putF (FloatElemRsv226 x) = putFloatElem 226 x
putF (FloatElemRsv227 x) = putFloatElem 227 x
putF (NumPeripherals x) = putFloatElem 228 x
putF (CoCConnCallID x) = putFloatElem 229 x
putF (CoCConnDevIDType x) = putFloatElem 230 x
putF (CoCConnDevID x) = putFloatElem 231 x
putF (CallOriginatedFrom x) = putFloatElem 232 x
putF (SetAppDataCallID x) = putFloatElem 233 x
putF (ClientShareKey x) = putFloatElem 234 x
putF (FloatElemRsv235 x) = putFloatElem 235 x
putF (FloatElemRsv236 x) = putFloatElem 236 x
putF (FloatElemRsv237 x) = putFloatElem 237 x
putF (FloatElemRsv238 x) = putFloatElem 238 x
putF (FloatElemRsv239 x) = putFloatElem 239 x
putF (FloatElemRsv240 x) = putFloatElem 240 x
putF (FloatElemRsv241 x) = putFloatElem 241 x
putF (FloatElemRsv242 x) = putFloatElem 242 x
putF (AgentTeamName x) = putFloatElem 243 x
putF (Direction x) = putFloatElem 244 x
putF (Options x) = putFloatElem 245 x
putF (FltMrdD x) = putFloatElem 246 x
putF (MediaClassID x) = putFloatElem 247 x
putF (TaskLife x) = putFloatElem 248 x
putF (TaskStartTimeout x) = putFloatElem 249 x
putF (MaxTaskDuration x) = putFloatElem 250 x
putF (FltInterruptible x) = putFloatElem 251 x
putF (MaxCallsInQueue x) = putFloatElem 252 x
putF (MaxCallsInQPreCallType x) = putFloatElem 253 x
putF (MaxTimeInQueue x) = putFloatElem 254 x
putF (InternalAgentState x) = putFloatElem 255 x
putF (FloatElemRsv256 x) = putFloatElem 256 x
putF (SsoEnabledF x) = putFloatElem 257 x
putF (FltTaskID x) = putFloatElem 258 x
putF (FltIcmDisp x) = putFloatElem 259 x
putF (FltAppDisp x) = putFloatElem 260 x
-}
