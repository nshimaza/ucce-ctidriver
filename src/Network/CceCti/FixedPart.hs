{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Network.CceCti.FixedPart where

import           Data.Binary               (Binary, get, put)
import           Data.Binary.Get           (Get, getWord32be)
import           Data.Binary.Put           (Put, putWord32be)
import           Data.Monoid               ((<>))
import           Data.Word                 (Word32)
import           GHC.Generics              (Generic)

import           Network.CceCti.FloatPart
import           Network.CceCti.MessageRec

data Message = Message FixedPart FloatPart deriving (Binary, Eq, Generic, Show)

instance Binary FixedPart where
    get = getFixedPart
    put = putR

getFixedPart :: Get FixedPart
getFixedPart =  getWord32be >>= getR

putFixedPart :: Binary a => Word32 -> a -> Put
putFixedPart msgType x = putWord32be msgType <> put x

data FixedPart
    = FailureConf                 FailureConfR                 -- 1
    | FailureEvent                FailureEventR                -- 2
    | OpenReq                     OpenReqR                     -- 3
    | OpenConf                    OpenConfR                    -- 4
    | HeartBeatReq                HeartBeatReqR                -- 5
    | HeartBeatConf               HeartBeatConfR               -- 6
    | CloseReq                    CloseReqR                    -- 7
    | CloseConf                   CloseConfR                   -- 8
    | CallDeliveredEvent          CallDeliveredEventR          -- 9
    | CallEstablishedEvent        CallEstablishedEventR        -- 10
    | CallHeldEvent               CallHeldEventR               -- 11
    | CallRetrievedEvent          CallRetrievedEventR          -- 12
    | CallClearedEvent            CallClearedEventR            -- 13
    | CallConnectionClearedEvent  CallConnectionClearedEventR  -- 14
    | CallOriginatedEvent         CallOriginatedEventR         -- 15
    | CallFailedEvent             CallFailedEventR             -- 16
    | CallConferencedEvent        CallConferencedEventR        -- 17
    | CallTransferredEvent        CallTransferredEventR        -- 18
    | CallDivertedEvent           CallDivertedEventR           -- 19
    | CallServiceInitiatedEvent   CallServiceInitiatedEventR   -- 20
    | CallQueuedEvent             CallQueuedEventR             -- 21
    | CallTranslationRouteEvent   CallTranslationRouteEventR   -- 22
    | BeginCallEvent              BeginCallEventR              -- 23
    | EndCallEvent                EndCallEventR                -- 24
    | CallDataUpdateEvent         CallDataUpdateEventR         -- 25
    | SetCallDataReq              SetCallDataReqR              -- 26
    | SetCallDataConf             SetCallDataConfR             -- 27
    | ReleaseCallReq              ReleaseCallReqR              -- 28
    | ReleaseCallConf             ReleaseCallConfR             -- 29
    | AgentStateEvent             AgentStateEventR             -- 30
    | SystemEvent                 SystemEventR                 -- 31
    | ClientEventReportReq        ClientEventReportReqR        -- 32
    | ClientEventReportConf       ClientEventReportConfR       -- 33
    | CallReachedNetworkEvent     CallReachedNetworkEventR     -- 34
    | ControlFailureConf          ControlFailureConfR          -- 35
    deriving (Eq, Generic, Show)

getR :: Word32 -> Get FixedPart
getR 1 = FailureConf <$> get
getR 2 = FailureEvent <$> get
getR 3 = OpenReq <$> get
getR 4 = OpenConf <$> get
getR 5 = HeartBeatReq <$> get
getR 6 = HeartBeatConf <$> get
getR 7 = CloseReq <$> get
getR 8 = CloseConf <$> get
getR 9 = CallDeliveredEvent <$> get
getR 10 = CallEstablishedEvent <$> get
getR 11 = CallHeldEvent <$> get
getR 12 = CallRetrievedEvent <$> get
getR 13 = CallClearedEvent <$> get
getR 14 = CallConnectionClearedEvent <$> get
getR 15 = CallOriginatedEvent <$> get
getR 16 = CallFailedEvent <$> get
getR 17 = CallConferencedEvent <$> get
getR 18 = CallTransferredEvent <$> get
getR 19 = CallDivertedEvent <$> get
getR 20 = CallServiceInitiatedEvent <$> get
getR 21 = CallQueuedEvent <$> get
getR 22 = CallTranslationRouteEvent <$> get
getR 23 = BeginCallEvent <$> get
getR 24 = EndCallEvent <$> get
getR 25 = CallDataUpdateEvent <$> get
getR 26 = SetCallDataReq <$> get
getR 27 = SetCallDataConf <$> get
getR 28 = ReleaseCallReq <$> get
getR 29 = ReleaseCallConf <$> get
getR 30 = AgentStateEvent <$> get
getR 31 = SystemEvent <$> get
getR 32 = ClientEventReportReq <$> get
getR 33 = ClientEventReportConf <$> get
getR 34 = CallReachedNetworkEvent <$> get
getR 35 = ControlFailureConf <$> get

putR :: FixedPart -> Put
putR (FailureConf x) = putFixedPart 1 x
putR (FailureEvent x) = putFixedPart 2 x
putR (OpenReq x) = putFixedPart 3 x
putR (OpenConf x) = putFixedPart 4 x
putR (HeartBeatReq x) = putFixedPart 5 x
putR (HeartBeatConf x) = putFixedPart 6 x
putR (CloseReq x) = putFixedPart 7 x
putR (CloseConf x) = putFixedPart 8 x
putR (CallDeliveredEvent x) = putFixedPart 9 x
putR (CallEstablishedEvent x) = putFixedPart 10 x
putR (CallHeldEvent x) = putFixedPart 11 x
putR (CallRetrievedEvent x) = putFixedPart 12 x
putR (CallClearedEvent x) = putFixedPart 13 x
putR (CallConnectionClearedEvent x) = putFixedPart 14 x
putR (CallOriginatedEvent x) = putFixedPart 15 x
putR (CallFailedEvent x) = putFixedPart 16 x
putR (CallConferencedEvent x) = putFixedPart 17 x
putR (CallTransferredEvent x) = putFixedPart 18 x
putR (CallDivertedEvent x) = putFixedPart 19 x
putR (CallServiceInitiatedEvent x) = putFixedPart 20 x
putR (CallQueuedEvent x) = putFixedPart 21 x
putR (CallTranslationRouteEvent x) = putFixedPart 22 x
putR (BeginCallEvent x) = putFixedPart 23 x
putR (EndCallEvent x) = putFixedPart 24 x
putR (CallDataUpdateEvent x) = putFixedPart 25 x
putR (SetCallDataReq x) = putFixedPart 26 x
putR (SetCallDataConf x) = putFixedPart 27 x
putR (ReleaseCallReq x) = putFixedPart 28 x
putR (ReleaseCallConf x) = putFixedPart 29 x
putR (AgentStateEvent x) = putFixedPart 30 x
putR (SystemEvent x) = putFixedPart 31 x
putR (ClientEventReportReq x) = putFixedPart 32 x
putR (ClientEventReportConf x) = putFixedPart 33 x
putR (CallReachedNetworkEvent x) = putFixedPart 34 x
putR (ControlFailureConf x) = putFixedPart 35 x
