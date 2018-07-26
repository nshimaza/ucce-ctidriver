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
