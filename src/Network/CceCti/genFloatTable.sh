#!/usr/bin/env bash
TARGET=FloatElem.hs

cat << EOF > $TARGET
{-# LANGUAGE DeriveGeneric #-}

module Network.CceCti.FloatElem where

import           Prelude                hiding (length)

import           Data.Binary            (Binary, decode, encode, get, put,
                                         putList)
import           Data.Binary.Get        (Get, getLazyByteString, getWord16be)
import           Data.Binary.Put        (Put, putLazyByteString, putWord16be)
import           Data.ByteString.Lazy   (ByteString, length)
import           Data.Word              (Word16, Word32, Word8)
import           GHC.Generics           (Generic)

import           Network.CceCti.BitMask
import           Network.CceCti.Enum
import           Network.CceCti.Types


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

EOF

echo 'data FloatElem' >> $TARGET

cat FloatElem.txt |\
sed 's/  *--.*//' | sed 's/\([0-9][0-9]*\),  *\([A-Za-z0-9][A-Za-z0-9]*\),  *\(.*$\)/\2,\3,-- \1/' |\
column -t -s ',' | sed 's/^/    = /' | head -n 1 >> $TARGET


cat FloatElem.txt |\
sed 's/  *--.*//' | sed 's/\([0-9][0-9]*\),  *\([A-Za-z0-9][A-Za-z0-9]*\),  *\(.*$\)/\2,\3,-- \1/' |\
column -t -s ',' | sed 's/^/    | /' | tail -n '+2' >> $TARGET

echo '    deriving (Eq, Generic, Show)' >> $TARGET


echo >> $TARGET

echo 'getF :: Word16 -> ByteString -> Get FloatElem' >> $TARGET
cat FloatElem.txt | grep -v '^--' |\
sed 's/\([0-9][0-9]*\),  *\([A-Za-z0-9][A-Za-z0-9]*\).*$/getF \1 = return . \2 . decode/' >> $TARGET

echo >> $TARGET

echo 'putF :: FloatElem -> Put' >> $TARGET
cat FloatElem.txt | grep -v '^--' |\
sed 's/\([0-9][0-9]*\),  *\([A-Za-z0-9][A-Za-z0-9]*\).*$/putF (\2 x) = putFloatElem \1 x/' >> $TARGET
