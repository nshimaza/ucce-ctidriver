#!/usr/bin/env bash
TARGET=FixedPart.hs

cat << EOF > $TARGET
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

EOF

echo 'data FixedPart' >> $TARGET

cat MessageRec.hs | grep '^data' |\
sed 's/data \([A-Za-z0-9][A-Za-z0-9]*\)R.*--  *\([0-9][0-9]*\).*$/\1,\1R,-- \2/' |\
column -t -s ',' | sed 's/^/    = /' | head -n 1 >> $TARGET

cat MessageRec.hs | grep '^data' |\
sed 's/data \([A-Za-z0-9][A-Za-z0-9]*\)R.*--  *\([0-9][0-9]*\).*$/\1,\1R,-- \2/' |\
column -t -s ',' | sed 's/^/    | /' | tail -n '+2' >> $TARGET

echo '    deriving (Eq, Generic, Show)' >> $TARGET

echo >> $TARGET


echo 'getR :: Word32 -> Get FixedPart' >> $TARGET
cat MessageRec.hs | grep '^data' |\
sed 's/data \([A-Za-z0-9][A-Za-z0-9]*\)R.*--  *\([0-9][0-9]*\).*$/getR \2 = \1 <$> get/' >> $TARGET

echo >> $TARGET

echo 'putR :: FixedPart -> Put' >> $TARGET
cat MessageRec.hs | grep '^data' |\
sed 's/data \([A-Za-z0-9][A-Za-z0-9]*\)R.*--  *\([0-9][0-9]*\).*$/putR (\1 x) = putFixedPart \2 x/' >> $TARGET
