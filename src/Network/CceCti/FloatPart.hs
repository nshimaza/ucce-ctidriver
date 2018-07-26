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
