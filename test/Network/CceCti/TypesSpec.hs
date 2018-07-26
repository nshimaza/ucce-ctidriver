{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.CceCti.TypesSpec where

import           Prelude hiding (length)

import           Data.Binary
import           Data.ByteString.Lazy
import           Data.Either

import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.ADT

import           Network.CceCti hiding (empty)


instance (Arbitrary a) => Arbitrary (CtiOpt a) where
    arbitrary = genericArbitrary
instance (Arbitrary a) => ToADTArbitrary (CtiOpt a)

instance Arbitrary PeripheralID where
    arbitrary = genericArbitrary
instance ToADTArbitrary PeripheralID

instance Arbitrary LineHandle where
    arbitrary = genericArbitrary
instance ToADTArbitrary LineHandle


spec :: Spec
spec = do
    describe "InvokeID" $ do
        it "Encoded InvokeID 0 should be [0x00, 0x00, 0x00, 0x00]" $
            encode (InvokeID 0) `shouldBe` pack [0,0,0,0]

        it "Encoded InvokeID 2560 should be [0x00, 0x00, 0x0a, 0x00]" $
            encode (InvokeID 2560) `shouldBe` pack [0,0,10,0]

        it "Encoded InvokeID 0xffffffff should be [0xff, 0xff, 0xff, 0xff]" $
            encode (InvokeID 0xffffffff) `shouldBe` pack [0xff,0xff,0xff,0xff]

        it "should decode [0x00, 0x00, 0x00, 0x00] to InvokeID 0" $
            decode (pack [0,0,0,0]) `shouldBe` InvokeID 0

        it "should decode [0x00, 0x00, 0x0a, 0x00] to InvokeID 256" $
            decode (pack [0,0,10,0]) `shouldBe` InvokeID 2560

        it "should decode [0xff, 0xff, 0xff, 0xff] to InvokeID 0xffffffff" $
            decode (pack [0xff,0xff,0xff,0xff]) `shouldBe` InvokeID 0xffffffff

        prop "(decode . encode) should be id" $ \(n :: Word32) ->
            (decode . encode . InvokeID) n == InvokeID n


    describe "LineHandle" $ do
        it "Encoded LineHandle (CtiOpt (Just 0)) should be [0x00, 0x00]" $
            encode (LineHandle (CtiOpt (Just 0))) `shouldBe` pack [0,0]

        it "Encoded LineHandle (CtiOpt (Just 2560)) should be [0x0a, 0x00]" $
            encode (LineHandle (CtiOpt (Just 2560))) `shouldBe` pack [10,0]

        it "Encoded LineHandle (CtiOpt Nothing) should be [0xff, 0xff]" $
            encode (LineHandle (CtiOpt Nothing)) `shouldBe` pack [0xff,0xff]

        it "should decode [0x00, 0x00] to LineHandle (CtiOpt (Just 0))" $
            decode (pack [0,0]) `shouldBe` LineHandle (CtiOpt (Just 0))

        it "should decode [0x0a, 0x00] to LineHandle (CtiOpt (Just 2560))" $
            decode (pack [10,0]) `shouldBe` LineHandle (CtiOpt (Just 2560))

        it "should decode [0xff, 0xff] to LineHandle (CtiOpt Nothing)" $
            decode (pack [0xff,0xff]) `shouldBe` LineHandle (CtiOpt Nothing)

        prop "(decode . encode) should be id" $ forAll (choose (0, 0xfffe :: Word16)) $ \n ->
            (decode . encode . LineHandle . CtiOpt . Just) n `shouldBe` (LineHandle . CtiOpt . Just) n

    describe "SkillGroupPriority" $ do
        it "Encoded SkillGroupPriority (CtiOpt0 (Just 1)) should be [0x00, 0x01]" $
            encode (SkillGroupPriority (CtiOpt0 (Just 1))) `shouldBe` pack [0,1]

        it "Encoded SkillGroupPriority (CtiOpt0 (Just 2560)) should be [0x0a, 0x00]" $
            encode (SkillGroupPriority (CtiOpt0 (Just 2560))) `shouldBe` pack [10,0]

        it "Encoded SkillGroupPriority (CtiOpt0 Nothing) should be [0x00, 0x00]" $
            encode (SkillGroupPriority (CtiOpt0 Nothing)) `shouldBe` pack [0,0]

        it "should decode [0x00, 0x01] to SkillGroupPriority (CtiOpt (Just 1))" $
            decode (pack [0,1]) `shouldBe` SkillGroupPriority (CtiOpt0 (Just 1))

        it "should decode [0x0a, 0x00] to SkillGroupPriority (CtiOpt (Just 2560))" $
            decode (pack [10,0]) `shouldBe` SkillGroupPriority (CtiOpt0 (Just 2560))

        it "should decode [0x00, 0x00] to SkillGroupPriority (CtiOpt Nothing)" $
            decode (pack [0,0]) `shouldBe` SkillGroupPriority (CtiOpt0 Nothing)

        prop "(decode . encode) should be id" $ forAll (choose (1, 0xffff :: Word16)) $ \n ->
            (decode . encode . SkillGroupPriority . CtiOpt0 . Just) n `shouldBe` (SkillGroupPriority . CtiOpt0 . Just) n

    describe "PeripheralID" $ do
        it "Encoded PeripheralID (CtiOpt (Just 0)) should be [0x00, 0x00, 0x00, 0x00]" $
            encode (PeripheralID (CtiOpt (Just 0))) `shouldBe` pack [0,0,0,0]

        it "Encoded PeripheralID (CtiOpt (Just 2560)) should be [0x00, 0x00, 0x0a, 0x00]" $
            encode (PeripheralID (CtiOpt (Just 2560))) `shouldBe` pack [0,0,10,0]

        it "Encoded PeripheralID (CtiOpt Nothing) should be [0xff, 0xff, 0xff, 0xff]" $
            encode (PeripheralID (CtiOpt Nothing)) `shouldBe` pack [0xff,0xff,0xff,0xff]

        it "should decode [0x00, 0x00, 0x00, 0x00] to PeripheralID (CtiOpt (Just 0))" $
            decode (pack [0,0,0,0]) `shouldBe` PeripheralID (CtiOpt (Just 0))

        it "should decode [0x00, 0x00, 0x0a, 0x00] to PeripheralID (CtiOpt (Just 2560))" $
            decode (pack [0,0,10,0]) `shouldBe` PeripheralID (CtiOpt (Just 2560))

        it "should decode [0xff, 0xff, 0xff, 0xff] to PeripheralID (CtiOpt Nothing)" $
            decode (pack [0xff,0xff,0xff,0xff]) `shouldBe` PeripheralID (CtiOpt Nothing)

        prop "(decode . encode) should be id" $ \(n :: PeripheralID) ->
            (decode . encode) n == n

    describe "Unspec8" $ do
        it "encodes up to 8 bytes of ByteString in as is form" $
            encode (Unspec8 (pack [1,2,3,4,5,6,7,8])) `shouldBe` pack [1,2,3,4,5,6,7,8]

        it "encodes ByteString shorter than 8 bytes" $
            encode (Unspec8 (pack [4,3,2,1])) `shouldBe` pack [4,3,2,1]

        it "encodes zero length source" $
            encode (Unspec8 empty) `shouldBe` pack []

        it "encodes bytes containing zero" $
            encode (Unspec8 (pack [1,2,0,4,5])) `shouldBe` pack [1,2,0,4,5]

        it "truncates bytes longer than 8" $
            encode (Unspec8 (pack [1,2,3,4,5,6,7,8,9,10])) `shouldBe` pack [1,2,3,4,5,6,7,8]

        it "decodes entire content of given ByteString" $
            decode (pack [0,3,1,2,3,4,5,6]) `shouldBe` Unspec8 (pack [0,3,1,2,3,4,5,6])

        it "decodes zero length input" $
            decode empty `shouldBe` Unspec8 empty

        prop "Length of encoded Unspec8 is shorter or equal to 8" $ \(ws :: [Word8]) ->
            (length . encode . Unspec8 . pack) ws <= 8

    describe "Unspec32" $ do
        it "truncates bytes longer than 32" $
            encode (Unspec32 (pack [  1, 2, 3, 4, 5, 6, 7, 8, 9,10
                                   , 11,12,13,14,15,16,17,18,19,20
                                   , 21,22,23,24,25,26,27,28,29,30
                                   , 31,32,33,34,35,36,37,38,39,40 ]))
                `shouldBe` pack [  1, 2, 3, 4, 5, 6, 7, 8, 9,10
                                , 11,12,13,14,15,16,17,18,19,20
                                , 21,22,23,24,25,26,27,28,29,30
                                , 31,32 ]

        prop "Length of encoded Unspec32 is shorter or equal to 32" $ \(ws :: [Word8]) ->
            (length . encode . Unspec32 . pack) ws <= 32

    describe "String4" $ do
        it "encodes NUL-terminated string into up to 4 bytes of NUL-terminated ByteString" $
            encode (String4 (pack [1,2,3,0])) `shouldBe` pack [1,2,3,0]

        it "encodes shorter string than limit" $
            encode (String4 (pack [1,2,0])) `shouldBe` pack [1,2,0]

        it "doesn't encode after NUL terminator" $
            encode (String4 (pack [1,2,0,4])) `shouldBe` pack [1,2,0]

        it "adds NUL to encoded ByteString if it is not present in source string" $
            encode (String4 (pack [1,2,3])) `shouldBe` pack [1,2,3,0]

        it "encodes zero length source" $
            encode (String4 empty) `shouldBe` pack [0]

        it "truncates and add NUL if source string is too long" $
            encode (String4 (pack [1,2,3,4,5,6,0])) `shouldBe` pack [1,2,3,0]

        it "decodes NUL-terminated bytes in input without terminating NUL" $
            decode (pack [1,2,3,0]) `shouldBe` String4 (pack [1,2,3])

        it "decodes string longer than 4 bytes" $
            decode (pack [1,2,3,4,5,6,7,0]) `shouldBe` String4 (pack [1,2,3,4,5,6,7])

        it "decodes bytes until NUL from given input" $
            decode (pack [1,2,0,4,5,6]) `shouldBe` String4 (pack [1,2])

        it "it decodes entire input if NUL is not present" $
            decode (pack [1,2,3,4,5]) `shouldBe` String4 (pack [1,2,3,4,5])

        it "decodes zero length input" $
            decode empty `shouldBe` String4 empty

    describe "NamedVar" $ do
        it "encodes name and value into packed NUL terminated ByteString" $
            encode (NamedVar (pack [1,2]) (pack [3,4])) `shouldBe` pack [1,2,0,3,4,0]

        it "encodes empty value" $
            encode (NamedVar (pack [1,2]) empty) `shouldBe` pack [1,2,0,0]

        it "encodes empty name" $
            encode (NamedVar empty (pack [3,4])) `shouldBe` pack [0,3,4,0]

        it "encodes empty name and value" $
            encode (NamedVar empty empty) `shouldBe` pack [0,0]

        it "decodes name and value from packed NUL terminated ByteString" $
            decode (pack [9,8,0,6,5,0]) `shouldBe` NamedVar (pack [9,8]) (pack [6,5])

        it "decodes empty value" $
            decode (pack [9,8,0,0]) `shouldBe` NamedVar (pack [9,8]) empty

        it "decodes empty name" $
            decode (pack [0,7,6,0]) `shouldBe` NamedVar empty (pack [7,6])

        it "decodes empty name and value" $
            decode (pack [0,0]) `shouldBe` NamedVar empty empty

        it "decodes single NUL-terminated input to name and empty value" $
            decode (pack [9,8,0]) `shouldBe` NamedVar (pack [9,8]) empty

        it "decodes non-NUL-terminated input to name and empty value" $
            decode (pack [5,4,3,2]) `shouldBe` NamedVar (pack [5,4,3,2]) empty

        it "decodes input containing only one NUL" $
            decode (pack [7,6,0,4,3]) `shouldBe` NamedVar (pack [7,6]) (pack [4,3])

        it "decodes NUL only input to empty name and empty value" $
            decode (pack [0]) `shouldBe` NamedVar empty empty

        it "decodes completely empty input to empty name and empty value" $
            decode (pack []) `shouldBe` NamedVar empty empty

    describe "NamedArr" $ do
        it "encodes index, name and value into Word8 and packed NUL terminated ByteString" $
            encode (NamedArr 9 (pack [1,2]) (pack [3,4])) `shouldBe` pack [9,1,2,0,3,4,0]

        it "encodes empty name and value" $
            encode (NamedArr 8 empty empty) `shouldBe` pack [8,0,0]

        it "decodes name and value from packed NUL terminated ByteString" $
            decode (pack [1,9,8,0,6,5,0]) `shouldBe` NamedArr 1 (pack [9,8]) (pack [6,5])

        it "decodes index only input to empty name and empty value" $
            decode (pack [1]) `shouldBe` NamedArr 1 empty empty



{-
    describe "MessageType" $ do
        it "MessageTypeZero should be zero" $
            fromEnum MessageTypeZero `shouldBe` 0

        it "ClientSessionClosedEvent should be 100" $
            fromEnum ClientSessionClosedEvent `shouldBe` 100

        it "TaskDataUpdateEvent should be 200" $
            fromEnum TaskDataUpdateEvent `shouldBe` 200

        it "ReservedMessageType259 should be 259" $
            fromEnum ReservedMessageType259 `shouldBe` 259

        it "Encoded MessageTypeZero should be [0x00, 0x00, 0x00, 0x00]" $
            encode MessageTypeZero `shouldBe` pack [0,0,0,0]

        it "Encoded ReservedMessageType259 should be [0x00, 0x00, 0x01, 0x03]" $
            encode ReservedMessageType259 `shouldBe` pack [0,0,1,3]

        prop "(decode . encode) should be id" $ \(n :: MessageType) ->
            (decode . encode) n == n

    describe "FloatTag" $ do
        it "ClientIdTag should be one" $
            fromEnum ClientIdTag `shouldBe` 1

        it "ApplicationString1Tag should be 100" $
            fromEnum ApplicationString1Tag `shouldBe` 100

        it "TimeoutTag should be 200" $
            fromEnum TimeoutTag `shouldBe` 200

        it "FltAppDispTag should be 260" $
            fromEnum FltAppDispTag `shouldBe` 260

        it "Encoded ClientIdTag should be [0x00, 0x01]" $
            encode ClientIdTag `shouldBe` pack [0,1]

        it "Encoded FltAppDispTag should be [0x01, 0x04]" $
            encode FltAppDispTag `shouldBe` pack [1,4]

        prop "(decode . encode) should be id" $ \(n :: FloatTag) ->
            (decode . encode) n == n




-}
