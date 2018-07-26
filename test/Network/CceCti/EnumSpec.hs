{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.CceCti.EnumSpec where

import           Control.Exception.Base (evaluate)
import           Data.Binary (encode, decode, decodeOrFail)
import           Data.Binary.Get (ByteOffset)
import           Data.ByteString.Lazy (ByteString, pack, unpack)
import           Data.Either (isLeft, isRight)
import           Data.List (elem, isInfixOf)
import           Data.Maybe (isJust)
import           Data.Typeable (typeOf)
import           Data.Word (Word16)

import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.ADT

import           Network.CceCti


instance Arbitrary AgentState where
    arbitrary = genericArbitrary
instance ToADTArbitrary AgentState

instance (Arbitrary a) => Arbitrary (CtiOpt a) where
    arbitrary = genericArbitrary
instance (Arbitrary a) => ToADTArbitrary (CtiOpt a)

instance Arbitrary CallType where
    arbitrary = genericArbitrary
instance ToADTArbitrary CallType

instance Arbitrary ControlFailureCode where
    arbitrary = genericArbitrary
instance ToADTArbitrary ControlFailureCode

instance Arbitrary DeviceIDType where
    arbitrary = genericArbitrary
instance ToADTArbitrary DeviceIDType

instance Arbitrary FailureIndicationMessageStatusCode where
    arbitrary = genericArbitrary
instance ToADTArbitrary FailureIndicationMessageStatusCode

instance Arbitrary (SafeEnum FailureIndicationMessageStatusCode) where
    arbitrary = genericArbitrary
instance ToADTArbitrary (SafeEnum FailureIndicationMessageStatusCode)


spec :: Spec
spec = do
    describe "AgentState" $ do
        it "Index of AgentStateLogin should be zero" $
            fromEnum AgentStateLogin `shouldBe` 0

        it "Index of AgentStateNotActive should be 14" $
            fromEnum AgentStateNotActive `shouldBe` 14

        it "AgentStateLogout encodes to [0x00, 0x01]" $
            encode AgentStateLogout `shouldBe` pack [0,1]

        it "decodes [0x00, 0x0d] to AgentStateInterrupted" $
            decode (pack [0,13]) `shouldBe` AgentStateInterrupted

        prop "(decoce . encode) shouldbe id" $ \(n :: AgentState) ->
            (decode . encode) n == n

        it "fails decoding if unrecognized Enum index is found in given binary" $ do
            let x = decodeOrFail (pack [1,0]) :: Either (ByteString, ByteOffset, String) (ByteString, ByteOffset, AgentState)
            x `shouldSatisfy` isLeft

        it "fails and returns error string on unrecognized Enum index in given binary" $ do
            let (Left (_, _, s)) = decodeOrFail (pack [1,0]) :: Either (ByteString, ByteOffset, String) (ByteString, ByteOffset, AgentState)
            s `shouldSatisfy` ("256" `isInfixOf`)
            s `shouldSatisfy` ("AgentState" `isInfixOf`)
            s `shouldSatisfy` ("(0,14)" `isInfixOf`)

    describe "SafeEnum" $ do
        it "decodes Right when enumeration is successfully decoded" $
            (decode (pack [0,1]) :: (SafeEnum AgentState)) `shouldBe` SafeEnum (Right AgentStateLogout)

        it "decodes Left when given bytes is decoded outside of enumeration range" $ do
            let (SafeEnum x) = decode (pack [0,15]) :: SafeEnum AgentState
            x `shouldSatisfy` isLeft

        it "encodes enumeration wrapped by Right" $
            encode (SafeEnum (Right AgentStateTalking)) `shouldBe` pack [0,4]

        it "throws error when it has Left value" $
            evaluate (encode (SafeEnum (Left "don't care") :: SafeEnum AgentState))
                `shouldThrow` errorCall "SafeEnum containing Left cannot be used for encoding message"

    describe "SafeEnum AgentState" $ do
        prop "decodes Right from [0,0] to [0,14]" $ forAll (choose (0,14)) $ \n -> do
            let (SafeEnum x) = decode (pack [0,n]) :: SafeEnum AgentState
            x `shouldSatisfy` isRight

        prop "decodes Left from [0,15] to [0xff,0xff]" $ forAll (choose (15, 0xffff :: Word16)) $ \n -> do
            let upper = fromIntegral $ n `div` 256
                lower = fromIntegral $ n `mod` 256
                (SafeEnum x) = decode (pack [upper, lower]) :: SafeEnum AgentState
            x `shouldSatisfy` isLeft
            let (Left s) = x
            s `shouldSatisfy` (show n `isInfixOf`)
            s `shouldSatisfy` ("AgentState" `isInfixOf`)
            s `shouldSatisfy` ("(0,14)" `isInfixOf`)

        prop "encodes any Right AgentState value" $ \(n :: AgentState) ->
            ((!!1) . unpack . encode . SafeEnum . Right) n `shouldSatisfy` (\b -> 0 <= b && b <= 14)

    describe "SafeEnum (CtiOpt CallType)" $ do
        it "Index of CallTypeZero should be zero" $
            fromEnum CallTypeZero `shouldBe` 0

        it "Index of CallTypeVoiceCallBack should be 41" $
            fromEnum CallTypeVoiceCallBack `shouldBe` 41

        prop "CallType encodes to range from [0,0] to [0,41]" $ \(n :: CallType) ->
            ((!!1) . unpack . encode) n `shouldSatisfy` (\b -> 0 <= b && b <= 41)

        prop "CallType decodes from range from [0,0] to [0,41]" $ forAll (choose (0, 41)) $ \n ->
            decode (pack [0,n]) `shouldSatisfy` (`elem` [minBound..maxBound :: CallType])

        it "decode CallType throws error when given binary has unrecognized Enum index" $
            evaluate (decode (pack [0,42]) :: CallType) `shouldThrow` anyErrorCall

        prop "CtiOpt CallType with Just value encodes to range [0,0] to [0,41]" $ \(n :: CallType) ->
            ((!!1) . unpack . encode . CtiOpt . Just) n `shouldSatisfy` (\b -> 0 <= b && b <= 41)

        prop "CtiOpt CallType decodes Just from binary in [0,0] to [0,41]" $ forAll (choose (0, 41)) $ \n -> do
            let (CtiOpt cc) = decode (pack [0,n]) :: CtiOpt CallType
            cc `shouldSatisfy` isJust
            let (Just c) = cc
            c `shouldSatisfy` (`elem` [minBound..maxBound :: CallType])

        it "CtiOpt CallType encodes Nothing to [0xff,0xff]" $
            encode (CtiOpt Nothing :: CtiOpt CallType) `shouldBe` pack [0xff,0xff]

        it "CtiOpt CallType decodes Nothing from [0xff,0xff]" $
            (decode (pack [0xff,0xff]) :: CtiOpt CallType) `shouldBe` (CtiOpt Nothing :: CtiOpt CallType)

        it "decode CtiOpt CallType throws error when given binary has unrecognized Enum index" $
            evaluate (decode (pack [0,42]) :: CtiOpt CallType) `shouldThrow` anyErrorCall

        it "fails decoding if unrecognized Enum index is found in given binary" $ do
            let x = decodeOrFail (pack [0,42]) :: Either (ByteString, ByteOffset, String) (ByteString, ByteOffset, CtiOpt CallType)
            x `shouldSatisfy` isLeft

        it "fails and returns error string on unrecognized Enum index in given binary" $ do
            let (Left (_, _, s)) = decodeOrFail (pack [0,42]) :: Either (ByteString, ByteOffset, String) (ByteString, ByteOffset, CtiOpt CallType)
            s `shouldSatisfy` ("42" `isInfixOf`)
            s `shouldSatisfy` ("CallType" `isInfixOf`)
            s `shouldSatisfy` ("(0,41)" `isInfixOf`)

        prop "encodes Right to range [0,0] to [0,41]" $ \(n :: CallType) ->
            ((!!1) . unpack . encode . SafeEnum . Right . CtiOpt . Just) n `shouldSatisfy` (\b -> 0 <= b && b <= 41)

        prop "decodes Right from range [0,0] to [0,41]" $ forAll (choose (0, 41)) $ \n -> do
            let (SafeEnum (Right (CtiOpt (Just c)))) = decode (pack [0,n])
            c `shouldSatisfy` (`elem` [minBound..maxBound :: CallType])

        it "decodes Right Nothing from [0xff,0xff]" $
            decode (pack [0xff,0xff]) `shouldBe` (SafeEnum (Right (CtiOpt Nothing)) :: SafeEnum (CtiOpt CallType))

        prop "decodes Left from unrecognized Enum index" $ forAll (choose (42, 0xfffe :: Word16)) $ \n -> do
            let b1 = fromIntegral (n `div` 0x100)
                b2 = fromIntegral (n `mod` 0x100)
                (SafeEnum (Left s)) = decode (pack [b1,b2]) :: SafeEnum (CtiOpt CallType)
            s `shouldSatisfy` (show n `isInfixOf`)
            s `shouldSatisfy` ("CallType" `isInfixOf`)
            s `shouldSatisfy` ("(0,41)" `isInfixOf`)

    describe "SafeEnum ControlFailureCode" $ do
        it "Index of CFGenericUnspecified should be zero" $
            fromEnum CFGenericUnspecified `shouldBe` 0

        it "Index of CFRequestsOnDeviceExceededRejection should be 79" $
            fromEnum CFRequestsOnDeviceExceededRejection `shouldBe` 79

        it "Index of CFInvalidAgentIDSpecified should be 256" $
            fromEnum CFInvalidAgentIDSpecified `shouldBe` 256

        it "Index of CFExtensionNotUnique should be 293" $
            fromEnum CFExtensionNotUnique `shouldBe` 293

        it "Index of CFUnknownInterfaceControllerID should be 1001" $
            fromEnum CFUnknownInterfaceControllerID `shouldBe` 1001

        it "Index of CFAgentGreetingControlOperationFailure should be 1016" $
            fromEnum CFAgentGreetingControlOperationFailure `shouldBe` 1016

        it "Index 1 should be converted to CFGenericOperation" $
            toEnum 1 `shouldBe` CFGenericOperation

        it "Index 78 should be converted to CFRequestTimeoutRejection" $
            toEnum 78 `shouldBe` CFRequestTimeoutRejection

        it "Index 257 should be converted to CFInvalidPasswordSpecified" $
            toEnum 257 `shouldBe` CFInvalidPasswordSpecified

        it "Index 292 should be converted to CFSharedLinesNotSupported" $
            toEnum 292 `shouldBe` CFSharedLinesNotSupported

        it "Index 1002 should be converted to CFInvalidInterfaceControllerID" $
            toEnum 1002 `shouldBe` CFInvalidInterfaceControllerID

        it "Index 1015 should be converted to CFRCServiceInactivatePim" $
            toEnum 1015 `shouldBe` CFRCServiceInactivatePim

        it "encodes CFGenericUnspecified to [0,0]" $
            encode CFGenericUnspecified `shouldBe` pack [0,0]

        it "encodes CFRequestsOnDeviceExceededRejection to [0,79]" $
            encode CFRequestsOnDeviceExceededRejection `shouldBe` pack [0,79]

        it "encodes CFInvalidAgentIDSpecified to [1,0]" $
            encode CFInvalidAgentIDSpecified `shouldBe` pack [1,0]

        it "encodes CFExtensionNotUnique to [0x1, 0x25]" $
            encode CFExtensionNotUnique `shouldBe` pack [0x01,0x25]

        it "encodes CFUnknownInterfaceControllerID to [0x03, 0xe9]" $
            encode CFUnknownInterfaceControllerID `shouldBe` pack [0x03,0xe9]

        it "encodes CFAgentGreetingControlOperationFailure to [0x03, 0xf8]" $
            encode CFAgentGreetingControlOperationFailure `shouldBe` pack [0x03,0xf8]

        it "decodes [0,1] to SafeEnum (Right CFGenericOperation)" $
            decode (pack [0,1]) `shouldBe` SafeEnum (Right CFGenericOperation)

        it "decodes [0,78] to SafeEnum (Right CFRequestTimeoutRejection)" $
            decode (pack [0,78]) `shouldBe` SafeEnum (Right CFRequestTimeoutRejection)

        it "decodes [0x01,0x01] to SafeEnum (Right CFInvalidPasswordSpecified)" $
            decode (pack [0x01,0x01]) `shouldBe` SafeEnum (Right CFInvalidPasswordSpecified)

        it "decodes [0x01,0x24] to SafeEnum (Right CFSharedLinesNotSupported)" $
            decode (pack [0x01,0x24]) `shouldBe` SafeEnum (Right CFSharedLinesNotSupported)

        it "decodes [0x03,0xea] to SafeEnum (Right CFInvalidInterfaceControllerID)" $
            decode (pack [0x03,0xea]) `shouldBe` SafeEnum (Right CFInvalidInterfaceControllerID)

        it "decodes [0x03,0xf7] to SafeEnum (Right CFRCServiceInactivatePim)" $
            decode (pack [0x03,0xf7]) `shouldBe` SafeEnum (Right CFRCServiceInactivatePim)

        it "fails to decode from index 80 to raw ControlFailureCode" $ do
            let (Left (_, _, s)) = decodeOrFail (pack [0,80]) :: Either (ByteString, ByteOffset, String) (ByteString, ByteOffset, ControlFailureCode)
            s `shouldSatisfy` ("not a member of ControlFailureCode" `isInfixOf`)

        it "decodes Left from index 80 to SafeEnum ControlFailureCode" $ do
            let (SafeEnum x) = decode (pack [0,80]) :: SafeEnum ControlFailureCode
            x `shouldSatisfy` isLeft
            let (Left s) = x
            s `shouldSatisfy` ("not a member of ControlFailureCode" `isInfixOf`)

        prop "(decode . encode) should be id" $ \(n :: ControlFailureCode) -> do
            (decode . encode) n `shouldBe` n
            (decode . encode) (SafeEnum (Right n)) `shouldBe` SafeEnum (Right n)

        prop "decodes range from [0,0] to [0,79]" $ forAll (choose (0, 79)) $ \n ->
            encode (decode (pack [0,n]) :: SafeEnum ControlFailureCode) `shouldBe` pack [0,n]

        prop "decodes range from [0x01,0x00] to [0x01,0x25]" $ forAll (choose (256, 293 :: Word16)) $ \n ->
            encode (decode (encode n) :: SafeEnum ControlFailureCode) `shouldBe` encode n

        prop "decodes range from [0x03,0xe9] to [0x03,0xf8]" $ forAll (choose (1001, 1016 :: Word16)) $ \n ->
            encode (decode (encode n) :: SafeEnum ControlFailureCode) `shouldBe` encode n

        prop "decodes Left from [0,81] to [0,255]" $ forAll (choose (80, 255)) $ \n -> do
            let (SafeEnum x) = decode (pack [0,n]) :: SafeEnum ControlFailureCode
            x `shouldSatisfy` isLeft

        prop "decodes Left from [0x01,0x26] to [0x03,0xe8]" $ forAll (choose (294, 1000 :: Word16)) $ \n -> do
            let (SafeEnum x) = decode (encode n) :: SafeEnum ControlFailureCode
            x `shouldSatisfy` isLeft

        prop "decodes Left from [0x03,0xf9] to [0xff,0xff]" $ forAll (choose (1017, 0xffff :: Word16)) $ \n -> do
            let (SafeEnum x) = decode (encode n) :: SafeEnum ControlFailureCode
            x `shouldSatisfy` isLeft

    describe "SafeEnum (CtiOpt DeviceIDType)" $ do
        it "Index of DevIDDevice should be zero" $
            fromEnum DevIDDevice `shouldBe` 0

        it "Index of DevIDTrunk should be 70" $
            fromEnum DevIDTrunk `shouldBe` 70

        it "Index of DevIDSharedDevice should be 79" $
            fromEnum DevIDSharedDevice `shouldBe` 79

        it "Index 71 should be converted to DevIDTrunkGroup" $
            toEnum 71 `shouldBe` DevIDTrunkGroup

        it "Encoded DevIDDevice should be [0x00, 0x00]" $
            encode DevIDDevice `shouldBe` pack [0,0]

        it "Encoded DevIDTrunk should be [0x00, 0x46]" $
            encode DevIDTrunk `shouldBe` pack [0,70]

        prop "DeviceIDType decodes from range from [0,70] to [0,79]" $ forAll (choose (70, 79)) $ \n ->
            encode (decode (pack [0,n]) :: DeviceIDType) `shouldBe` pack [0,n]

        it "decode DeviceIDType fails when given binary has unrecognized Enum index" $ do
            let (Left (_, _, s)) = decodeOrFail (pack [0,1]) :: Either (ByteString, ByteOffset, String) (ByteString, ByteOffset, DeviceIDType)
            s `shouldSatisfy` ("not a member of DeviceIDType" `isInfixOf`)

        it "Encoded CtiOpt (Just DevIDDevice) should be [0x00, 0x00]" $
            encode (CtiOpt (Just DevIDDevice)) `shouldBe` pack [0,0]

        it "Encoded CtiOpt Nothing :: CtiOpt DeviceIDType should be [0xff, 0xff]" $
            encode (CtiOpt Nothing :: CtiOpt DeviceIDType) `shouldBe` pack [0xff, 0xff]

        it "decodes [0x00,0x4f] to CtiOpt (Just DevIDSharedDevice)" $
            decode (pack [0,79]) `shouldBe` CtiOpt (Just DevIDSharedDevice)

        it "decodes [0xff,0xff] to CtiOpt Nothing :: CtiOpt DeviceIDType" $
            decode (pack [0xff,0xff])  `shouldBe` (CtiOpt Nothing :: CtiOpt DeviceIDType)

        it "decode CtiOpt DeviceIDType fails when given binary has unrecognized Enum index" $ do
            let (Left (_, _, s)) = decodeOrFail (pack [0,1]) :: Either (ByteString, ByteOffset, String) (ByteString, ByteOffset, CtiOpt DeviceIDType)
            s `shouldSatisfy` ("not a member of DeviceIDType" `isInfixOf`)

        it "Encoded SafeEnum (Rignt (CtiOpt (Just DevIDIPPhoneMac))) should be [0x00, 0x48]" $
            encode (SafeEnum (Right (CtiOpt (Just DevIDIPPhoneMac)))) `shouldBe` pack [0,72]

        it "Encoded SafeEnum (Right (CtiOpt Nothing :: CtiOpt DeviceIDType)) should be [0xff, 0xff]" $
            encode (SafeEnum (Right (CtiOpt Nothing :: CtiOpt DeviceIDType))) `shouldBe` pack [0xff,0xff]

        it "decodes [0x00,0x49] to SafeEnum (Right (CtiOpt (Just DevIDCtiPort)))" $
            decode (pack [0,79]) `shouldBe` SafeEnum (Right (CtiOpt (Just DevIDSharedDevice)))

        it "decodes [0xff,0xff] to SafeEnum (Right (CtiOpt Nothing))" $
            decode (pack [0xff,0xff]) `shouldBe` SafeEnum (Right (CtiOpt Nothing :: CtiOpt DeviceIDType))

        it "decodes unrecognized index to SafeEnum Left :: SafeEnum (CtiOpt DeviceIDType)" $ do
            let (SafeEnum x) = decode (pack [0,1]) :: SafeEnum (CtiOpt DeviceIDType)
            x `shouldSatisfy` isLeft
            let (Left s) = x
            s `shouldSatisfy` ("not a member of DeviceIDType" `isInfixOf`)

        prop "decodes Left from [0,70] to [0,79]" $ forAll (choose (70,79)) $ \n -> do
            let (SafeEnum x) = decode (pack [0,n]) :: SafeEnum (CtiOpt DeviceIDType)
            x `shouldSatisfy` isRight

        prop "decodes Left from [0,1] to [0,69]" $ forAll (choose (1,69)) $ \n -> do
            let (SafeEnum x) = decode (pack [0,n]) :: SafeEnum (CtiOpt DeviceIDType)
            x `shouldSatisfy` isLeft

        prop "decodes Left from [0,80] to [0xff,0xfe]" $ forAll (choose (80, 0xfffe :: Word16)) $ \n -> do
            let upper = fromIntegral $ n `div` 256
                lower = fromIntegral $ n `mod` 256
                (SafeEnum x) = decode (pack [upper, lower]) :: SafeEnum (CtiOpt DeviceIDType)
            x `shouldSatisfy` isLeft

        prop "(decode . encode) should be id" $ \(n :: CtiOpt DeviceIDType) ->
            (decode . encode) (SafeEnum (Right n)) == SafeEnum (Right n)

    describe "FailureIndicationMessageStatusCode" $ do
        it "Index of ECtiNoError should be zero" $
            fromEnum ECtiNoError `shouldBe` 0

        it "Index of ECtiInvalidAgentWorkMode should be 50" $
            fromEnum ECtiInvalidAgentWorkMode `shouldBe` 50

        it "Index of ECtiInvalidRequestType should be 98" $
            fromEnum ECtiInvalidRequestType `shouldBe` 98

        it "Encoded ECtiNoError should be [0x00, 0x00, 0x00, 0x00]" $
            encode ECtiNoError `shouldBe` pack [0,0,0,0]

        it "Encoded ECtiInvalidRequestType should be [0x00, 0x00, 0x01, 0x62]" $
            encode ECtiInvalidRequestType `shouldBe` pack [0,0,0,98]

        prop "(decode . encode) should be id" $ \(n :: FailureIndicationMessageStatusCode) ->
            (decode . encode) n `shouldBe` n

    describe "CtiOpt PeripheralType" $ do
        it "Index of PeripheralTypeAspect should be 1" $
            fromEnum PeripheralTypeAspect `shouldBe` 1

        it "Index of PeripheralTypeGeneric should be 20" $
            fromEnum PeripheralTypeGeneric `shouldBe` 20

        it "Encoded Some PeripheralTypeGeneric should be [0x00, 0x14]" $
            encode (CtiOpt (Just PeripheralTypeGeneric)) `shouldBe` pack [0,20]

        it "Encoded None :: PeripheralType should be [0xff, 0xff]" $
            encode (CtiOpt Nothing :: CtiOpt PeripheralType) `shouldBe` pack [0xff, 0xff]

        it "decodes [0x00,0x13] to Some PeripheralTypeMediaRouting" $
            decode (pack [0,19]) `shouldBe` CtiOpt (Just PeripheralTypeMediaRouting)

        it "decodes [0xff,0xff] to None :: CtiOpt PeripheralType" $
            decode (pack [0xff,0xff])  `shouldBe` (CtiOpt Nothing :: CtiOpt PeripheralType)
