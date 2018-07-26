{-# LANGUAGE FlexibleInstances #-}

module Network.CceCti.Arbitrary where

import           Network.CceCti
--import           Network.CceCti.Types
--import           Network.CceCti.Message
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.ADT


-- BitMasks

{-
instance Arbitrary AgentStateMask where
    arbitrary = genericArbitrary
instance ToADTArbitrary AgentStateMask

instance Arbitrary CallVariableMask where
    arbitrary = genericArbitrary
instance ToADTArbitrary CallVariableMask
-}


{-
-- Types with special value
instance Arbitrary PeripheralID where
    arbitrary = genericArbitrary
instance ToADTArbitrary PeripheralID
-}


-- Types

{-
instance Arbitrary InvokeID where
    arbitrary = genericArbitrary
instance ToADTArbitrary InvokeID
-}

{-
instance Arbitrary MessageType where
    arbitrary = genericArbitrary
instance ToADTArbitrary MessageType

instance Arbitrary (SafeEnum MessageType) where
    arbitrary = genericArbitrary
instance ToADTArbitrary (SafeEnum MessageType)

instance Arbitrary FloatTag where
    arbitrary = genericArbitrary
instance ToADTArbitrary FloatTag



-- Messages

instance Arbitrary MessageFailureConf where
    arbitrary = genericArbitrary
instance ToADTArbitrary MessageFailureConf

instance Arbitrary ParsedFailureConf where
    arbitrary = genericArbitrary
instance ToADTArbitrary ParsedFailureConf

instance Arbitrary MessageFailureEvent where
    arbitrary = genericArbitrary
instance ToADTArbitrary MessageFailureEvent

instance Arbitrary ParsedFailureEvent where
    arbitrary = genericArbitrary
instance ToADTArbitrary ParsedFailureEvent
-}
