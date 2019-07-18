module PropTest where

import           Test.QuickCheck

half x = x / 2
-- this property should hold
halfIdentity = (*2) . half

