module Test where

import AnotherModule

data TestType = TestType Int


hello :: TestType -> TestType
hello (TestType x) = TestType (x+1)

aValue :: TestType
aValue = TestType 5

anotherValue = ThisIsAType
