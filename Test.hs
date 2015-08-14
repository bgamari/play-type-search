module Test where

import AnotherModule

data TestType = TestType Int
data Ident a = Ident a

mapIdent :: (a -> b) -> Ident a -> Ident b
mapIdent f (Ident a) = Ident (f a)

hello :: TestType -> TestType
hello (TestType x) = TestType (x+1)

aValue :: TestType
aValue = TestType 5

anotherValue = ThisIsAType
