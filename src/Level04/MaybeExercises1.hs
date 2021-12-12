-- |
-- Here we introduce a new ADT - @Maybe@ for dealing with values that may not exist.
--
-- We will also cover safe constructors, which in conjunction with ADTs, allow us to prevent invalid
-- states from being represented.
--
-- @Maybe@ data type:
--
-- @
-- data Maybe a = Nothing | Just a
-- @
module Level04.MaybeExercises1 where

import Level04

-- |
-- >>> safeMean [1,2,10]
-- Just 4.333333333333333
--
-- >>> safeMean []
-- Nothing
--
-- Hint: Use @sum@, @length@ and convert the numerator or denominator to a @Double@ using
-- @fromIntegral@.
--
-- The division operator, @/@ isn't defined for @Int@.
--
-- >>> (1 :: Int) / 2
-- No instance for (Fractional Int) arising from a use of ‘/’
--
-- >>> fromIntegral (1 :: Int) / 2
-- 0.5
safeMean :: [Int] -> Maybe Double
safeMean = undefined

-- * Safe Constructors

--
-- Allows us to convert input from the real world (e.g. files, HTTP request, etc.) into ADTs.

-- |
-- >>> mkTrafficLight "red"
-- Just Red
--
-- >>> mkTrafficLight "yellow"
-- Just Yellow
--
-- >>> mkTrafficLight "green"
-- Just Green
--
-- >>> mkTrafficLight "bob"
-- Nothing
--
-- Hint: Use pattern matching.
mkTrafficLight :: String -> Maybe TrafficLight
mkTrafficLight = undefined

-- |
-- >>> mkPerson "Bob" 20
-- Just (Person {personName = "Bob", personAge = 20})
--
-- If @name@ is blank:
--
-- >>> mkPerson "" 20
-- Nothing
--
-- If @age@ < 0:
--
-- >>> mkPerson "Bob" (-1)
-- Nothing
--
-- Hint: Don't forget every @if@ needs an @else@!
mkPerson :: String -> Int -> Maybe Person
mkPerson = undefined

-- |
-- >>> mkPersonThenReverseName "John" 20
-- Just (Person {personName = "nhoJ", personAge = 20})
--
-- >>> mkPersonThenReverseName "John" (-1)
-- Nothing
--
-- >>> mkPersonThenReverseName "" 20
-- Nothing
--
-- Hint: Use @mkPerson@, @reverseName@ and pattern matching.
mkPersonThenReverseName :: String -> Int -> Maybe Person
mkPersonThenReverseName = undefined

reverseName :: Person -> Person
reverseName person = person {personName = reverse $ personName person}
