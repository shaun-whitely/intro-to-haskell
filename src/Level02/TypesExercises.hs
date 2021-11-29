-- |
-- These exercises introduce data types and also algebraic data types (ADTs). ADTs are a huge part
-- of typed functional programming. You will also be introduced to a very useful technique for
-- working with ADTs, i.e. pattern matching.
module Level02.TypesExercises where

-- * Section 1 - People

-- |
-- A simple data type
--
-- Here is an example of a `Person` type, which is a wrapper on `String` and `Int`.
--
-- This is a "product type", i.e. a `Person` is a "product" of `String` and `Int`.
--
-- We're using record syntax to also create accessor functions for the person's parameters. We
-- could have also written the definition like so:
--
-- >>> data Person = Person String Int deriving (Eq, Show)
data Person = Person {personName :: String, personAge :: Int} deriving (Eq, Show)

-- |
-- An instance of a person using simple syntax.
person1 :: Person
person1 = Person "John Kane" 35

-- |
-- An instance of a person using record syntax.
person2 :: Person
person2 = Person {personName = "John Kane", personAge = 35}

-- |
-- All data types in Haskell are immutable. However, you can "update" an instance of a data type.
-- Doing so will not modify the existing instance, but will return a new instance with the modified
-- value.
person3 :: Person
person3 = person2 {personAge = 40}

-- |
-- >>> person = Person "Bob" 50
-- >>> showPerson1 person
-- "Bob is 50 years old"
--
-- This uses a technique called pattern matching. You will see more of this later.
--
-- You can read the following code as:
--
-- "If @person@ matches the @Person@ constructor, return the expression to the right hand side of
-- the @=@
--
-- The expression on the right hand side has access to the @name@ and @age@
-- "extracted" out of @person@.
--
-- Hint: Use @show@ to convert an Int to a String.
showPerson1 :: Person -> String
showPerson1 (Person name age) = undefined <> " is " <> undefined <> " years old"

-- |
-- Same as showPerson1, but using accessors instead.
--
-- Hint: Use @show@ to convert an Int to a String.
showPerson2 :: Person -> String
showPerson2 person = undefined <> " is " <> undefined <> " years old"

-- |
-- Write a function that changes the age of a person.
--
-- >>> person = Person "Bob" 50
-- >>> changeAge 51 person
-- Person {personName = "Bob", personAge = 51}
--
-- @person@ is immutable! This function returns a new instance of @Person@ with the @age@ changed.
-- Check out the corresponding test in @TypesExercisesSpec@ to understand why.
changeAge :: Int -> Person -> Person
changeAge = undefined

-- * Section 2 - Wallet

-- |
-- Let's look at another data type.
--
-- @Wallet@ is a tiny type on `Double` to represent the amount of money someone has.
--
-- We could have also used the @data@ keyword, but @newtype@ creates a thin wrapper around a type
-- that incurs no extra runtime cost.
newtype Wallet = Wallet Double deriving (Eq, Show)

-- |
-- >>> wallet = Wallet 20.5
-- >>> showWallet wallet
-- "The wallet amount is 20.5"
--
-- You can solve this like how you solved @showPerson1@.
showWallet :: Wallet -> String
showWallet = undefined

-- |
-- Here is another example of working with immutable values.
--
-- >>> wallet = Wallet 100
-- >>> purchase 80 wallet
-- Wallet 20.0
purchase :: Double -> Wallet -> Wallet
purchase = undefined

-- * Section 3 - Test Driven Development

--
-- >>> showTrafficLightStr "red"
-- "The traffic light is red"
--
-- >>> showTrafficLightStr "yellow"
-- "The traffic light is yellow"
--
-- >>> showTrafficLightStr "green"
-- "The traffic light is green"
--
-- What if @trafficLight@ is not "red", "yellow" or "green"?
--
-- Go to @TypesExercisesSpec.hs@ and implement the test for this scenario:
--
-- "returns a default on other inputs"
--
-- return "The traffic light is invalid" for other inputs.

-- * Section 4 - Traffic Light as String

-- |
-- Implement the following showTrafficLightStr function to pass all your tests!
showTrafficLightStr :: String -> String
showTrafficLightStr = undefined

-- ** Section 4.1 - Adding a new Traffic Light (using TDD)

--
-- We need to have a new traffic light called Flashing:
--
-- 1. Implement the test for this scenario: "should show flashing"
-- it should return "the traffic light is flashing"
-- 2. Extend @showTrafficLightStr@ that you have just implemented above to support this new
-- flashing functionality.
--
-- >>> showTrafficLightStr "flashing"
-- "The traffic light is flashing"

-- * Section 5 - Traffic Light as an ADT

-- |
-- A "sum type" represents more than one possible value.
--
-- You can read the following as a `TrafficLight` is either `Red` or `Yellow` or `Green`.
--
-- This technique helps you make invalid states/values irrepresentable in your programs
data TrafficLight = Red | Yellow | Green deriving (Eq, Show)

-- |
-- >>> showTrafficLight Red
-- "The traffic light is red"
--
-- >>> showTrafficLight Yellow
-- "The traffic light is yellow"
--
-- >>> showTrafficLight Green
-- "The traffic light is green"
--
-- It is impossible to get an invalid TrafficLight as input.
--
-- Hint: Use pattern matching.
showTrafficLight :: TrafficLight -> String
showTrafficLight = undefined

-- ** Section 5.1 - Add a new Traffic Light

--
-- Now introduce a new type of @TrafficLight@ called @Flashing@.
--
-- 1. Add a new value Flashing to the @TrafficLight@ Type
-- 2. Try compile. What happens? How is this different than the previous String implementation?
-- 3. Extend @showTrafficLight@ to fix the compilation error.
-- 4. Fill in the unit test for this new scenario: "showTrafficLight should show Flashing"