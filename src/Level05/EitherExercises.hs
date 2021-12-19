-- |
-- These exercises show Haskell\'s way of representing errors. Some other languages tend to use
-- exceptions for such scenarios.
module Level05.EitherExercises where

import Data.Char (toUpper)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

data Person = Person
  { personName :: String,
    personAge :: Int
  }
  deriving (Eq, Show)

-- |
-- ADT for representing errors as values.
data AppError = EmptyName | InvalidAgeValue String | InvalidAgeRange Int deriving (Eq, Show)

-- $eitherexercises
-- In other languages, you may be familiar with using exceptions to handle validation and flow.
-- Haskell does have exceptions, but using them for these reasons is considered bad design.
-- Exceptions are only for exceptional situations!
--
-- Instead we\'re going to look at how to handle validations with 'Either's, which help us
-- represent errors as values. This means that Eithers are referentially transparent. An 'Either'
-- can be a 'Left' value to denote an error, or a 'Right' value to denote a success.
--
-- What is Right is correct; what is not Right is wrong.
--
-- @
-- data Either e a = Left e | Right a
-- @

-- |
-- Implement this function, such that it returns a 'Left' with an 'EmptyName' if the supplied name
-- is empty, or 'Right' if the supplied name is not empty
--
-- >>> getName "Fred"
-- Right "Fred"
--
-- >>> getName ""
-- Left EmptyName
getName :: String -> Either AppError String
getName = undefined

-- |
-- Implement this function, such that:
--
-- - It returns a 'Left' of 'InvalidAgeValue' if the provided age can\'t be converted to an integer
-- - It returns a 'Left' of 'InvalidAgeRange' if the provided age isn\'t between 0 and 120 inclusive
-- - It returns a 'Right' with the 'Int' age if it is valid
--
-- >>> getAge "20"
-- Right 20
--
-- >>> getAge "Fred"
-- Left (InvalidAgeValue "Fred")
--
-- >>> getAge "-1"
-- Left (InvalidAgeRange (-1))
--
-- Hint: Use 'readMaybe' to convert a 'String' to an 'Int'. 'readMaybe' can convert strings to many
-- types. Haskell is smart enough to infer which type you want to convert to based on how you use
-- it.
getAge :: String -> Either AppError Int
getAge = undefined

-- |
-- Implement this function, such that:
--
-- - It returns a 'Right' of a 'Person' if the name and age are valid
-- - It returns a 'Left' of an 'AppError' if either the name or age are invalid
--
-- >>> createPerson "Fred" "32"
-- Right (Person {personName = "Fred", personAge = 32})
--
-- >>> createPerson "" "32"
-- Left EmptyName
--
-- >>> createPerson "Fred" "ThirtyTwo"
-- Left (InvalidAgeValue "ThirtyTwo")
--
-- >>> createPerson "Fred" "150"
-- Left (InvalidAgeRange 150)
--
-- Hint: Use do-notation to sequence the 'Either's from 'getName' and 'getAge'.
createPerson :: String -> String -> Either AppError Person
createPerson name age = undefined

-- |
-- Do the same again using only '=<<' / '>>=' and 'pure'.
createPerson2 :: String -> String -> Either AppError Person
createPerson2 name age = undefined

-- |
-- >>> makeNameUppercase "Fred" "32"
-- Right (Person {personName = "FRED", personAge = 32})
--
-- >>> makeNameUppercase "" "32"
-- Left EmptyName
--
-- >>> makeNameUppercase "Fred" "ThirtyTwo"
-- Left (InvalidAgeValue "ThirtyTwo")
--
-- >>> makeNameUppercase "Fred" "150"
-- Left (InvalidAgeRange 150)
--
-- Hint: Use 'createPerson' and 'fmap' / '<$>'.
makeNameUppercase :: String -> String -> Either AppError Person
makeNameUppercase name age =
  let uppercase = fmap toUpper
      uppercaseName person = person {personName = uppercase (personName person)}
   in undefined

-- |
-- When handling errors, you usually only want to handle them at a single point in your application.
-- That error handler will interpret the AppError ADT into something that's more human readable. You
-- could think of this function as the end of the world for our application, where we are providing
-- feedback to the user on their input.
--
-- >>> createPersonAndShow "Fred" "32"
-- "Fred is 32"
--
-- >>> createPersonAndShow "" "32"
-- "Empty name supplied"
--
-- >>> createPersonAndShow "Fred" "ThirtyTwo"
-- "Invalid age value supplied: ThirtyTwo"
--
-- >>> createPersonAndShow "Fred" "150"
-- "Provided age must be between 0 and 120: 150"
createPersonAndShow :: String -> String -> String
createPersonAndShow name age = undefined

personStringPairs :: [(String, String)]
personStringPairs =
  [ ("Tokyo", "30"),
    ("Moscow", "5o"),
    ("The Professor", "200"),
    ("Berlin", "43"),
    ("Arturo Roman", "-1"),
    ("", "30")
  ]

-- |
-- Define 'validPeople' such that it only contains the 'Person' instances that can be created from
-- 'personStringPairs'.
--
-- >>> validPeople
-- [ Person {personName = "Tokyo", personAge = 30},
--   Person {personName = "Berlin", personAge = 43}
-- ]
--
-- The @mapMaybe@ function can help here. See
-- https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Maybe.html#v:mapMaybe for more
-- information.
--
-- Hint: Use 'fmap' / '<$>' and 'mapMaybe'
validPeople :: [Person]
validPeople =
  let rightToMaybe (Left _) = undefined
      rightToMaybe (Right a) = undefined
   in undefined

-- |
-- Define 'peopleErrors' such that it only contains the errors that occur while processing
-- 'personStringPairs'.
--
-- >>> peopleErrors
-- [InvalidAgeValue "5o",InvalidAgeRange 200,InvalidAgeRange (-1),EmptyName]
peopleErrors :: [AppError]
peopleErrors =
  let leftToMaybe (Left e) = undefined
      leftToMaybe (Right _) = undefined
   in undefined

-- * Bonus Exercises!

-- |
-- There is an operator in Haskelll, '<*>', pronounced \"ap\".
--
-- It has the following type signature (sort of):
--
-- @
-- Either e (a -> b) -> Either e a -> Either e b
-- @
--
-- It takes a function @a -> b@, wrapped in an 'Either' as its first argument.
--
-- Compare this to '<$>', AKA 'fmap':
--
-- @
--          (a -> b) -> Either e a -> Either e b
-- @
--
-- Why is '<*>' useful? To illustrate, let\'s look at the type of the following expression:
--
-- >>> :t Person <$> getName "Foo"
-- Person <$> getName "Foo" :: Either AppError (Int -> Person)
--
-- In other words, we take advantage of currying and use 'fmap' to apply the person\'s name to the
-- 'Person' constructor. This gives us back a function from an age to a 'Person', all wrapped in
-- an 'Either'.
--
-- Is there something we can do to the result of this expression to create a 'Person'?
--
-- Try to reimplement 'createPerson' using only '<$>' and '<*>'.
createPerson3 :: String -> String -> Either AppError Person
createPerson3 name age = undefined
