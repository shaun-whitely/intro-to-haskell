-- |
-- Level 1 focuses on basic Haskell, including syntax and especially programming with functions.
-- We will go through a few exercises to familiarise ourselves with writing Haskell.
--
-- What\'s a function?
--
-- A function takes inputs and returns an output.
-- It should always return the same output given the same inputs.
module Level01.IntroExercises where

-- |
-- This function called 'add' takes two ints and returns an int.
--
-- In Haskell, we write the function\'s type signature on the line above its definition.
-- If you leave out the type signature, Haskell will infer the type of the function for us the
-- implementation.
--
-- All functions in Haskell are curried functions. You can think of 'add' as a function that takes
-- an int and returns a function from int to int.
-- >>> add 1 2
-- 3
--
-- >>> let add1 = add 1
-- >>> add1 2
-- 3
add :: Int -> Int -> Int
add = undefined

-- |
-- Reuse the 'add' function and partially apply it for adding 5 to anything.
--
-- >>> add5 4
-- 9
add5 :: Int -> Int
add5 = undefined

-- * Parametric Types

-- |
-- How many ways can you implement this function?
--
-- Note: Concrete types start with an uppercase letter. Type parameters start with a lowercase
-- letter.
foo :: a -> a
foo = undefined

-- |
-- How about this one?
bar :: Int -> Int
bar = undefined

-- |
-- >>> timesTwoIfEven 4
-- 8
--
-- >>> timesTwoIfEven 3
-- 3
--
-- Hint: use the built-in 'even' function.
timesTwoIfEven :: Int -> Int
timesTwoIfEven = undefined

-- |
-- >>> showNumber 100
-- "The number is 100"
--
-- - Hint 1: Use '<>' to concatenate strings.
-- - Hint 2: Use 'show' to convert an int to a String.
showNumber :: Int -> String
showNumber = undefined

-- * Tuples

-- |
-- How can we group together @name@ and @age@ in a pair?
pair :: String -> Int -> (String, Int)
pair name age = undefined

-- |
-- How can we extract the first element of a pair?
--
-- - Hint 1: Search Hoogle for @+base (String, Int) -> String@. @+base@ limits the search to the
-- standard library.
-- - Hint 2: Read up on pattern matching:
-- https://en.wikibooks.org/wiki/Haskell/Pattern_matching#Tuple_constructors
getName :: (String, Int) -> String
getName = undefined

-- |
-- How can we extract the second element of a pair?
getAge :: (String, Int) -> Int
getAge = undefined
