-- |
-- These exercises will teach you how to work with lists in Haskell.
--
-- At the end of these exercises, you should have a good intuition on when to use @fmap@, @filter@
-- or @foldr@.
module Level03.ListExercises where

-- A list in Haskell is a linked list. It has two constructors:
--
-- - @[]@: the empty list
-- - @(:)@: the "cons" operator
--
-- Using the "cons" operator and the empty list:
--
-- >>> 1:2:3:[]
-- [1,2,3]
--
-- Using the "cons" operator in the prefix position (not recommended):
--
-- >>> (:) 1 ((:) 2 ((:) 3 []))
-- [1,2,3]
--
-- Using syntactic sugar:
--
-- >>> [1,2,3]
-- [1,2,3]

-- |
-- >>> prependToList 1 [2,3,4]
-- [1,2,3,4]
--
-- Remember that a type starting with a lowercase letter is a type parameter.
prependToList :: a -> [a] -> [a]
prependToList = undefined

-- |
-- >>> appendToList 1 [2,3,4]
-- [2,3,4,1]
--
-- - Hint 1: You can concatenate two lists with @<>@.
-- - Hint 2: You can construct a singleton list like so: @[a]@.
--
-- Appending to a list (especially a large one) is discouraged. Why might that be?
appendToList :: a -> [a] -> [a]
appendToList = undefined

-- |
-- Haskell's built-in @null@ function returns whether a list is empty or not.
--
-- For this exercise, let's build a version of @null@ called @isEmptyList@ without using @null@
-- (that would be cheating!).
--
-- >>> isEmptyList []
-- True
--
-- >>> isEmptyList [1,2,3]
-- False
--
-- Hint: Use pattern matching. You can pattern match on a list using its two constructors: @[]@ and
-- @(:)@.
isEmptyList :: [a] -> Bool
isEmptyList = undefined

-- |
-- >>> showListSize [1,2,3]
-- "This is a list of size 3"
--
-- >>> showListSize ["abc"]
-- "This is a list of size 1"
--
-- >>> showListSize []
-- "This is an empty list"
--
-- Hint: Use pattern matching, @<>@ to concatenate strings, and the @length@ function.
showListSize :: [a] -> String
showListSize = undefined

-- |
-- Mapping a function over a list
--
-- This is typically what you want if the initial list and the resulting list are the same length.
--
-- >>> addNumToEach 10 [1,2,3]
-- [11,12,13]
--
-- Remember: A lambda expression can be written like so:
--
-- >>> \x -> x + 1
--
-- This is equivalent to:
--
-- >>> (+1)
--
-- Hint: Use @fmap@
addNumToEach1 :: Int -> [Int] -> [Int]
addNumToEach1 = undefined

-- |
-- Same as @addNumToEach1@, but using the infix @<$>@ operator.
addNumToEach2 :: Int -> [Int] -> [Int]
addNumToEach2 = undefined

-- |
-- Filter a list
--
-- This is typically what you want if the length of the resulting list is <= that of the initial.
--
-- >>> filterEven [1,2,3,4]
-- [2,4]
--
-- Hint: Use @filter@ and @even@.
filterEven :: [Int] -> [Int]
filterEven = undefined

-- * Folds

--
-- A fold is an operation over a data structure to yield a summary value.
--
-- The next 3 exercises are to practise folding.
--
-- Examples: sum, product, min, max
--
-- Hint: Use the @foldr@ function.

-- |
-- >>> productOfNums [2,5,3]
-- 30
--
-- https://en.wikipedia.org/wiki/Empty_product
--
-- >>> productOfNums []
-- 1
--
-- Haskell already has a built-in @product@ function. Don't use it for this exercise (that would
-- be cheating!).
--
-- Hint: Use @foldr@
productOfNums :: [Int] -> Int
productOfNums = undefined

-- |
-- >>> minOfNums [4,6,1]
-- 1
--
-- >>> minOfNums []
-- -9223372036854775808
minOfNums :: [Int] -> Int
minOfNums [] = minBound -- the smallest possible Int
minOfNums (x : xs) = undefined

data Person = Person {personName :: String, personAge :: Int} deriving (Eq, Show)

peopleList :: [Person]
peopleList =
  [ Person "Matt Murdock" 30,
    Person "Karen Page" 27,
    Person "Franklin 'Foggy' Nelson" 31,
    Person "Claire Temple" 32,
    Person "Wilson Fisk" 42,
    Person "Elektra Natchios" 27
  ]

-- |
-- Return the person in the List that is the youngest. If there is more than one person with the
-- youngest age, return the first one.
--
-- >>> youngestPerson peopleList
-- Person {personName = "Karen Page", personAge = 27}
--
-- >>> youngestPerson []
-- Person {personName = "Nobody", personAge = 0}
youngestPerson :: [Person] -> Person
youngestPerson = undefined

-- |
-- Return a list of pairs of a Person and their position in the list. The position should be a
-- 1-based index.
--
-- >>> personWithIndex [Person "A" 1, Person "B" 2]
-- [(Person {personName = "A", personAge = 1},1),(Person {personName = "B", personAge = 2},2)]
--
-- You can take advantage of the laziness of lists in Haskell to construct an infinite list.
--
-- >>> take 20 [1..]
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
--
-- You can combine corresponding elements of two lists into tuples using @zip@. The lists will be
-- zipped together until either one reaches the end. The length of the resulting list is the length
-- of the shorter of the two lists
--
-- >>> zip [1,2,3,4,5,6,7,8,9] ["alpha", "beta", "gamma", "delta", "epsilon"]
-- [(1,"alpha"),(2,"beta"),(3,"gamma"),(4,"delta"),(5,"epsilon")]
personWithIndex :: [Person] -> [(Person, Int)]
personWithIndex = undefined

-- |
-- Log every nth Person in a list, given an index "n".
--
-- >>> showEveryNthPerson 2 peopleList
-- ["Karen Page is 27 years old","Claire Temple is 32 years old","Elektra Natchios is 27 years old"]
--
-- >>> showEveryNthPerson 7 peopleList
-- []
--
-- >>> showEveryNthPerson (-1) [Person "Foo" 1, Person "Bar" 2]
-- ["Foo is 1 years old","Bar is 2 years old"]
--
-- Hint: Use @personWithIndex@, @filter@ and @showPerson@.
showEveryNthPerson :: Int -> [Person] -> [String]
showEveryNthPerson n people = undefined

showPerson :: Person -> String
showPerson (Person name age) = name <> " is " <> show age <> " years old"

-- * Bonus Exercises!

-- |
-- Read up a bit on pointfree style:
--
-- - https://wiki.haskell.org/Pointfree
-- - https://amar47shah.github.io/posts/2016-08-06-point-free-part-1.html
--
-- Then implement the following function using a pointfree style. You've implemented it before.
-- This time do it without declaring any arguments.
--
-- Hint: Use Haskell's built-in @even@ function.
--
-- Try on your own first. But if you get stuck, try out http://pointfree.io/.
filterEven1 :: [Int] -> [Int]
filterEven1 = undefined

-- |
-- Just as before, implement this function using a pointfree style.
--
-- Don't use the built-in @product@ function. That would be cheating!
productOfNums1 :: [Int] -> Int
productOfNums1 = undefined

-- |
-- Implement the following imperative pseudocode in functional Haskell.
--
-- @
-- function getNames(persons)
--   var names = []
--   for person in persons
--     names += personName(person)
--   end for
--
--   return names
-- end function
-- @
--
-- As a bonus, try using a pointfree style.
getNames :: [Person] -> [String]
getNames = undefined

-- |
-- Implement the following imperative pseudocode in functional Haskell.
--
-- @
-- function getAdults(persons)
--   var adults = []
--   for person in persons
--     if personAge(person) >= 18 then
--       adults += personName(person)
--     end if
--   end for
--
--   return adults
-- end function
-- @
getAdults :: [Person] -> [Person]
getAdults = undefined

-- |
-- Read up on the difference between @foldr@ and @foldl@:
-- https://gist.github.com/CMCDragonkai/9f5f75118dda10131764.
--
-- Try implementing the following function that will reverse a list using @foldl@.
--
-- Once you get it working, try using @foldr@ instead. You will need to change the arguments around.
-- What result do you get?
--
-- Don't use the built-in @reverse@ function. That would be cheating!
--
-- >>> reverseList [1,2,3,4,5]
-- [5,4,3,2,1]
reverseList :: [a] -> [a]
reverseList = undefined

-- |
-- Pack consecutive duplicates of list elements into sublists.
--
-- >>> xs = ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"]
-- >>> sublists xs
-- [["a","a","a","a"],["b"],["c","c"],["a","a"],["d"],["e","e","e","e"]]
--
-- Note: This function has a type signature we haven't seen before. It uses a type constraint.
-- You can read this as "Given a type @a@ that can be compared for equality, this is a function from
-- @[a]@ to @[[a]]@".
sublists :: Eq a => [a] -> [[a]]
sublists = undefined