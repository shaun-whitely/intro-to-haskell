module Level03.ListExercisesSpec where

import Level03.ListExercises
import Test.Hspec

spec :: Spec
spec = do
  describe "prependToList" $ do
    it "adds an element to the start of the list" $ do
      prependToList 1 [2, 3, 4] `shouldBe` ([1, 2, 3, 4] :: [Int])

  describe "appendToList" $ do
    it "adds an element to the end of the list" $ do
      appendToList 1 [2, 3, 4] `shouldBe` ([2, 3, 4, 1] :: [Int])

  describe "isEmptyList" $ do
    it "returns True for []" $ do
      isEmptyList [] `shouldBe` True

    it "returns False for a non-empty list" $ do
      isEmptyList [1 :: Int] `shouldBe` False

  describe "showListSize" $ do
    it "shows size for []" $ do
      showListSize [] `shouldBe` "This is an empty list"

    it "shows the size of a non-empty list" $ do
      showListSize ([1, 2, 3] :: [Int]) `shouldBe` "This is a list of size 3"

  describe "addNumToEach1" $ do
    it "returns [] given []" $ do
      addNumToEach1 5 [] `shouldBe` []

    it "adds 5 to each element of a non-empty list" $ do
      addNumToEach1 5 [1, 2, 3] `shouldBe` [6, 7, 8]

  describe "addNumToEach2" $ do
    it "returns [] given []" $ do
      addNumToEach2 5 [] `shouldBe` []

    it "adds 5 to each element of a non-empty list" $ do
      addNumToEach2 5 [1, 2, 3] `shouldBe` [6, 7, 8]

  describe "filterEven" $ do
    it "returns even numbers" $ do
      filterEven [1, 2, 3, 4, 5, 6] `shouldBe` [2, 4, 6]

  describe "productOfNums" $ do
    it "returns 1 given []" $ do
      productOfNums [] `shouldBe` 1

    it "multiplies all elements of a non-empty list" $ do
      productOfNums [2, 5, 3] `shouldBe` 30

  describe "minOfNums" $ do
    it "returns the smallest Int given []" $ do
      minOfNums [] `shouldBe` minBound

    it "returns the smallest number in a non-empty list" $ do
      minOfNums [4, 6, 1] `shouldBe` 1

  describe "youngestPerson" $ do
    it "returns a silly default person given []" $ do
      youngestPerson [] `shouldBe` Person "Nobody" 0

    it "returns the first person in the list with the smallest age given a non-empty list" $ do
      youngestPerson peopleList `shouldBe` Person "Karen Page" 27

  describe "personWithIndex" $ do
    it "returns each person with their 1-based index" $ do
      personWithIndex peopleList
        `shouldBe` [ (Person "Matt Murdock" 30, 1),
                     (Person "Karen Page" 27, 2),
                     (Person "Franklin 'Foggy' Nelson" 31, 3),
                     (Person "Claire Temple" 32, 4),
                     (Person "Wilson Fisk" 42, 5),
                     (Person "Elektra Natchios" 27, 6)
                   ]

  describe "showEveryNthPerson" $ do
    it "shows every Nth person" $ do
      let allPeople = fmap showPerson peopleList

      showEveryNthPerson (-5) peopleList `shouldBe` allPeople
      showEveryNthPerson 0 peopleList `shouldBe` allPeople
      showEveryNthPerson 1 peopleList `shouldBe` allPeople
      showEveryNthPerson 2 peopleList
        `shouldBe` [ "Karen Page is 27 years old",
                     "Claire Temple is 32 years old",
                     "Elektra Natchios is 27 years old"
                   ]
      showEveryNthPerson 3 peopleList
        `shouldBe` [ "Franklin 'Foggy' Nelson is 31 years old",
                     "Elektra Natchios is 27 years old"
                   ]
      showEveryNthPerson 5 peopleList `shouldBe` ["Wilson Fisk is 42 years old"]
      showEveryNthPerson 6 peopleList `shouldBe` ["Elektra Natchios is 27 years old"]
      showEveryNthPerson 8 peopleList `shouldBe` []

  describe "filterEven1" $ do
    it "returns even numbers" $ do
      filterEven1 [1, 2, 3, 4, 5, 6] `shouldBe` [2, 4, 6]

  describe "productOfNums1" $ do
    it "returns 1 given []" $ do
      productOfNums1 [] `shouldBe` 1

    it "multiplies all elements of a non-empty list" $ do
      productOfNums1 [2, 5, 3] `shouldBe` 30

  describe "getNames" $ do
    it "returns the names of all persons" $ do
      let powerRangers =
            [ Person "Red Ranger" 22,
              Person "Yellow Ranger" 20,
              Person "Pink Ranger" 21
            ]

      getNames powerRangers `shouldBe` ["Red Ranger", "Yellow Ranger", "Pink Ranger"]

  describe "getAdults" $ do
    it "returns persons aged >= 18" $ do
      let powerRangers =
            [ Person "Red Ranger" 17,
              Person "Yellow Ranger" 18,
              Person "Pink Ranger" 19
            ]

      getAdults powerRangers `shouldBe` [Person "Yellow Ranger" 18, Person "Pink Ranger" 19]

  describe "reverseList" $ do
    it "returns the input list reversed" $ do
      reverseList [1, 2, 3] `shouldBe` ([3, 2, 1] :: [Int])

  describe "sublists" $ do
    it "packs consecutive duplicates of list elements into sublists" $ do
      let inputList = ["a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"]

      sublists inputList
        `shouldBe` [ ["a", "a", "a", "a"],
                     ["b"],
                     ["c", "c"],
                     ["a", "a"],
                     ["d"],
                     ["e", "e", "e", "e"]
                   ]
