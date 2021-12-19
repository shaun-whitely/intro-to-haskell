module Level01.IntroExercisesSpec where

import Level01.IntroExercises
import Test.Hspec

spec :: Spec
spec = do
  describe "add" $ do
    it "evaluates 1 + 2 as 3" $ do
      add 1 2 `shouldBe` 3

    it "evaluates 5 + 7 as 12" $ do
      add 5 7 `shouldBe` 12

  describe "add5" $ do
    it "evaluates 5 + 4 as 9" $ do
      add5 4 `shouldBe` 9

    it "evaluates 5 + 10 as 15" $ do
      add5 10 `shouldBe` 15

  describe "foo" $ do
    it "can only return the parameter unmodified (aka the 'identity' function)" $ do
      foo 1 `shouldBe` (1 :: Int)

  describe "bar" $ do
    it "can be any Int" $ do
      bar 1 `shouldBe` bar 1

  describe "timesTwoIfEven" $ do
    it "given 4 returns 8" $ do
      timesTwoIfEven 4 `shouldBe` 8

    it "given 3 returns 3" $ do
      timesTwoIfEven 3 `shouldBe` 3

  describe "showNumber" $ do
    it "constructs the correct string" $ do
      showNumber 1 `shouldBe` "The number is 1"

  describe "pair" $ do
    it "tuples Jimmy and 25" $ do
      pair "Jimmy" 25 `shouldBe` ("Jimmy", 25)

    it "tuples Sammy and 30" $ do
      pair "Sammy" 30 `shouldBe` ("Sammy", 30)

  describe "getName" $ do
    it "returns Jimmy's name" $ do
      getName ("Jimmy", 25) `shouldBe` "Jimmy"

    it "returns Sammy's name" $ do
      getName ("Sammy", 30) `shouldBe` "Sammy"

  describe "getAge" $ do
    it "returns Jimmy's age" $ do
      getAge ("Jimmy", 25) `shouldBe` 25

    it "returns Sammy's age" $ do
      getAge ("Sammy", 30) `shouldBe` 30
