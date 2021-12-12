module Level04.MaybeExercises1Spec where

import Level04
import Level04.MaybeExercises1
import Test.Hspec

spec :: Spec
spec = do
  describe "safeMean" $ do
    it "returns Nothing given []" $ do
      safeMean [] `shouldBe` Nothing

    it "returns the mean of a non-empty list of numbers" $ do
      safeMean [1,2,3,4] `shouldBe` Just 2.5

  describe "mkTrafficLight" $ do
    it "returns Nothing given an unknown String" $ do
      mkTrafficLight "invalid" `shouldBe` Nothing

    it "returns Red" $ do
      mkTrafficLight "red" `shouldBe` Just Red

    it "returns Yellow" $ do
      mkTrafficLight "yellow" `shouldBe` Just Yellow

    it "returns Green" $ do
      mkTrafficLight "green" `shouldBe` Just Green

  describe "mkPerson" $ do
    it "returns a Person" $ do
      mkPerson "Bob" 20 `shouldBe` Just (Person "Bob" 20)

    it "returns Nothing if name is blank" $ do
      mkPerson "" 20 `shouldBe` Nothing

    it "returns Nothing if age is < 0" $ do
      mkPerson "Bob" (-1) `shouldBe` Nothing

  describe "mkPersonThenReverseName" $ do
    it "returns a Person with the name reversed" $ do
      mkPersonThenReverseName "John" 20 `shouldBe` Just (Person "nhoJ" 20)

    it "returns Nothing if name is blank" $ do
      mkPersonThenReverseName "" 20 `shouldBe` Nothing

    it "returns Nothing if age is < 0" $ do
      mkPersonThenReverseName "John" (-1) `shouldBe` Nothing