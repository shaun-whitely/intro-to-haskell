module Level02.TypesExercisesSpec where

import Level02.TypesExercises
import Test.Hspec

spec :: Spec
spec = do
  describe "showPerson1" $ do
    it "turns the Person into a String" $ do
      let person = Person "Bob" 50
      showPerson1 person `shouldBe` "Bob is 50 years old"

  describe "showPerson2" $ do
    it "turns the Person into a String" $ do
      let person = Person "Bob" 50
      showPerson2 person `shouldBe` "Bob is 50 years old"

  describe "changeAge" $ do
    it "updates the Person's age" $ do
      let person = Person "Bob" 50
      let updatedPerson = changeAge 60 person

      updatedPerson `shouldBe` Person "Bob" 60
      person `shouldBe` Person "Bob" 50 -- assertion is unnecessary, but shows immutability
  describe "showWallet" $ do
    it "shows the wallet amount as a String" $ do
      let wallet = Wallet 23.4
      showWallet wallet `shouldBe` "The wallet amount is 23.4"

  describe "purchase" $ do
    it "returns a wallet with cost deducted" $ do
      let wallet = Wallet 100
      let updatedWallet = purchase 25 wallet

      updatedWallet `shouldBe` Wallet 75
      wallet `shouldBe` Wallet 100 -- assertion is unnecessary, but shows immutability
  describe "showTrafficLightStr" $ do
    it "shows red" $ do
      showTrafficLightStr "red" `shouldBe` "The traffic light is red"

    it "shows yellow" $ do
      showTrafficLightStr "yellow" `shouldBe` "The traffic light is yellow"

    it "shows green" $ do
      showTrafficLightStr "green" `shouldBe` "The traffic light is green"

    it "returns a default on other inputs" $ do
      pendingWith "See Section 3"

    it "shows flashing" $ do
      pendingWith "See Section 4.1"

  describe "showTrafficLight" $ do
    it "shows red" $ do
      showTrafficLight Red `shouldBe` "The traffic light is red"

    it "shows yellow" $ do
      showTrafficLight Yellow `shouldBe` "The traffic light is yellow"

    it "shows green" $ do
      showTrafficLight Green `shouldBe` "The traffic light is green"

    it "shows flashing" $ do
      pendingWith "See Section 5.1"
