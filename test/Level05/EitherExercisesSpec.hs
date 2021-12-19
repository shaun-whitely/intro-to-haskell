module Level05.EitherExercisesSpec where

import Level05.EitherExercises
import Test.Hspec

spec :: Spec
spec = do
  describe "getName" $ do
    it "returns the supplied name if it is valid" $ do
      getName "Fred" `shouldBe` Right "Fred"

    it "returns EmptyName if the supplied name is empty" $ do
      getName "" `shouldBe` Left EmptyName

  describe "getAge" $ do
    it "returns the supplied age if it is valid" $ do
      getAge "20" `shouldBe` Right 20

    it "returns InvalidAgeValue if the supplied age is not an Int" $ do
      getAge "Fred" `shouldBe` Left (InvalidAgeValue "Fred")

    it "returns InvalidAgeRange if the supplied age is not between 0 and 120" $ do
      getAge "-1" `shouldBe` Left (InvalidAgeRange (-1))
      getAge "121" `shouldBe` Left (InvalidAgeRange 121)

    it "accepts an age of 0" $ do
      getAge "0" `shouldBe` Right 0

    it "accepts an age of 120" $ do
      getAge "120" `shouldBe` Right 120

  describe "createPerson" $ do
    it "returns a Person if supplied with a valid name and age" $ do
      createPerson "Fred" "32" `shouldBe` Right (Person "Fred" 32)

    it "returns EmptyName if the supplied name is empty" $ do
      createPerson "" "32" `shouldBe` Left EmptyName

    it "returns InvalidAgeValue if the supplied age is not an Int" $ do
      createPerson "Fred" "ThirtyTwo" `shouldBe` Left (InvalidAgeValue "ThirtyTwo")

    it "returns InvalidAgeRange if the supplied age is not between 0 and 120" $ do
      createPerson "Fred" "-1" `shouldBe` Left (InvalidAgeRange (-1))

  describe "createPerson2" $ do
    it "returns a Person if supplied with a valid name and age" $ do
      createPerson2 "Fred" "32" `shouldBe` Right (Person "Fred" 32)

    it "returns EmptyName if the supplied name is empty" $ do
      createPerson2 "" "32" `shouldBe` Left EmptyName

    it "returns InvalidAgeValue if the supplied age is not an Int" $ do
      createPerson2 "Fred" "ThirtyTwo" `shouldBe` Left (InvalidAgeValue "ThirtyTwo")

    it "returns InvalidAgeRange if the supplied age is not between 0 and 120" $ do
      createPerson2 "Fred" "-1" `shouldBe` Left (InvalidAgeRange (-1))

  describe "createPersonAndShow" $ do
    it "shows a valid Person" $ do
      createPersonAndShow "Fred" "32" `shouldBe` "Fred is 32"

    it "shows an invalid name" $ do
      createPersonAndShow "" "32" `shouldBe` "Empty name supplied"

    it "shows an invalid age value" $ do
      createPersonAndShow "Fred" "ThirtyTwo" `shouldBe` "Invalid age value supplied: ThirtyTwo"

    it "shows an invalid age range" $ do
      createPersonAndShow "Fred" "150" `shouldBe` "Provided age must be between 0 and 120: 150"

  describe "makeNameUppercase" $ do
    it "returns a Person if supplied with a valid name and age" $ do
      makeNameUppercase "Fred" "32" `shouldBe` Right (Person "FRED" 32)

    it "returns EmptyName if the supplied name is empty" $ do
      makeNameUppercase "" "32" `shouldBe` Left EmptyName

    it "returns InvalidAgeValue if the supplied age is not an Int" $ do
      makeNameUppercase "Fred" "ThirtyTwo" `shouldBe` Left (InvalidAgeValue "ThirtyTwo")

    it "returns InvalidAgeRange if the supplied age is not between 0 and 120" $ do
      makeNameUppercase "Fred" "-1" `shouldBe` Left (InvalidAgeRange (-1))

  describe "validPeople" $ do
    it "contains only valid Person instances" $ do
      validPeople `shouldBe` [Person "Tokyo" 30, Person "Berlin" 43]

  describe "peopleErrors" $ do
    it "contains all of the errors from processing the people" $ do
      peopleErrors
        `shouldBe` [ InvalidAgeValue "5o",
                     InvalidAgeRange 200,
                     InvalidAgeRange (-1),
                     EmptyName
                   ]

  describe "createPerson3" $ do
    it "returns a Person if supplied with a valid name and age" $ do
      createPerson3 "Fred" "32" `shouldBe` Right (Person "Fred" 32)

    it "returns EmptyName if the supplied name is empty" $ do
      createPerson3 "" "32" `shouldBe` Left EmptyName

    it "returns InvalidAgeValue if the supplied age is not an Int" $ do
      createPerson3 "Fred" "ThirtyTwo" `shouldBe` Left (InvalidAgeValue "ThirtyTwo")

    it "returns InvalidAgeRange if the supplied age is not between 0 and 120" $ do
      createPerson3 "Fred" "-1" `shouldBe` Left (InvalidAgeRange (-1))
