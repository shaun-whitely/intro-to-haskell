module Level04.MaybeExercises3Spec where

import Level04.MaybeExercises3
import Test.Hspec
import Level04.MaybeExercises2 (Job(Job))

spec :: Spec
spec = do
  describe "findJobIdByHumanIdUsingBind" $ do
    it "returns Nothing if humanId is not in humansDatabase" $ do
      findJobIdByHumanIdUsingBind 1024 `shouldBe` Nothing

    it "returns Nothing if humanId is found but the Human has no job" $ do
      findJobIdByHumanIdUsingBind 1 `shouldBe` Nothing

    it "returns jobId if humanId is found and the Human has a job" $ do
      findJobIdByHumanIdUsingBind 2 `shouldBe` Just 1

  describe "findJobIdByHumanIdUsingDo" $ do
    it "returns Nothing if humanId is not in humansDatabase" $ do
      findJobIdByHumanIdUsingDo 1024 `shouldBe` Nothing

    it "returns Nothing if humanId is found but the Human has no job" $ do
      findJobIdByHumanIdUsingDo 1 `shouldBe` Nothing

    it "returns jobId if humanId is found and the Human has a job" $ do
      findJobIdByHumanIdUsingDo 2 `shouldBe` Just 1

  describe "findJobByHumanIdUsingBind" $ do
    it "returns Nothing if humanId is not in humansDatabase" $ do
      findJobByHumanIdUsingBind 1024 `shouldBe` Nothing

    it "returns Nothing if humanId is found but the Human has no job" $ do
      findJobByHumanIdUsingBind 1 `shouldBe` Nothing

    it "returns Nothing if humanId is found and the Human has a job not in jobsDatabase" $ do
      findJobByHumanIdUsingBind 3 `shouldBe` Nothing

    it "returns jobId if humanId is found and the Human has a job" $ do
      findJobByHumanIdUsingBind 2 `shouldBe` Just (Job "Teacher" "Expert in their field")