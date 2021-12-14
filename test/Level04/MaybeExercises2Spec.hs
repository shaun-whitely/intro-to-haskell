module Level04.MaybeExercises2Spec where
import Level04.MaybeExercises2
import Test.Hspec

spec :: Spec
spec = do
  describe "findHumanById" $ do
    it "returns Nothing if humanId is not in humansDatabase" $ do
      findHumanById 1024 `shouldBe` Nothing

    it "returns the Human for the humanId" $ do
      findHumanById 1 `shouldBe` Just (Human "Sally" Nothing)
      findHumanById 2 `shouldBe` Just (Human "Jenny" (Just 1))

  describe "findJobById" $ do
    it "returns Nothing if jobId is not in jobsDatabase" $ do
      findJobById 1024 `shouldBe` Nothing

    it "returns the Job for the jobId" $ do
      findJobById 1 `shouldBe` Just (Job "Teacher" "Expert in their field")
      findJobById 2 `shouldBe` Just (Job "Engineer" "Build things for people")

  describe "findJobDescriptionGivenJobId1" $ do
    it "returns Nothing if jobId is not in jobsDatabase" $ do
      findJobDescriptionGivenJobId1 1024 `shouldBe` Nothing

    it "returns the description of the job for the jobId" $ do
      findJobDescriptionGivenJobId1 1 `shouldBe` Just "Expert in their field"
      findJobDescriptionGivenJobId1 2 `shouldBe` Just "Build things for people"

  describe "findJobDescriptionGivenJobId2" $ do
    it "returns Nothing if jobId is not in jobsDatabase" $ do
      findJobDescriptionGivenJobId2 1024 `shouldBe` Nothing

    it "returns the description of the job for the jobId" $ do
      findJobDescriptionGivenJobId2 1 `shouldBe` Just "Expert in their field"
      findJobDescriptionGivenJobId2 2 `shouldBe` Just "Build things for people"


  describe "findJobDescriptionGivenJobIdOrElse1" $ do
    it "returns 'does not exist' message if jobId is not in jobsDatabase" $ do
      findJobDescriptionGivenJobIdOrElse1 1024 `shouldBe` "Job with id 1024 does not exist"

    it "returns the description of the job for the jobId" $ do
      findJobDescriptionGivenJobIdOrElse1 1 `shouldBe` "Expert in their field"
      findJobDescriptionGivenJobIdOrElse1 2 `shouldBe` "Build things for people"

  describe "findJobDescriptionGivenJobIdOrElse2" $ do
    it "returns 'does not exist' message if jobId is not in jobsDatabase" $ do
      findJobDescriptionGivenJobIdOrElse2 1024 `shouldBe` "Job with id 1024 does not exist"

    it "returns the description of the job for the jobId" $ do
      findJobDescriptionGivenJobIdOrElse2 1 `shouldBe` "Expert in their field"
      findJobDescriptionGivenJobIdOrElse2 2 `shouldBe` "Build things for people"

  describe "findJobIdByHumanId" $ do
    it "returns Nothing if humanId is not in humansDatabase" $ do
      findJobIdByHumanId 1024 `shouldBe` Nothing

    it "returns Nothing if humanId is found but the Human has no job" $ do
      findJobIdByHumanId 1 `shouldBe` Nothing

    it "returns jobId if humanId is found and the Human has a job" $ do
      findJobIdByHumanId 2 `shouldBe` Just 1

  describe "findJobByHumanId" $ do
    it "returns Nothing if humanId is not in humansDatabase" $ do
      findJobByHumanId 1024 `shouldBe` Nothing

    it "returns Nothing if humanId is found but the Human has no job" $ do
      findJobByHumanId 1 `shouldBe` Nothing

    it "returns Nothing if humanId is found and the Human has a job not in jobsDatabase" $ do
      findJobByHumanId 3 `shouldBe` Nothing

    it "returns the Job if humanId is found and the Human has a job in jobsDatabase" $ do
      findJobByHumanId 2 `shouldBe` Just (Job "Teacher" "Expert in their field")

  describe "findJobNameByHumanId" $ do
    it "returns Nothing if humanId is not in humansDatabase" $ do
      findJobNameByHumanId 1024 `shouldBe` Nothing

    it "returns Nothing if humanId is found but the Human has no job" $ do
      findJobNameByHumanId 1 `shouldBe` Nothing

    it "returns Nothing if humanId is found and the Human has a job not in jobsDatabase" $ do
      findJobNameByHumanId 3 `shouldBe` Nothing

    it "returns the Job's name if humanId is found and the Human has a job in jobsDatabase" $ do
      findJobNameByHumanId 2 `shouldBe` Just "Teacher"