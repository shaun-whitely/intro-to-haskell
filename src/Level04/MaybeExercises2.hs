-- |
-- These exercises simulate the \"real-world\" problem of retrieving records from a data store. You
-- will learn to use 'Maybe's to represent values that may or may not exist in the data store and
-- also techniques to work with the 'Maybe' type.
module Level04.MaybeExercises2 where

import Control.Monad (join)
import Data.Maybe (fromMaybe)

-- |
-- The keyword @type@ creates a type alias.
--
-- This means we have created an alias for the type 'Int' called 'JobId'.
--
-- It serves as a light-weight way to give your types more meaning and have better documentation in
-- code.
type JobId = Int

type HumanId = Int

data Job = Job
  { jobName :: String,
    jobDescription :: String
  }
  deriving (Eq, Show)

data Human = Human
  { humanName :: String,
    humanMaybeJobId :: Maybe JobId
  }
  deriving (Eq, Show)

jobsDatabase :: [(JobId, Job)]
jobsDatabase =
  [ (1, Job "Teacher" "Expert in their field"),
    (2, Job "Engineer" "Build things for people")
  ]

humansDatabase :: [(HumanId, Human)]
humansDatabase =
  [ (1, Human "Sally" Nothing),
    (2, Human "Jenny" (Just 1)),
    (3, Human "Timmy" (Just 1024)) -- job ID doesn't exist in @jobsDatabase@
  ]

-- |
-- >>> findHumanById 1
-- Just (Human {humanName = "Sally", humanMaybeJobId = Nothing})
--
-- >>> findHumanById 100
-- Nothing
--
-- Hint: Use the 'lookup' function on 'humansDatabase'.
findHumanById :: HumanId -> Maybe Human
findHumanById = undefined

-- |
-- >>> findJobById 1
-- Just (Job {jobName = "Teacher", jobDescription = "Expert in their field"})
--
-- >>> findJobById 100
-- Nothing
--
-- Hint: Use the 'lookup' function on 'jobsDatabase'.
findJobById :: JobId -> Maybe Job
findJobById = undefined

-- |
-- >>> findJobDescriptionGivenJobId1 1
-- Just "Expert in their field"
--
-- >>> findJobDescriptionGivenJobId1 100
-- Nothing
--
-- Hint: Use 'findJobById' and then pattern match.
findJobDescriptionGivenJobId1 :: JobId -> Maybe String
findJobDescriptionGivenJobId1 = undefined

-- |
-- Same as above, but use 'fmap' instead.
findJobDescriptionGivenJobId2 :: JobId -> Maybe String
findJobDescriptionGivenJobId2 = undefined

-- |
-- >>> findJobDescriptionGivenJobIdOrElse1 1
-- "Expert in their field"
--
-- >>> findJobDescriptionGivenJobIdOrElse1 100
-- "Job with id 100 does not exist"
--
-- Hint: Use 'findJobDescriptionGivenJobId1' then pattern match.
findJobDescriptionGivenJobIdOrElse1 :: JobId -> String
findJobDescriptionGivenJobIdOrElse1 = undefined

-- |
-- Same as above, but use 'fromMaybe' instead of pattern matching.
findJobDescriptionGivenJobIdOrElse2 :: JobId -> String
findJobDescriptionGivenJobIdOrElse2 = undefined

-- |
-- >>> findJobIdByHumanId 1
-- Nothing
--
-- >>> findJobIdByHumanId 2
-- Just 1
--
-- >>> findJobIdByHumanId 100
-- Nothing
--
-- Hint: Use 'findHumanById', 'fmap' and 'join'.
--
-- What\'s the type that you get after using 'fmap'? What\'s different between that and the
-- function\'s return type?
findJobIdByHumanId :: HumanId -> Maybe JobId
findJobIdByHumanId = undefined

-- |
-- >>> findJobByHumanId 2
-- Just (Job {jobName = "Teacher", jobDescription = "Expert in their field"})
--
-- Hint: Use 'findJobIdByHumanId' and 'findJobById'.
findJobByHumanId :: HumanId -> Maybe Job
findJobByHumanId = undefined

-- |
-- >>> findJobNameByHumanId 2
-- Just "Teacher"
--
-- >>> findJobNameByHumanId 1
-- Nothing
findJobNameByHumanId :: HumanId -> Maybe String
findJobNameByHumanId = undefined
