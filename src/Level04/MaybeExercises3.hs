-- |
-- These exercises mirror the ones from "Level04.MaybeExercises2". They are for the purpose of
-- teaching the bind operators '=<<' and '>>=', and do-notation, which is very useful for working
-- with 'Maybe'.
module Level04.MaybeExercises3 where

import Control.Monad (join)
import Level04.MaybeExercises2 (Human (..), HumanId, Job (..), JobId, findHumanById, findJobById)

-- |
-- Rewrite this function using '>>=' or '=<<'.
--
-- >>> findJobIdByHumanIdUsingBind 1
-- Nothing
--
-- >>> findJobIdByHumanIdUsingBind 2
-- Just 1
findJobIdByHumanIdUsingBind :: HumanId -> Maybe JobId
findJobIdByHumanIdUsingBind humanId = join (fmap humanMaybeJobId (findHumanById humanId))

-- |
-- Rewrite the same function using do-notation.
findJobIdByHumanIdUsingDo :: HumanId -> Maybe JobId
findJobIdByHumanIdUsingDo humanId = join (fmap humanMaybeJobId (findHumanById humanId))

-- |
-- Rewrite this function using multiple '>>='s or '=<<'s.
--
-- >>> findJobByHumanIdUsingBind 2
-- Just (Job {jobName = "Teacher", jobDescription = "Expert in their field"})
--
-- >>> findJobByHumanIdUsingBind 3
-- Nothing
findJobByHumanIdUsingBind :: HumanId -> Maybe Job
findJobByHumanIdUsingBind humanId =
  do
    human <- findHumanById humanId
    jobId <- humanMaybeJobId human
    findJobById jobId
