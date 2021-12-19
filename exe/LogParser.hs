module Main where

import Level07.LogParser (showErrorsOverSeverity)
import System.Environment (getArgs)

-- |
-- The @main@ function does the following:
--
-- 1. Read @filepath@ from CLI args
-- 2. Read the content in @filepath@
-- 3. Call @showErrorsOverSeverity@ from @Level07.LogParser@
-- 4. Print the errors out to STDOUT
main :: IO ()
main = do
  args <- getArgs
  case args of
    [filepath] ->
      do
        -- Read from file
        fileContent <- readFile filepath

        -- Implement @printErrorsOverSeverity@ and then call it from here
        undefined
    _ ->
      putStrLn "usage: cabal new-run log-parser -- logfile.csv"

-- |
-- Note that at no point have we printed anything out to the user.

-- By pushing side-effects like printing to stdout to the very end of our program, we are able to
-- unit test the majority of our program.
--
-- Now, using @showErrorsOverSeverity@, let's print out the results to stdout.
--
-- - Hint 1: Use @putStrLn@ to write to STDOUT.
-- - Hint 2: Use @unlines@ to join a list of strings with newlines.
printErrorsOverSeverity :: String -> Int -> IO ()
printErrorsOverSeverity fileContent severity = undefined