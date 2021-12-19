-- |
-- The exercises here are adapted from: http://www.cis.upenn.edu/~cis194/spring13/hw/02-ADTs.pdf
--
-- Let's finish off this course by building a CLI program!
-- We will build a program to parse a log file (containing Info, Warn and Error messages) and print
-- out errors over a severity level.
--
-- Finish each exercise and we will head over to @Main.hs@ to hook it all up with the CLI.
module Level07.LogParser where

import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Text.Read (readMaybe)

-- |
-- Here is how a log file may look.
--
-- The @\@ at the start and end of each line can be ignored. This is how Haskell represents a
-- multi-line string.
logFile :: String
logFile =
  "I,147,mice in the air\n\
  \W,149,could've been bad\n\
  \E,5,158,some strange error\n\
  \E,2,148,istereadea\n"

-- Let's define an ADT to represent all possible log messages.

-- |
-- Let's start with all the possible log levels:
--
-- - Info
-- - Warning
-- - Error with a severity
data LogLevel
  = Info
  | Warning
  | Error Int
  deriving (Eq, Show)

type Timestamp = Int

-- |
-- Here is an ADT for @LogMessage@, which can be one of two possibilities:
--
-- - KnownLog for messages in a recognised format
-- - UnknownLog for all other messages
data LogMessage
  = KnownLog LogLevel Timestamp String
  | UnknownLog String
  deriving (Eq, Show)

-- |
-- Define a function to parse an individual log message.
--
-- >>> parseLog "I,147,mice in the air"
-- KnownLog Info 147 "mice in the air"
--
-- >>> parseLog "E,2,148,weird"
-- KnownLog (Error 2) 148 "weird"
--
-- >>> parseLog "X blblbaaaaa"
-- UnknownLog "X blblbaaaaa"
parseLog :: String -> LogMessage
parseLog str =
  let fields = splitOn "," str
      maybeLog =
        case fields of
          ["I", timestampStr, message] ->
            fmap (\timestamp -> undefined) (readMaybe timestampStr :: Maybe Int)
          -- Add more cases
          _ -> Nothing
   in undefined -- What should we do if maybeLog is @Nothing@.

-- |
-- >>> parseLogFile "I,147,mice in the air\nX blblbaaaaa"
-- [KnownLog Info 147 "mice in the air",UnknownLog "X blblbaaaaa"]
--
-- Hint: Use @parseLog@ and @lines@
--
-- What if we get an empty line from the file content.
parseLogFile :: String -> [LogMessage]
parseLogFile fileContent = undefined

-- |
-- Define a function that returns only logs that are errors over the given severity level.
-- It should not return any log that is not an error.
--
-- >>> getErrorsOverSeverity [KnownLog (Error 2) 123 "some error msg", UnknownLog "blblbaaaaa"] 1
-- [KnownLog (Error 2) 123 "some error msg"]
--
-- >>> getErrorsOverSeverity [KnownLog (Error 2) 123 "some error msg"] 2
-- []
--
-- Hint: Use @mapMaybe@.
getErrorsOverSeverity :: [LogMessage] -> Int -> [LogMessage]
getErrorsOverSeverity logs severity =
  let errorOverSeverity message@(KnownLog (Error sev) _ _) = undefined
      errorOverSeverity _ = undefined
   in undefined

-- |
-- This function converts a @LogMessage@ to a readable @String@.
showLogMessage :: LogMessage -> String
showLogMessage (KnownLog Info timestamp message) =
  "Info (" <> show timestamp <> ") " <> message
showLogMessage (KnownLog Warning timestamp message) =
  "Warning (" <> show timestamp <> ") " <> message
showLogMessage (KnownLog (Error severity) timestamp message) =
  "Error " <> show severity <> " (" <> show timestamp <> ") " <> message
showLogMessage (UnknownLog message) =
  "Unknown log: " <> message

-- |
-- Use @showLogMessage@ on error logs with severity greater than the given @severity@.
--
-- >>> showErrorsOverSeverity logFile 2
-- ["Error 5 (158) some strange error"]
--
-- Hint: Use @parseLogFile@, @getErrorsOverSeverity@ and @showLogMessage@.
showErrorsOverSeverity :: String -> Int -> [String]
showErrorsOverSeverity fileContent severity = undefined

-- Now head over to @exe/LogParser.hs@ to complete the rest of the program.