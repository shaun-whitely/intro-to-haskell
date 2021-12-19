module Level07.LogParserSpec where

import Level07.LogParser
import Test.Hspec

spec :: Spec
spec = do
  describe "parseLog" $ do
    it "returns a KnownLog for an Info message" $ do
      parseLog "I,147,mice in the air" `shouldBe` KnownLog Info 147 "mice in the air"

    it "returns a KnownLog for a Warning message" $ do
      parseLog "W,188,hamsters underground" `shouldBe` KnownLog Warning 188 "hamsters underground"

    it "returns a KnownLog for an Error message" $ do
      parseLog "E,5,193,ferrets out at sea" `shouldBe` KnownLog (Error 5) 193 "ferrets out at sea"

    it "returns an UnknownLog for other messages" $ do
      parseLog "X blblbaaaaa" `shouldBe` UnknownLog "X blblbaaaaa"

  describe "parseLogFile" $ do
    it "returns a list of log messages" $ do
      let logMessages = parseLogFile "I,147,mice in the air\nX blblbaaaaa"
      logMessages `shouldBe` [KnownLog Info 147 "mice in the air", UnknownLog "X blblbaaaaa"]

    it "returns an empty list for an empty file" $ do
      parseLogFile "" `shouldBe` []

  describe "getErrorsOverSeverity" $ do
    it "returns only errors over the specified severity" $ do
      let errorLogUnderSeverity = KnownLog (Error 2) 123 "some error msg"
      let errorLogOverSeverity = KnownLog (Error 3) 123 "some error msg"
      let unknown1 = UnknownLog "X blblbaaaaa"
      let unknown2 = UnknownLog "W foo"
      let messages = [errorLogUnderSeverity, errorLogOverSeverity, unknown1, unknown2]
      getErrorsOverSeverity messages 2 `shouldBe` [errorLogOverSeverity]

  describe "showLogMessage" $ do
    it "returns Info LogMessage in readable format" $ do
      let infoMessage = KnownLog Info 100 "one"
      showLogMessage infoMessage `shouldBe` "Info (100) one"

    it "returns Warning LogMessage in readable format" $ do
      let warnMessage = KnownLog Warning 100 "caution!"
      showLogMessage warnMessage `shouldBe` "Warning (100) caution!"

    it "returns Error LogMessage in readable format" $ do
      let errorMessage = KnownLog (Error 1) 300 "some message"
      showLogMessage errorMessage `shouldBe` "Error 1 (300) some message"

    it "returns UnknownLog in readable format" $ do
      let unknownMessage = UnknownLog "message"
      showLogMessage unknownMessage `shouldBe` "Unknown log: message"

  describe "showErrorsOverSeverity" $ do
    it "should show all errors over a given severity level in a readable format" $ do
      let fileContent =
            "I,147,mice in the air\n\
            \W,149,could've been bad\n\
            \E,5,158,some strange error\n\
            \E,6,170,another error\n\
            \E,2,148,istereadea\n\
            \E,4,199,frabjous"
      showErrorsOverSeverity fileContent 4
        `shouldBe` ["Error 5 (158) some strange error", "Error 6 (170) another error"]