module Level04 where

data Person = Person
  { personName :: String,
    personAge :: Int
  }
  deriving (Eq, Show)

data TrafficLight = Red | Yellow | Green deriving (Eq, Show)