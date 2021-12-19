```haskell
-- data type
data Person = Person String Int
  deriving
    -- derive Eq to be able to compare Person instances for equality
    ( Eq
    -- derive Show in order to print Person instances to the terminal
    , Show
    )

-- or using record syntax
data Person
  = Person
  { personName :: String
  , personAge :: Int
  }
  deriving (Eq, Show)

person1 = Person "Fred" 20
person2 = Person { personName = "Wilma", personAge = 30 }

name1 = personName person1
age2 = personAge person2
```
