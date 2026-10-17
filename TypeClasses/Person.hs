module TypeClasses.Person where

data Person = MkPerson { nr:: Int, email :: String }

-- Show: zeige "Person <nr> <email>" aus
instance Show Person where
  show (MkPerson n e) = "Person " ++ show n ++ " " ++ e

-- Eq: vergleiche die Nummer
instance Eq Person where
  (MkPerson nr1 _) == (MkPerson nr2 _) = nr1 == nr2

-- Ord:  nach der Nummer ordnen
instance Ord Person where
  compare (MkPerson nr1 _) (MkPerson nr2 _) = compare nr1 nr2