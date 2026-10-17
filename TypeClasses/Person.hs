module TypeClasses.Person where

data Person = MkPerson { nr:: Int, email :: String }

instance Eq Person where
    MkPerson a b == MkPerson c d = a==c

instance Show Person where
    show (MkPerson a b) = b

instance Ord Person where
    MkPerson a b <= MkPerson c d = a <= c