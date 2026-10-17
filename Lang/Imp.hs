{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall #-}
module Lang.Imp where

import qualified Map.TreeMap as M

todo :: a
todo = error "TODO"


type Name = String
type State = M.Map Name Integer

emptyState :: State
emptyState = M.empty

data Expr a where
    BNot :: Expr Bool -> Expr Bool
    BAnd :: Expr Bool -> Expr Bool -> Expr Bool
    BOr  :: Expr Bool -> Expr Bool -> Expr Bool

    REq :: Expr Integer -> Expr Integer -> Expr Bool
    RLt :: Expr Integer -> Expr Integer -> Expr Bool

    AConst :: Integer -> Expr Integer
    AVar   :: Name -> Expr Integer
    APlus  :: Expr Integer -> Expr Integer -> Expr Integer
    AMinus :: Expr Integer -> Expr Integer -> Expr Integer
    AMul   :: Expr Integer -> Expr Integer -> Expr Integer
    ADiv   :: Expr Integer -> Expr Integer -> Expr Integer
    AMod   :: Expr Integer -> Expr Integer -> Expr Integer

deriving instance Eq (Expr a) 
deriving instance Show (Expr a)

data Cmd = 
    CSkip
  | CAssign Name (Expr Integer)
  | CIfThEl (Expr Bool) Cmd Cmd
  | CWhile (Expr Bool) Cmd
  | CSeq [Cmd]

eval :: Expr a -> State -> a
eval (BNot b) st = not (eval b st)
-- Bool
eval (BNot b)   st = not (eval b st)
eval (BAnd a b) st = eval a st && eval b st
eval (BOr  a b) st = eval a st || eval b st

-- Relational
eval (REq a b)  st = eval a st == eval b st
eval (RLt a b)  st = eval a st <  eval b st

-- Integer
eval (AConst n) st = n
eval (AVar x)   st =
    case M.lookup x st of
      Just v  -> v
      Nothing -> 0   -- Variablen ohne Wert = 0

eval (APlus  a b) st = eval a st + eval b st
eval (AMinus a b) st = eval a st - eval b st
eval (AMul   a b) st = eval a st * eval b st
eval (ADiv   a b) st = eval a st `div` eval b st
eval (AMod   a b) st = eval a st `mod` eval b st

exec :: Cmd -> State -> State
exec CSkip st = st

exec (CAssign x e) st =
    let v = eval e st
    in M.insert x v st

exec (CIfThEl cond cThen cElse) st =
    if eval cond st
      then exec cThen st
      else exec cElse st

exec (CWhile cond body) st =
    if eval cond st
      then exec (CWhile cond body) (exec body st)
      else st

exec (CSeq []) st = st
exec (CSeq (c:cs)) st =
    let st' = exec c st
    in exec (CSeq cs) st'
