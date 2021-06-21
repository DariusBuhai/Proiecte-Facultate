--- Define data
import Data.Maybe
import Data.List

type Name = String
data  Pgm  = Pgm [Name] Stmt
        deriving (Read, Show)
data Stmt = Skip | Stmt ::: Stmt | If BExp Stmt Stmt | While BExp Stmt | Name := AExp | Break
        deriving (Read, Show)
data AExp = Lit Integer | AExp :+: AExp | AExp :*: AExp | Var Name | Inc Name | Dec Name
        deriving (Read, Show)
data BExp = BTrue | BFalse | AExp :==: AExp | Not BExp
        deriving (Read, Show)

infixr 2 :::
infix 3 :=
infix 4 :==:
infixl 6 :+:
infixl 7 :*:


type Env = [(Name, Integer)]

--- Testing
factStmt :: Stmt
factStmt =
  "p" := Lit 1 ::: "n" := Lit 3 :::
  While (Not (Var "n" :==: Lit 0))
    ( "p" := Var "p" :*: Var "n" :::
      "n" := Var "n" :+: Lit (-1)
    )
pg1 = Pgm [] factStmt


--- Implementation
pEval :: Pgm -> Env
pEval = undefined

sEval :: Stmt -> Env -> Env
sEval = undefined 

bEval :: BExp -> Env -> Bool
bEval = undefined 

aEval :: AExp -> Env -> Integer
aEval = undefined 



