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


factStmt :: Stmt
factStmt =
  "p" := Lit 1 ::: "n" := Lit 3 :::
  While (Not (Var "n" :==: Lit 0))
    ( "p" := Var "p" :*: Var "n" :::
      "n" := Var "n" :+: Lit (-1)
    )
   
factStmt2 :: Stmt
factStmt2 =
  "p" := Lit 1 ::: "n" := Lit 5 :::
  While (BTrue)
    ( 
      If (Var "n" :==: Lit 0)
        Break
     ("p" := Var "p" :*: Var "n" :::
      "n" := Var "n" :+: Lit (-1))
    ) :::
    "p" := (Var "p" :+: Lit 1)

factStmt3 :: Stmt
factStmt3 = ("p" := Lit 1 ::: "n" := Lit 3 :::
   While (Not (Inc "i" :==: Var "n"))
     ("p" := Var "p" :*: Var "i")
   ) 

pg1 = Pgm [] factStmt 
pg2 = Pgm [] factStmt2
pg3 = Pgm ["p", "n", "i"] factStmt3

extractVal :: Name -> Env -> Integer
extractVal _ [] = 0
extractVal name (h:t)
   | (fst h) == name = snd h
   | otherwise = extractVal name t

updateEnv :: Name -> Integer -> Env -> Env
updateEnv name val [] = [(name, val)]
updateEnv name val (h:t)
   | (fst h) == name = (name, val):t
   | otherwise = (updateEnv name val t) ++ [h]

aEval :: AExp -> Env -> (Integer, Env)
aEval (Lit x) env = (x, env)
aEval (exp1 :+: exp2) env = let (val1, env1) = (aEval exp1 env) in
                            let (val2, env2) = (aEval exp2 env1) in
                            (val1 + val2, env2)
aEval (exp1 :*: exp2) env = let (val1, env1) = (aEval exp1 env) in 
                            let (val2, env2) = (aEval exp2 env1) in
                            (val1 * val2, env2)
aEval (Inc name) env = let val = extractVal name env in
                       let val1 = val + 1 in
                       (val1, updateEnv name val1 env)
aEval (Dec name) env = let val = extractVal name env in
                       let val1 = val - 1 in
                       (val1, updateEnv name val1 env) 
aEval (Var name) env = (extractVal name env, env)

bEval :: BExp -> Env -> (Bool, Env)
bEval BTrue env = (True, env)
bEval BFalse env = (False, env)
bEval (expa1 :==: expa2) env = let (val1, env1) = (aEval expa1 env) in
                               let (val2, env2) = (aEval expa2 env1) in
                               (val1 == val2, env2)
bEval (Not exp) env = let (val1, env1) = bEval exp env in
                      (not val1, env1)

sEval :: Stmt -> Env -> Env
sEval Skip env = env
sEval (stmt1 ::: stmt2) env = let env1 = sEval stmt1 env in
                              sEval stmt2 env1
sEval (If expb stmt1 stmt2) env = let (eval, env1) = bEval expb env in
                                  if eval then
                                      sEval stmt1 env1
                                  else
                                      sEval stmt2 env1
sEval (While expb stmt) env = let (eval, env1) = bEval expb env in
                              if eval then
                                  let env2 = sEval stmt env1 in
                                      sEval (While expb stmt) env2
                              else
                                  env1
sEval (name := expa) env = let (val, env1) = aEval expa env in
                           updateEnv name val env1

pEval :: Pgm -> Env
pEval (Pgm names stmt) = let env = [(name, 0) | name <- names] in
                         sEval stmt env

main = undefined   
