data Prog = On Instr
data Instr = Off | Expr :> Instr
data Expr = Mem | V Int | Expr :+ Expr
type Env = Int -- valoarea celulei de memorie
type DomProg = [Int]
type DomInstr = Env -> [Int]
type DomExpr = Env -> Int

prog :: Prog -> DomProg
prog (On i) = stmt i 0

stmt :: Instr -> DomInstr
stmt i = stmt' i
   where 
      stmt' Off l = []
      stmt' (e :> i) l = let eval = expr e l in
                         [eval] ++ (stmt' i eval)

expr :: Expr -> DomExpr
expr e = expr' e
   where
      expr' Mem l = l
      expr' (V v) _ = v
      expr' (e1 :+ e2) l = (expr' e1 l) + (expr' e2 l)

p1 = On ( (V 3) :> ((Mem :+ (V 5)):> Off))


type Name = String
data Hask = HTrue
 | HFalse
 | HLit Int
 | HIf Hask Hask Hask
 | Hask :==: Hask
 | Hask :+: Hask
 | HVar Name
 | HLam Name Hask
 | Hask :$: Hask
  deriving (Read, Show)
infix 4 :==:
infixl 6 :+:
infixl 9 :$:

data Value = VBool Bool
 | VInt Int
 | VFun (Value -> Value)
 | VError -- pentru reprezentarea erorilor
type HEnv = [(Name, Value)]

type DomHask = HEnv -> Value

-- 1
instance Show Value where
   show (VBool x) = show x
   show (VInt x) = show x
   show (VFun _) = "function"
   show VError = "error"

-- 2
instance Eq Value where
   (VBool x1) == (VBool x2) = x1 == x2
   (VInt x1) == (VInt x2) = x1 == x2
   _ == _ = False

-- 3
isError :: Value -> Bool
isError VError = True
isError _ = False

hEval :: Hask -> DomHask

hEval HTrue _ = VBool True
hEval HFalse _ = VBool False
hEval (HLit i) _ = VInt i
hEval (HIf a b c) env =
    case hEval a env of
        VBool True -> hEval b env
        VBool False -> hEval c env
        _ -> VError
hEval (a :==: b) env =
    let x :: Value
        x = hEval a env
        y :: Value
        y = hEval b env in
    if isError x || isError y then
        VError
    else
        VBool $ x == y

hEval (a :+: b) env =
    let x :: Value
        x = hEval a env
        y :: Value
        y = hEval b env in
    case (x, y) of
        (VInt a, VInt b) -> VInt (a + b)
        _ -> VError
hEval (HVar n) env =
    let rez = lookup n env in
    case rez of
        Nothing -> VError
        Just exp -> exp
hEval (HLam n exp) env =
    VFun (\x ->
        let new_entry = (n, x)
            new_env =
                if lookup n env == Nothing then
                    new_entry : env
                else
                    map (\(name, val) ->
                        if name == n then
                            new_entry
                        else
                            (name, val)) env in
        hEval exp new_env)
hEval (a :$: b) env =
    let fun = hEval a env
        val = hEval b env in
    case fun of
        VError -> VError
        VFun f -> f val

lambdaExp :: Hask
lambdaExp = HLam "X" (HVar "X" :+: HVar "X") :$: HLit 10

eval :: Value
eval = hEval lambdaExp []

main = undefined
