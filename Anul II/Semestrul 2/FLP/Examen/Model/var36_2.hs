{-
Gasiti mai jos limbajul unui minicalculator si o interpretare partiala. 
Calculatorul are o celulă de memorie, care are valoarea initiala  0.
Interpretarea instructiunilor este data mai jos.

Un program este o expresie de tip `Prog`iar rezultatul executiei este starea finala a memoriei. 
Testare se face apeland `prog test`. 
-}

data Prog  = On [Stmt]
data Stmt =
     Save Expr     -- evalueaza expresia și salvează rezultatul in Mem
   | NoSave Expr   -- evalueaza expresia, fără a modifica Mem 
data Expr =  Mem | V Int | Expr :+ Expr | If Expr Expr Expr

infixl 6 :+

type Env = Int   -- valoarea curentă a celulei de memorie

expr ::  Expr -> Env -> Int
expr Mem m = m
expr (V val) _ = val
expr (exp1 :+ exp2) m = expr exp1 m + expr exp2 m
expr (If e e1 e2) env
    | expr e env == 0 = expr e1 env
    | otherwise = expr e2 env

stmt :: Stmt -> Env -> Env
stmt (NoSave _) env = env
stmt (Save e) env = expr e env

stmts :: [Stmt] -> Env -> Env
stmts ss env = foldr stmt env ss

prog :: Prog -> Env
prog (On ss) = stmts ss 0


test1 :: Prog
test1 = On [Save (V 3), NoSave (Mem :+ (V 5))]
test2 :: Prog
test2 = On [NoSave (V 3 :+ V 3)]

-- Teste pentru cerinta 1
test3 :: Prog
test3 = On [Save (V 3), Save (V 4)]
test4 :: Prog
test4 = On [NoSave (V 3), NoSave (Mem :+ (V 5))]

-- Teste pentru cerinta 2
test5 :: Prog
test5 = On [Save (If (V 0) (V 1) (V 2))]
test6 :: Prog
test6 = On [Save (If (V 1) (V 1) (V 2)), NoSave (V 5)]


{-CERINTE

1) (10pct) Finalizati definitia functiilor de interpretare.
2) (10 pct) Adaugati expresia `If e e1 e2` care se evaluează `e1` daca `e` are valoarea `0` si la `e2` in caz contrar.
3) (20pct)Definiti interpretarea  limbajului extins modificand functiile de interpretare astfel incat executia unui program
 sa intoarca starea memoriei si  lista valorilor calculate. 
Rezolvați subiectul 3) în același fișier redenumind funcțiile de interpretare.     


Indicati testele pe care le-ati folosit in verificarea solutiilor. 

-}

-- Ex 3
newtype IntListState a = IntListState { runIntListState :: Int -> (a, Int) }

instance Show a => Show (IntListState a) where
  show ma = "Memorie: " ++ show a ++ " Calculate: " ++ show state
    where (a, state) = runIntListState ma 0

instance Monad IntListState where
  return x = IntListState (\s -> (x, s))
  ma >>= k = IntListState (\state -> let (a, state1) = runIntListState ma state
                                 in runIntListState (k a) state1)

instance Applicative IntListState where
  pure = return
  mf <*> ma = do
    f <- mf
    f <$> ma

instance Functor IntListState where
  fmap f ma = f <$> ma

-- schimbare starea
modify :: (Int -> Int) -> IntListState ()
modify fun = IntListState (\s -> ((), fun s))

-- crestere stare contor
tickS :: Int -> IntListState ()
tickS op = modify (max op)

-- obtinere stare curenta
get :: IntListState Int
get = IntListState (\s -> (s, s))

--- Limbajul si Interpretorul
type M a = IntListState a

showM :: Show a => M a -> String
showM = show

add :: Int -> Int -> M Int
add x y = tickS (x + y) >> return (x + y)

runIf :: Int -> Int -> Int -> Int
runIf cond e1 e2
    | cond == 0 = e1
    | otherwise = e2

expr2 ::  Expr -> Env -> M Int
expr2 Mem m = return m
expr2 (V val) _ = return val
expr2 (exp1 :+ exp2) m = do
    e1 <- expr2 exp1 m
    e2 <- expr2 exp2 m
    add e1 e2
expr2 (If e me1 me2) env = do
    cond <- expr2 e env
    e1 <- expr2 me1 env
    e2 <- expr2 me2 env
    return (runIf cond e1 e2)

stmt2 :: Stmt -> Env -> M Env
stmt2 (NoSave _) env = return env
stmt2 (Save me) env = do
    e <- expr2 me env
    return e

stmts2 :: [Stmt] -> Env -> M Env
stmts2 (ms:ss) env = do
    s <- stmt2 ms env
    stmts2 ss s
stmts2 [] env = return env

prog2 :: Prog -> M Env
prog2 (On ss) = stmts2 ss 0

-- Teste pentru cerinta 3
test7 :: Prog
test7 = On [Save (V 3), Save (Mem :+ (V 5))]
test8 :: Prog
test8 = On [Save (V 3 :+ V 3), Save (V 5 :+ V 9)]
