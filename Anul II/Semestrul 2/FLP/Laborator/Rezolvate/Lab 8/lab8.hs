import Data.List

data Variable = Variable String Int

var :: String -> Variable
var x = Variable x 0

-- 1
fresh :: Variable -> [Variable] -> Variable
fresh v [] = v
fresh (Variable name id) ((Variable name1 id1):t)
   | name == name1 = fresh (Variable name (id1 + 1)) t
   | otherwise = fresh (Variable name id) t

-- 2
instance Show Variable where
   show (Variable name 0) = name
   show (Variable name id) = name ++ "_" ++ (show id)

instance Eq Variable where
   (Variable x1 y1) == (Variable x2 y2) = (x1 == x2) && (y1 == y2)

data Term = V Variable
   | App Term Term
   | Lam Variable Term

v :: String -> Term
v x = V (var x)

lam :: String -> Term -> Term
lam x = Lam (var x)

lams :: [String] -> Term -> Term
lams xs t = foldr lam t xs

($$) :: Term -> Term -> Term 
($$) = App
infixl 9 $$

-- 3
instance Show Term where
   show (V v) = show v
   show (App t1 t2) = "(" ++ (show t1) ++ " " ++ (show t2) ++ ")"
   show (Lam v t) = "(\\" ++ (show v) ++ "." ++ (show t) ++ ")"


-- 4
unique = reverse . nub . reverse

freeVars' :: Term -> [Variable] -> [Variable]
freeVars' (V v) usages
   | not $ elem v usages = [v]
   | otherwise = []
freeVars' (App t1 t2) usages = (freeVars' t1 usages) ++ (freeVars' t2 usages)
freeVars' (Lam v t) usages = freeVars' t (v:usages)

freeVars :: Term -> [Variable]
freeVars t = unique $ freeVars' t []

-- 5
allVars' :: Term -> [Variable]
allVars' (V v) = [v]
allVars' (App t1 t2) = (allVars t1) ++ (allVars t2)
allVars' (Lam v t) = allVars t

allVars :: Term -> [Variable]
allVars t = unique $ allVars' t

-- 6
subst :: Term -> Variable -> Term -> Term -- [u/x]t 
subst u x (Lam y t) -- usor diferita fata de curs
   | x == y = undefined
   | y `notElem` freeVarsU = undefined
   | x `notElem` freeVarsT = undefined
   | otherwise = undefined
   where
      freeVarsT = freeVars t
      freeVarsU = freeVars u
      allFreeVars = nub ([x] ++ freeVarsU ++ freeVarsT)
      y' = fresh y allFreeVars 
      t' = subst (V y') y t

-- 7
aEq :: Term -> Term -> Bool
aEq (V a) (V b) = a == b
aEq (App a1 a2) (App b1 b2) = 
   a1 `aEq` b1 && a2 `aEq` b2
aEq (Lam x1 t1) (Lam x2 t2)
   | x1 == x2 = t1 `aEq` t2
   | otherwise = 
        let new_var = fresh x1 $ nub $ allVars t1 ++ allVars t2 ++ [x1, x2]
            new_t1 = subst (V new_var) x1 t1
            new_t2 = subst (V new_var) x2 t2 in
        new_t1 `aEq` new_t2
aEq _ _ = False

main = undefined
