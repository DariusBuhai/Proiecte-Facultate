module LambdaChurch where

import Data.Char (isLetter)
import Data.List ( nub )

class ShowNice a where
    showNice :: a -> String

class ReadNice a where
    readNice :: String -> (a, String)

data Variable
  = Variable
  { name :: String
  , count :: Int
  }
  deriving (Show, Eq, Ord)

var :: String -> Variable
var x = Variable x 0

instance ShowNice Variable where
    showNice (Variable x 0) = x
    showNice (Variable x cnt) = x <> "_" <> show cnt

instance ReadNice Variable where
    readNice s
      | null x = error $ "expected variable but found " <> s
      | otherwise = (var x, s')
      where
        (x, s') = span isLetter s

freshVariable :: Variable -> [Variable] -> Variable
freshVariable var vars = Variable x (cnt + 1)
  where
    x = name var
    varsWithName = filter ((== x) . name) vars
    Variable _ cnt = maximum (var : varsWithName)


data Term
  = V Variable
  | App Term Term
  | Lam Variable Term
  deriving (Show)

-- alpha-equivalence
aEq :: Term -> Term -> Bool
aEq (V x) (V x') = x == x'
aEq (App t1 t2) (App t1' t2') = aEq t1 t1' && aEq t2 t2'
aEq (Lam x t) (Lam x' t')
  | x == x'   = aEq t t'
  | otherwise = aEq (subst (V y) x t) (subst (V y) x' t')
  where
    fvT = freeVars t
    fvT' = freeVars t'
    allFV = nub ([x, x'] ++ fvT ++ fvT')
    y = freshVariable x allFV
aEq _ _ = False

v :: String -> Term
v x = V (var x)

lam :: String -> Term -> Term
lam x = Lam (var x)

lams :: [String] -> Term -> Term
lams xs t = foldr lam t xs

($$) :: Term -> Term -> Term
($$) = App
infixl 9 $$

instance ShowNice Term where
    showNice (V var) = showNice var
    showNice (App t1 t2) = "(" <> showNice t1 <> " " <> showNice t2 <> ")"
    showNice (Lam var t) = "(" <> "\\" <> showNice var <> "." <> showNice t <> ")"

instance ReadNice Term where
    readNice [] = error "Nothing to read"
    readNice ('(' : '\\' : s) = (Lam var t, s'')
      where
        (var, '.' : s') = readNice s
        (t, ')' : s'') = readNice s'
    readNice ('(' : s) = (App t1 t2, s'')
      where
        (t1, ' ' : s') = readNice s
        (t2, ')' : s'') = readNice s'
    readNice s = (V var, s')
      where
        (var, s') = readNice s

freeVars :: Term -> [Variable]
freeVars (V var) = [var]
freeVars (App t1 t2) = nub $ freeVars t1 ++ freeVars t2
freeVars (Lam var t) = filter (/= var) (freeVars t)


-- subst u x t defines [u/x]t, i.e.,  substituting u for x in t
-- for example [3/x](x + x) == 3 + 3
-- This substitution avoids variable captures so it is safe to be used when 
-- reducing terms with free variables (e.g., if evaluating inside lambda abstractions)
subst
    :: Term     -- ^ substitution term
    -> Variable -- ^ variable to be substitutes
    -> Term     -- ^ term in which the substitution occurs
    -> Term
subst u x (V y)
  | x == y    = u
  | otherwise = V y
subst u x (App t1 t2) = App (subst u x t1) (subst u x t2)
subst u x (Lam y t)
  | x == y          = Lam y t
  | y `notElem` fvU = Lam y (subst u x t)
  | x `notElem` fvT = Lam y t
  | otherwise       = Lam y' (subst u x (subst (V y') y t))
  where
    fvT = freeVars t
    fvU = freeVars u
    allFV = nub ([x] ++ fvU ++ fvT)
    y' = freshVariable y allFV

-- Normal order reduction
-- - like call by name
-- - but also reduce under lambda abstractions if no application is possible
-- - guarantees reaching a normal form if it exists
normalReduceStep :: Term -> Maybe Term
normalReduceStep (App (Lam v t) t2) = Just $ subst t2 v t
normalReduceStep (App t1 t2)
  | Just t1' <- normalReduceStep t1 = Just $ App t1' t2
  | Just t2' <- normalReduceStep t2 = Just $ App t1 t2'
normalReduceStep (Lam x t)
  | Just t' <- normalReduceStep t = Just $ Lam x t'
normalReduceStep _ = Nothing

normalReduce :: Term -> Term
normalReduce t
  | Just t' <- normalReduceStep t = normalReduce t'
  | otherwise = t

reduce :: Term -> Term
reduce = normalReduce

-- alpha-beta equivalence (for strongly normalizing terms) is obtained by
-- fully evaluating the terms using beta-reduction, then checking their
-- alpha-equivalence.
abEq :: Term -> Term -> Bool
abEq t1 t2 = aEq (reduce t1) (reduce t2)

evaluate :: String -> String
evaluate s = showNice (reduce t)
  where
    (t, "") = readNice s


-- Church Encodings in Lambda

-- BOOLEANS

-- A boolean is any way to choose between two alternatives (t -> t -> t)


churchTrue :: Term
churchTrue = lams ["t", "f"] (v "t")

churchFalse :: Term
churchFalse = lams ["t", "f"] (v "f")

churchIf :: Term
churchIf = lams ["c", "then", "else"] (v "c" $$ v "then" $$ v "else")

churchNot :: Term
churchNot = lam "b" (v "b" $$ churchFalse $$ churchTrue)

churchAnd :: Term
churchAnd = lams ["b1", "b2"] (v "b1" $$ v "b2" $$ churchFalse)

churchOr :: Term
churchOr = lams ["b1", "b2"] (v "b1" $$ churchTrue $$ v "b2")

church0 :: Term
church0 = lams ["s", "z"] (v "z") -- note that it's the same as churchFalse

church1 :: Term
church1 = lams ["s", "z"] (v "s" $$ v "z")

church2 :: Term
church2 = lams ["s", "z"] (v "s" $$ (v "s" $$ v "z"))

churchS :: Term
churchS = lams ["t","s","z"] (v "s" $$ (v "t" $$ v "s" $$ v "z"))

churchNat :: Integer -> Term
churchNat = undefined

churchPlus :: Term
churchPlus = lams ["n", "m", "s", "z"] (v "n" $$ v "s" $$ (v "m" $$ v "s" $$ v "z"))

churchPlus' :: Term
churchPlus' = lams ["n", "m"] (v "n" $$ churchS $$ v "m")

churchMul :: Term
churchMul = lams ["n", "m", "s"] (v "n" $$ (v "m" $$ v "s"))

churchMul' :: Term
churchMul' = lams ["n", "m"] (v "n" $$ (churchPlus' $$ v "m") $$ church0)

churchPow :: Term
churchPow = lams ["m", "n"] (v "n" $$ v "m")

churchPow' :: Term
churchPow' = lams ["m", "n"] (v "n" $$ (churchMul' $$ v "m") $$ church1)

churchIs0 :: Term
churchIs0 = lam "n" (v "n" $$ (churchAnd $$ churchFalse) $$ churchTrue)

churchS' :: Term
churchS' = lam "n" (v "n" $$ churchS $$ church1)

churchS'Rev0 :: Term
churchS'Rev0 = lams ["s","z"] church0

churchPred :: Term
churchPred =
    lam "n"
        (churchIf
        $$ (churchIs0 $$ v "n")
        $$ church0
        $$ (v "n" $$ churchS' $$ churchS'Rev0))

churchSub :: Term
churchSub = lams ["m", "n"] (v "n" $$ churchPred $$ v "m")

churchLte :: Term
churchLte = lams ["m", "n"] (churchIs0 $$ (churchSub $$ v "m" $$ v "n"))

churchGte :: Term
churchGte = lams ["m", "n"] (churchLte $$ v "n" $$ v "m")

churchLt :: Term
churchLt = lams ["m", "n"] (churchNot $$ (churchGte $$ v "m" $$ v "n"))

churchGt :: Term
churchGt = lams ["m", "n"] (churchLt $$ v "n" $$ v "m")

churchEq :: Term
churchEq = lams ["m", "n"] (churchAnd $$ (churchLte $$ v "m" $$ v "n") $$ (churchLte $$ v "n" $$ v "m"))

churchPair :: Term
churchPair = lams ["f", "s", "action"] (v "action" $$ v "f" $$ v "s")

churchFst :: Term
churchFst = lam "pair" (v "pair" $$ churchTrue)

churchSnd :: Term
churchSnd = lam "pair" (v "pair" $$ churchFalse)

churchPred' :: Term
churchPred' = lam "n" (churchFst $$
    (v "n"
    $$ lam "p" (lam "x" (churchPair $$ v "x" $$ (churchS $$ v "x"))
          $$ (churchSnd $$ v "p"))
    $$ (churchPair $$ church0 $$ church0)
    ))

churchFactorial :: Term
churchFactorial = lam "n" (churchSnd $$
    (v "n"
    $$ lam "p"
        (churchPair
        $$ (churchS $$ (churchFst $$ v "p"))
        $$ (churchMul $$ (churchFst $$ v "p") $$ (churchSnd $$ v "p"))
        )
    $$ (churchPair $$ church1 $$ church1)
    ))

churchFibonacci :: Term
churchFibonacci = lam "n" (churchFst $$
    (v "n"
    $$ lam "p"
        (churchPair
        $$ (churchSnd $$ v "p")
        $$ (churchPlus $$ (churchFst $$ v "p") $$ (churchSnd $$ v "p"))
        )
    $$ (churchPair $$ church0 $$ church1)
    ))

churchDivMod :: Term
churchDivMod =
    lams ["m", "n"]
        (v "m"
        $$ lam "pair"
          (churchIf
          $$ (churchLte $$ v "n" $$ (churchSnd $$ v "pair"))
          $$ (churchPair
             $$ (churchS $$ (churchFst $$ v "pair"))
             $$ (churchSub
                $$ (churchSnd $$ v "pair")
                $$ v "n"
                )
             )
          $$ v "pair"
          )
        $$ (churchPair $$ church0 $$ v "m")
        )

churchNil :: Term
churchNil = lams ["agg", "init"] (v "init")

churchCons :: Term
churchCons = lams ["x","l","agg", "init"]
    (v "agg"
    $$ v "x"
    $$ (v "l" $$ v "agg" $$ v "init")
    )

churchList :: [Term] -> Term
churchList = foldr (\x l -> churchCons $$ x $$ l) churchNil

churchNatList :: [Integer] -> Term
churchNatList = churchList . map churchNat

churchSum :: Term
churchSum = lam "l" (v "l" $$ churchPlus $$ church0)

churchIsNil :: Term
churchIsNil = lam "l" (v "l" $$ lams ["x", "a"] churchFalse $$ churchTrue)

churchHead :: Term
churchHead = lams ["l", "default"] (v "l" $$ lams ["x", "a"] (v "x") $$ v "default")

churchTail :: Term
churchTail = lam "l" (churchFst $$
    (v "l"
    $$ lams ["x","p"] (lam "t" (churchPair $$ v "t" $$ (churchCons $$ v "x" $$ v "t"))
          $$ (churchSnd $$ v "p"))
    $$ (churchPair $$ churchNil $$ churchNil)
    ))

fix :: Term
fix = lam "f" (lam "x" (v "f" $$ (v "x" $$ v "x")) $$ lam "x" (v "f" $$ (v "x" $$ v "x")))

churchDivMod' :: Term
churchDivMod' = lams ["m", "n"]
  (churchIs0 $$ v "n"
  $$ (churchPair $$ church0 $$ v "m")
  $$ (fix
      $$ lams ["f", "p"]
          (lam "x"
              (churchIs0 $$ v "x"
              $$ (churchLte $$ v "n" $$ (churchSnd $$ v "p")
                  $$ (churchPair $$ (churchS $$ (churchFst $$ v "p")) $$ church0)
                  $$ v "p"
                 )
              $$ (v "f" $$ (churchPair $$ (churchS $$ (churchFst $$ v "p")) $$ v "x"))
              )
          $$ (churchSub $$ (churchSnd $$ v "p") $$ v "n")
          )
      $$ (churchPair $$ church0 $$ v "m")
      )
  )

churchSudan :: Term
churchSudan = fix $$ lam "f" (lams ["n", "x", "y"]
    (churchIs0 $$ v "n"
        $$ (churchPlus $$ v "x" $$ v "y")
        $$ (churchIs0 $$ v "y"
            $$ v "x"
            $$ (lam "fnpy"
                (v "f" $$ (churchPred $$ v "n")
                $$ v "fnpy"
                $$ (churchPlus $$ v "fnpy" $$ v "y")
                )
                $$ (v "f" $$ v "n" $$ v "x" $$ (churchPred $$ v "y"))
                )
            )
    ))

churchAckermann :: Term
churchAckermann = fix $$ lam "A" (lams ["m", "n"]
    (churchIs0 $$ v "m"
    $$ (churchS $$ v "n")
    $$ (churchIs0 $$ v "n"
        $$ (v "A" $$ (churchPred $$ v "m") $$ church1)
        $$ (v "A" $$ (churchPred $$ v "m")
            $$ (v "A" $$ v "m" $$ (churchPred $$ v "n")))
        )
    ))
