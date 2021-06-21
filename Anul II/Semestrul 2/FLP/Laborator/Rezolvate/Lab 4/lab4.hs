--- Monada Identity

newtype Identity a = Identity { runIdentity :: a }

instance Show a => Show (Identity a) where
   show id = show (runIdentity id)

instance Functor Identity where
   fmap f ma = do
      a <- ma
      return (f a)

instance Applicative Identity where
   pure = return
   mf <*> ma = do
       f <- mf
       a <- ma
       return (f a)

instance Monad Identity where
   (Identity a) >>= f = f a
   return a = Identity a

--- Limbajul si  Interpretorul

type M = Identity

showM :: Show a => M a -> String
showM = undefined

type Name = String

data Term = Var Name
          | Con Integer
          | Term :+: Term
          | Lam Name Term
          | App Term Term
  deriving (Show)

pgm :: Term
pgm = App
  (Lam "y"
    (App
      (App
        (Lam "f"
          (Lam "y"
            (App (Var "f") (Var "y"))
          )
        )
        (Lam "x"
          (Var "x" :+: Var "y")
        )
      )
      (Con 3)
    )
  )
  (Con 4)


data Value = Num Integer
           | Fun (Value -> M Value)
           | Wrong

instance Show Value where
 show (Num x) = show x
 show (Fun _) = "<function>"
 show Wrong   = "<wrong>"

type Environment = [(Name, Value)]

--add :: M Value -> M Value
add (Num a) (Num b) = return (Num (a + b))
add _ _ = return Wrong

interp :: Term -> Environment -> M Value
interp (Var n) env = let val = lookup n env in
                     f' val where
                        f' Nothing = return Wrong
                        f' (Just val1) = return val1 
interp (Con i) _ = return (Num i)
interp (t1 :+: t2) env = do 
    v1 <- interp t1 env
    v2 <- interp t2 env
    add v1 v2
--interp (Lam name term) env = Fun f' where
--                                f' :: Value -> M Value
--                                f' x = interp term ((name, x):env)


-- test :: Term -> String
-- test t = showM $ interp t []

pgm1:: Term
pgm1 = App
          (Lam "x" ((Var "x") :+: (Var "x")))
          ((Con 10) :+:  (Con 11))
-- test pgm
-- test pgm1

main = undefined
