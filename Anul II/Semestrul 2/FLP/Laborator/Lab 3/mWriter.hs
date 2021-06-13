--- Monada Writer

newtype WriterS a = Writer { runWriter :: (a, String) } 


instance  Monad WriterS where
  return va = Writer (va, "")
  ma >>= k = let (va, log1) = runWriter ma
                 (vb, log2) = runWriter (k va)
             in  Writer (vb, log1 ++ log2)


instance  Applicative WriterS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterS where              
  fmap f ma = pure f <*> ma     

tell :: String -> WriterS ()
tell log = Writer ((), log)

logIncrement :: Int -> WriterS Int
logIncrement x = do
    tell ("Increment: " ++ show x ++ "\n")
    return (x + 1)
logIncrementN :: Int -> Int -> WriterS Int
logIncrementN x n = do
    if n == 1 then logIncrement x
    else do
        tell ("Increment: " ++ show x ++ "\n")
        logIncrementN (x + 1) (n - 1)
                  
main = undefined
