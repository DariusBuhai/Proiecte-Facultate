{- Monada Maybe este definita in GHC.Base 

instance Monad Maybe where
  return = Just
  Just va  >>= k   = k va
  Nothing >>= _   = Nothing


instance Applicative Maybe where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)       

instance Functor Maybe where              
  fmap f ma = pure f <*> ma   
-}

(<=<) :: (a -> Maybe b) -> (c -> Maybe a) -> c -> Maybe b
f <=< g = (\ x -> g x >>= f)

-- 1.2
asoc :: (Int -> Maybe Int) -> (Int -> Maybe Int) -> (Int -> Maybe Int) -> Int -> Bool
asoc f g h x = (h <=< (g <=< f) $ x) == ((h <=< g) <=< f $ x)

-- 2
pos :: Int -> Bool
pos  x = if (x>=0) then True else False

-- 2.1
foo :: Maybe Int ->  Maybe Bool 
--foo mx = mx  >>= (\x -> Just (pos x))  

-- 2.2
foo mx = do
           x <- mx
           Just (pos x)

-- 3.1
addM :: Maybe Int -> Maybe Int -> Maybe Int  
addM (Just x) (Just y) = Just (x+y)
addM _ _ = Nothing

-- 3.2
addM' :: Maybe Int -> Maybe Int -> Maybe Int
addM' mx my = do 
                x1 <- mx
                x2 <- my
                Just (x1 + x2)
-- 3.3
testAddM = (\x y -> (addM x y) == (addM' x y))

-- 4
-- cartesian_product xs ys = xs >>= ( \x -> (ys >>= \y-> return (x,y)))
cartesian_product xs ys = do
                            x1 <- xs
                            x2 <- ys
                            return (x1, x2)

--prod f xs ys = [f x y | x <- xs, y<-ys]
prod f xs ys = do 
                 x <- xs
                 y <- ys
                 f x y

myGetLine :: IO String
myGetLine = getChar >>= \x ->
         if x == '\n' then
             return []
         else
             myGetLine >>= \xs -> return (x:xs)

myGetLine' = do
               x <- getChar
               if x == '\n' then
                   return []
               else
                   do
                       xs <- myGetLine'
                       return (x:xs)

-- 5
prelNo noin = sqrt noin
ioNumber = do
    noin <- readLn :: IO Float
    putStrLn $ "Intrare\n" ++ (show noin)
    let noout = prelNo noin
    putStrLn $ "Iesire"
    print noout

--ioNumber' = 

main = undefined
