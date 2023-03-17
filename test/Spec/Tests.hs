module Spec.Tests where

import Test.Hspec ( it, shouldBe, Spec )

biggerThan5 :: Monad m => Int -> m (Maybe Int, Spec)
biggerThan5 n = let spec = it (show n ++ " is bigger than 5") $ n > 5 `shouldBe` True
                in if n > 5 
                    then return (Just n, spec)
                    else return (Nothing, spec)

onlyEven :: Monad m => Int -> m (Maybe Int, Spec)
onlyEven n = let spec = it (show n ++ " is even") $ even n `shouldBe` True
             in if even n
                then return (Just n, spec)
                else return (Nothing, spec)

onlyEvenBiggerThan5 :: Monad m => Int -> m (Maybe Int, Spec)
onlyEvenBiggerThan5 n = let spec = it (show n ++ " is even and bigger than 5") $ n `div` 2 <= 5 `shouldBe` True
                        in if n `div` 2 <= 5
                            then return (Just n, spec)
                            else return (Nothing, spec)



-- Discussed example: 
f1_modulo3 :: Monad m => Int -> m (Maybe Int, Spec)
f1_modulo3 n = let m = mod n 3 
                   spec = it (show n ++ " mod 3 is not odd") $ odd m `shouldBe` False
               in if odd m
                   then return (Nothing, spec)
                   else return (Just m, spec)
    
f2_minus5 :: Monad m => Int -> m (Maybe Int, Spec)
f2_minus5 n = let m = n - 5
                  spec = it (show n ++ " minus 5 is not odd") $ odd m `shouldBe` False
              in if odd m
                    then return (Nothing, spec)
                    else return (Just m, spec)

productNotOdd :: Monad m => Int -> Int -> m (Maybe Int, Spec)
productNotOdd n m = let o = m * n
                        spec = it (show n ++ "*" ++ show m ++ " is not odd") $ odd o `shouldBe` False
                    in if odd o
                        then return (Nothing, spec)
                        else return (Just o, spec)


------------ Nicht lineare Tests: ----------------

fNonlinearZero :: Int -> Int
fNonlinearZero x
  | x == 0    = 1
  | x < 0     = x
  | otherwise = fNonlinearZero (x-1)

fNonlinearZeroFail :: Int -> Int
fNonlinearZeroFail x
  | x == 0    = 1
  | x == 5    = 0
  | x < 0     = x
  | otherwise = fNonlinearZeroFail (x - 1)

fDependingG :: Int -> Int
fDependingG x = g (2 * x)

g :: Int -> Int
g = id

-- Testergebnis vom selben Test Abhängig
reachesZero :: Monad m => Int -> m (Maybe Int, Spec)
reachesZero n = case fNonlinearZero n of 
                  1 -> return (Just n, it (show n ++ " reaches zero") $ n > 0 `shouldBe` True)
                  x -> return (Nothing, it (show n ++ " is smaller than zero") $ n > 0 `shouldBe` True)

reachesZeroFail :: Monad m => Int -> m (Maybe Int, Spec)
reachesZeroFail n = case fNonlinearZeroFail n of
                        1 -> return (Just n, it (show n ++ " reaches zero") $ n > 0 `shouldBe` True)
                        0 -> return (Nothing, it (show n ++ " was bigger than 4") $ n < 5 `shouldBe` True)
                        x -> return (Nothing, it (show n ++ " is smaller than zero") $ n > 0 `shouldBe` True)

stringTest :: Monad m => String -> m (Maybe String, Spec)
stringTest s = return (Just s, it "It works" $ s `shouldBe` s)