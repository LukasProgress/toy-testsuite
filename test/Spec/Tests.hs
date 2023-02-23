module Spec.Tests where

import Test.Hspec

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