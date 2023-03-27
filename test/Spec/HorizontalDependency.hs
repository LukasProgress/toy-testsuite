module Spec.HorizontalDependency where

import Test.Hspec


parseTest :: Monad m => Int -> m (Maybe String, Spec)
parseTest n = let spec = it (show n ++ " is parsable as string") $ n < 3 `shouldBe` False
              in if n < 3 then return (Nothing, spec) 
                           else return (Just (show n ++ ".parsed"), spec)

typechecktest :: Monad m => String -> m (Maybe String, Spec)
typechecktest s = let spec = it (s ++ " is typecheckable") $ s `shouldBe` s
                  in return (Just (reverse s), spec)

-- someOtherTest takes a typecheck and returns a boolean array
someOthertest :: Monad m => String -> m (Maybe [Bool], Spec)
someOthertest s = let boollist = map (\c -> fromEnum c < 79) s 
                      sumOfList = sum (map fromEnum s)
                      spec = it (s ++ " sum of chars even?, return bool array") $ even sumOfList `shouldBe` True
                  in if even sumOfList then return (Just boollist, spec)
                                       else return (Nothing, spec)

