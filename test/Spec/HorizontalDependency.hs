module Spec.HorizontalDependency where

import Test.Hspec

parseTest :: Monad m => Int -> m (Maybe String, Spec)
parseTest n = let spec = it (show n ++ " is parsable as string") $ (n /= 5) `shouldBe` True
              in if n == 5 then return (Nothing, spec) 
                           else return (Just (show n), spec)

typechecktest :: Monad m => String -> m (Maybe String, Spec)
typechecktest s = let spec = it (s ++ " is typecheckable") $ s `shouldBe` s
                  in return (Just (reverse s), spec)