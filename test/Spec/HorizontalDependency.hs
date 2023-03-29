module Spec.HorizontalDependency where

import Test.Hspec
import Data.Coerce (coerce)

newtype ParseResult = MkPR { runPR :: String }
  deriving (Show,Eq)

newtype TypecheckResult = MkTR {runTR :: String}
  deriving (Show,Eq)

parseTest :: Monad m => Int -> m (Maybe ParseResult, Spec)
parseTest n = let spec = it (show n ++ " is parsable as string") $ n < 3 `shouldBe` False
              in if n < 3 then return (Nothing, spec) 
                           else return (Just (coerce (show n ++ ".parsed")), spec)

typechecktest :: Monad m => ParseResult -> m (Maybe TypecheckResult, Spec)
typechecktest s = let spec = it (runPR s ++ " is typecheckable") $ s `shouldBe` s   -- How to get string back out after coerce?
                  in return (Just (coerce $ reverse $ show s), spec)

-- someOtherTest takes a typecheck and returns a boolean array
someOthertest :: Monad m => TypecheckResult -> m (Maybe [Bool], Spec)
someOthertest s = let boollist = map (\c -> fromEnum c < 79) (show s) 
                      sumOfList = sum (map fromEnum (show s))
                      spec = it (show s ++ " sum of chars even?, return bool array") $ even sumOfList `shouldBe` True
                  in if even sumOfList then return (Just boollist, spec)
                                       else return (Nothing, spec)

