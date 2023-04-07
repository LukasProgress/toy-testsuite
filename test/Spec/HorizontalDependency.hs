module Spec.HorizontalDependency where

import Test.Hspec
import Data.Coerce (coerce)


-- type testvalue: Use this to wrap tuples for calculating with multiple horizontal deps
newtype TestValue b = TestValue b
  deriving (Eq)

newtype ParseResult = MkPR { runPR :: String }
  deriving (Show,Eq)

newtype TypecheckResult = MkTR {runTR :: String}
  deriving (Show,Eq)

newtype BoolResult = MkBR {runBR :: [Bool]}
  deriving (Show, Eq)



parseTest :: Monad m => Int -> m (Maybe ParseResult, Spec)
parseTest n = let spec = it (show n ++ " is parsable as string") $ n < 3 `shouldBe` False
              in if n < 3 then return (Nothing, spec) 
                           else return (Just (coerce (show n ++ ".parsed")), spec)

typechecktest :: Monad m => ParseResult -> m (Maybe TypecheckResult, Spec)
typechecktest s = let spec = it (runPR s ++ " is typecheckable") $ s `shouldBe` s   -- How to get string back out after coerce?
                  in return (Just (coerce $ reverse $ runPR s), spec)

-- someOtherTest takes a typecheck and returns a boolean array
someOthertest :: Monad m => TypecheckResult -> m (Maybe BoolResult, Spec)
someOthertest s = let boollist = map (\c -> fromEnum c < 79) (show s) 
                      sumOfList = sum (map fromEnum (show s))
                      spec = it (runTR s ++ " sum of chars even?, return bool array") $ even sumOfList `shouldBe` True
                  in if even sumOfList then return (Just $ coerce boollist, spec)
                                       else return (Nothing, spec)


threefoldDependentTest :: Monad m => ParseResult -> TypecheckResult -> BoolResult 
                                  -> m (Maybe Bool, Spec)
threefoldDependentTest pr tr br = let res = or (runBR br) && 
                                                          even (length (runTR tr)) && 
                                                          even (length (runPR pr))
                                      spec = it ("(" ++ runPR pr ++ "," ++ runTR tr ++ "," ++ show (runBR br) ++ ") passes test") $
                                                res `shouldBe` True
                                  in if res then return (Just res, spec)
                                            else return (Nothing, spec)
