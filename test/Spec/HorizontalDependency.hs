module Spec.HorizontalDependency where

import Test.Hspec
import Data.Coerce (coerce)
import Spec.TestResult ( TestResult(..) )
import Unsafe.Coerce ( unsafeCoerce )
import Debug.Trace

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
                  in return (Just (coerce $ reverse $ runPR s), spec)

-- someOtherTest takes a typecheck and returns a boolean array
someOthertest :: Monad m => TypecheckResult -> m (Maybe [Bool], Spec)
someOthertest s = let boollist = map (\c -> fromEnum c < 79) (show s) 
                      sumOfList = sum (map fromEnum (show s))
                      spec = it (runTR s ++ " sum of chars even?, return bool array") $ even sumOfList `shouldBe` True
                  in if even sumOfList then return (Just boollist, spec)
                                       else return (Nothing, spec)


-------------------------------------------------------------
----------- Testing out TestResult spectests-----------------

parseTestTR :: Monad m => [TestResult] -> m (Maybe ParseResult, Spec)
parseTestTR [TestResult n] = let n = (unsafeCoerce n :: Int)
                                 spec = it (show n ++ " is parsable as string") $ n < 3 `shouldBe` False
                             in do 
                              trace (show $ "here " ++ show n) (pure())
                              if n < 3 then return (Nothing, spec) 
                                         else return (Just (coerce (show n ++ ".parsed")), spec)
                                         
typechecktestTR :: Monad m => [TestResult] -> m (Maybe TypecheckResult, Spec)
typechecktestTR [TestResult s] = let parse = runPR (unsafeCoerce s) 
                                     spec = it (parse ++ " is typecheckable") $ parse `shouldBe` parse  
                                     in return (Just (coerce $ reverse $ parse), spec)
                
reverseTypechecktestTR :: Monad m => [TestResult] -> m (Maybe TypecheckResult, Spec)
reverseTypechecktestTR [TestResult s] = let tc = runTR (unsafeCoerce s)
                                            spec = it (tc ++ " is reverse typecheckable") $ (tc == "desrap.02") `shouldBe` False
                                        in if tc == "desrap.02" then return (Nothing, spec) 
                                                                else return (Just (coerce (reverse tc)), spec)

multipleHorizDepsTR :: Monad m => [TestResult] -> m (Maybe Bool, Spec)
multipleHorizDepsTR [TestResult r1, TestResult r2] = 
  let tc1 = runTR (unsafeCoerce r1)
      tc2 = runTR (unsafeCoerce r2)
      revEq = tc1 == reverse tc2
      spec = it (tc1 ++ " backwards is " ++ tc2) $ revEq `shouldBe` True
  in if not revEq then return (Nothing, spec)
                               else return (Just revEq, spec)