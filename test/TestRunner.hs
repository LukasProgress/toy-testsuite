module Main where

import Control.Monad.Except (forM)
import Control.Monad (foldM)

import Test.Hspec ( hspec, describe, Spec )

import Spec.Tests
import Spec.IOTest
import Data.Maybe (fromJust, isJust)
import Data.List (transpose)

type Description = String


filterDependentTests :: Monad m => [[Maybe a]] -> m [Maybe [a]]
filterDependentTests exs = return (foldr (zipWith (\x xs -> (:) <$> x <*> xs)) (repeat (Just [])) exs)

runner :: Monad m
            => Description
            -> [[Maybe a]]
            -> ([a] -> m (Maybe b, Spec))
            -> m ([Maybe b], Spec)
runner descr exs spectest = do
  dependencyFullfilled <- filterDependentTests exs
  tested <- forM dependencyFullfilled $ \a -> case a of 
                                                Nothing -> return (Nothing, pure ())
                                                Just xs -> spectest xs
  sequenced <- foldM f ([], return ()) tested
  case sequenced of
    (bs, specs) -> return (bs, describe descr specs)
  where f (bs, specsequence) (b, spec) = return (b:bs, spec >> specsequence)



fromTo :: Int -> Int -> [Int]
fromTo x y | x > y = []
           | otherwise = x : fromTo (x + 1) y





main :: IO ()
main = do
    let examples = map Just $ fromTo 1 10
    print examples
    (biggerThan5examples, biggerThan5spec) <- runner "Only examples bigger than 5" [examples] (\[ex] -> Spec.Tests.biggerThan5 ex)

    (evenExamples, evenSpec) <- runner "Only even examples" [examples] (\[ex] -> Spec.Tests.onlyEven ex)

    -- Result of second dependency not used:
    (evenAndBiggerThan5, dependentSpec) <- runner "Dependent on the first two tests: Bigger than 5 and even" [biggerThan5examples, evenExamples] (\[x1, _] -> Spec.Tests.onlyEvenBiggerThan5 x1)

    -- Result of second dependecy used: 
    (xs1, t1) <- runner "Numbers are not odd" [examples] (\[x] -> Spec.Tests.f1_modulo3 x)
    (xs2, t2) <- runner "These Numbers are also not odd" [examples] (\[x] -> Spec.Tests.f2_minus5 x)
    -- Use both dependencies: 
    (productsNotOdd, doubleDependentSpec) <- runner "f (f1 x) (f2 x) not odd" [xs1, xs2] (\[x1, x2] -> Spec.Tests.productNotOdd x1 x2)


    -----------------Monadische Werte-----------------
    
    (_, mv) <- runner "Reading in a file of numbers, are they bigger than these examples?" [examples] (\[x] -> Spec.IOTest.spec x)

    hspec $ do
        -- biggerThan5spec -- 5 failures
        -- evenSpec        -- 5 failures
        dependentSpec      -- 0 failures, dependent on the first two
        doubleDependentSpec
