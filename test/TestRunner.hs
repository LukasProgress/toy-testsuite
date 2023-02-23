module Main where

import Control.Monad.Except (forM)
import Control.Monad (foldM)

import Test.Hspec
import Test.Hspec.Runner
import Test.Hspec.Formatters

import Spec.Tests
import Data.Maybe (fromJust, isJust)
import Data.List (transpose)

type Description = String


filterDependentTests :: Monad m => [[Maybe a]] -> m [Maybe a]
filterDependentTests exs = return (map head . filter (all isJust) $ transpose exs)

runner :: Monad m
            => Description
            -> [[Maybe a]]
            -> (a -> m (Maybe b, Spec))
            -> m ([Maybe b], Spec)
runner descr exs spectest = do
  dependencyFullfilled <- filterDependentTests exs
  tested <- forM dependencyFullfilled $ \a -> spectest (fromJust a)
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
    (biggerThan5examples, biggerThan5spec) <- runner "Only examples bigger than 5" [examples] Spec.Tests.biggerThan5

    (evenExamples, evenSpec) <- runner "Only even examples" [examples] Spec.Tests.onlyEven

    (evenAndBiggerThan5, dependentSpec) <- runner "Dependent on the first two tests: Bigger than 5 and even" [biggerThan5examples, evenExamples] Spec.Tests.onlyEvenBiggerThan5

    hspec $ do
        -- biggerThan5spec -- 5 failures
        -- evenSpec        -- 5 failures
        dependentSpec      -- 0 failures, dependent on the first two
